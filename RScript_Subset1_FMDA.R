

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/

# Factor Analysis of Mixed Data Using FactoMineR (video course). https://goo.gl/64gY3R


# Factor analysis of mixed data (FAMD) is a principal component method dedicated to 
# analyze a data set containing both quantitative and qualitative variables (Pages 2004). 

# Pages, J. 2004. "Analyse Factorielle de Donnees Mixtes." Revue Statistique Appliquee 4: 93-111.


library(FactoMineR)
library(factoextra)
#library(missMDA)
library(ggplot2)

#df is a dataframe that contains a subset of your data for which you want to perform FMDA

df <- df1
keep <- complete.cases(df)
keep
dff <- df[keep, ]

#dff <- na.omit(df)

str(df)
str(dff)

# if necessary, impute missing values
#imp <- imputeFAMD(df, ncp = 5, threshold = 1e-01)
#Error in imputeMFA(X = X, group = rep(1, ncol(X)), type = type, ncp = ncp,  : 
#no missing values in X, this function is not useful. Perform MFA on X.

rm(res.famd)
res.famd <- FAMD(dff, ncp = 5, graph = FALSE, sup.var = 1:1, ind.sup = NULL)
# ncp: nb of dimensions kept for the results
# sup.var = supplementary variables, in this case, ProdTypV2 and Group1
# ind.sup = supplementary individuals
# tab.comp is an object from the missMDA package to handle missing values NOT NECESSARY for df1

summary(res.famd)
# Eigenvalues
# Dim.1  Dim.2  Dim.3  Dim.4  Dim.5
# Variance             10.431  7.919  7.351  6.344  6.093
# % of var.             7.963  6.045  5.611  4.843  4.651
# Cumulative % of var.  7.963 14.008 19.619 24.462 29.113

print(res.famd)

eig.val <- get_eigenvalue(res.famd)

head(eig.val)

fviz_screeplot(res.famd, geom="line", ggtheme=theme_bw())
ggsave(filename = "Fig2A_screeplot_df1.jpeg",
       path = "C:/05 Consulting/Zeeh/FMDA_plots/Dec2021", 
       width = 5, height = 4, device='jpeg', dpi=300)

var <- get_famd_var(res.famd)
var


# Coordinates of variables
head(var$coord)
# Cos2: quality of representation on the factore map
head(var$cos2)
# Contributions to the  dimensions
head(var$contrib)

# Plot of variables
fviz_famd_var(res.famd, repel = TRUE, col.var = "black", col.var.sup ="grey40")
ggsave(filename = "Fig2B_VarPlot_df1.jpeg",
       path = "C:/05 Consulting/Zeeh/FMDA_plots/Dec2021", 
       width = 5, height = 4, device='jpeg', dpi=300)


# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)

#To extract the results for quantitative variables
quanti.var <- get_famd_var(res.famd, "quanti.var")
quanti.var


# visualize quantitative variables. Additionally, we'll show how to highlight variables according to 
# either i) their quality of representation on the factor map or ii) their contributions to the dimensions.

fviz_famd_var(res.famd, "quanti.var", xlim=c(-1,1), ylim=c(-1,1),
              repel = TRUE, col.var = "black")
ggsave(filename = "Fig2C_QuantVar_df1.jpeg",
       path = "C:/05 Consulting/Zeeh/FMDA_plots/Dec2021", 
       width = 4, height = 4, device='jpeg', dpi=300)


fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)


# Color by cos2 values: quality on the factor map
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)


#Like quantitative variables, the results for qualitative variables can be extracted as follow:
  
quali.var <- get_famd_var(res.famd, "quali.var")
quali.var 

#visualize qualitative variables
  
fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE
  )


fviz_famd_var(res.famd, "quali.var",
              xlim=c(-3,3), ylim=c(-3,3),
              col.var = "black", repel = TRUE
              #select.var = list(name = NULL, cos2 = NULL, contrib>1)DOES NOT WORK
              )
ggsave(filename = "Fig2D_QualiVar_df1.jpeg",
      path = "C:/05 Consulting/Zeeh/FMDA_plots/Dec2021", 
      width = 5, height = 4, device='jpeg', dpi=300)


#To get the results for individuals
ind <- get_famd_ind(res.famd)
ind
ind$contrib
rownames(dff)

names(res.famd)

#To plot individuals, use the function fviz_mfa_ind() [in factoextra]. 
#By default, individuals are colored in blue. However, like variables, 
#it's also possible to color individuals by their cos2 and contribution values:

#If a variable is well represented by two dimensions, the sum of the cos2 is closed to one.   

fviz_famd_ind(res.famd, col.ind = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE)


# to color the individuals using any of the qualitative variables in the initial data table. 
# To do this, the argument habillage is used in the fviz_famd_ind() function. 
# For example, if you want to color the wines according to the supplementary qualitative variable "Label"

str(dff$ProdTypV2)

dff$ProdTypV2

fviz_mfa_ind(res.famd, 
             habillage = "ProdType", # color by groups 
             #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "#FC4E08"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
)

fviz_ellipses(res.famd, c("ProdType"), repel = TRUE)

# to color individuals using multiple categorical variables at the same time, 
# use the function fviz_ellipses() [in factoextra]
  
#fviz_ellipses(res.famd, c("ProdTypV2","ErgeSan"), repel = TRUE)


#Alternatively, you can specify categorical variable indices:
  
fviz_ellipses(res.famd, c("ProdType"), xlim=c(-4,4), ylim=c(-4,4),
              #palette = c("#00AFBB", "#E7B800", "#FC4E07", "#FC4E08"),
              #palette = "grey",
              palette = c("grey50","grey30","grey10"),
              pointsize = 2,
              geom = "point", 
              #ggtheme = theme_bw(),
              #ggtheme = theme_gray(),
              repel = TRUE)
ggsave(filename = "Fig4A_IndEllipses_df1.jpeg",
      path = "C:/05 Consulting/Zeeh/FMDA_plots/Dec2021", 
      width = 5, height = 4, device='jpeg', dpi=300)

