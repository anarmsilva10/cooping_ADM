##############################################################
#                 Project nr1: Group 1                      #
#    COPING, PSYCHOSOCIAL FACTORS AND WORK ABILITY        #
# STUDY OF THE BRIEFCOPE SCALE IN HEALTH PROFESSIONALS  #
#######################################################

# Clear variables 
rm(list=ls())
#Get path
path<-getwd(); path
#Set path
setwd(path)

#Installing packages
#Uncomment this lines to install required packages
#packages <- c("foreign", "gtsummary", "caret", "ggplot2", "gridExtra", 
#              "stats", "psych", "factoextra", "FactoMineR", "MVN", 
#             "heplots", "effectsize", "ggcorrplot")
#install.packages(packages, dependencies = TRUE)

#Libraries
library(foreign) 
library(gtsummary) 
library(caret) 
library(ggplot2) 
library(gridExtra)
library(corrplot)
library(stats)
library(psych) 
library(factoextra) 
library(FactoMineR) 
library(MVN) 
library(heplots) 
library(effectsize) 
library(ggcorrplot)

##Importing Data
my.file <- "data2.sav"
data1 <- read.spss(file=my.file)
mydata1 <- as.data.frame(data1)

#######################
# Data Transformation #
#######################
str(mydata1)
levels(mydata1$Grupoprofi) #"Profissional de saúde"  "Não profissional Saúde" "Professores"  
#Data only with Health Care Professionals
mydata_health <- subset(mydata1, mydata1$Grupoprofi=="Profissional de saúde") #909 entries, 117 total columns
##Removing columns that are not included in our studty
#columns with names matching 'csq' followed by digits
mydata_clean <- mydata_health %>% select(-matches("^csq\\d+$"))
#Showing missing values
colSums(is.na(mydata_clean)) #there are 0 missing values
#Number of Questionary as row names
rownames(mydata_clean) <- mydata_clean$Questionário_nº

#################################################
# Data for Dimensions and Scales of Brief COPE #
###############################################
#bcope is going to be my data frame to use for the creation of dimensions and scales of Brief COPE
bcope <- mydata_clean[, -(1:12)]
levels(bcope$bCope1) #"Nunca faço isto" "faço isto por vezes" "Em média é isto que faço" "Faço quase sempre isto" 
#Transforming in a 1 to 4 caracterization
levels_order <- c("Nunca faço isto", 
                  "faço isto por vezes", 
                  "Em média é isto que faço", 
                  "Faço quase sempre isto")

bcope[, paste0("bCope", 1:28)] <- lapply(bcope[, paste0("bCope", 1:28)], function(col) {
  as.numeric(factor(col, levels = levels_order))
})

##################
## Using MEANS ##
#################
#14 Brief COPE scales
bcope_scales_means <- data.frame(
  ICT = bcope$ICT_FINAL_GRUPO,
  Coping_ativo = rowMeans(bcope[, c(3,8)]), 
  Planeamento = rowMeans(bcope[, c(15,26)]),
  Apoio_instrumental = rowMeans(bcope[, c(11,24)]),
  Apoio_emocional = rowMeans(bcope[, c(6,16)]),
  Religiao = rowMeans(bcope[, c(23,28)]),
  Reinterpretacao_positiva = rowMeans(bcope[,c(13,18)]),
  Autoculpabilizacao = rowMeans(bcope[, c(14,27)]),
  Aceitacao = rowMeans(bcope[, c(21,25)]),
  Expressao_Sentimentos = rowMeans(bcope[, c(10,22)]),
  Negacao = rowMeans(bcope[, c(4,9)]),
  Autodistracao = rowMeans(bcope[, c(2,20)]),
  Desinvestimento_comportamental = rowMeans(bcope[, c(7 ,17)]),
  Uso_Substancias = rowMeans(bcope[, c(5,12)]),
  Humor = rowMeans(bcope[, c(19,29)])
)

####################
## Using MEDIANS ##
###################
#14 Brief COPE scales
bcope_scales_median <- data.frame(
  ICT = bcope$ICT_FINAL_GRUPO,
  Coping_ativo = apply(bcope[, c(3,8)],1,median), 
  Planeamento = apply(bcope[, c(15,26)],1,median),
  Apoio_instrumental = apply(bcope[, c(11,24)],1,median),
  Apoio_emocional = apply(bcope[, c(6,16)],1,median),
  Religiao = apply(bcope[, c(23,28)],1,median),
  Reinterpretacao_positiva = apply(bcope[,c(13,18)],1,median),
  Autoculpabilizacao = apply(bcope[, c(14,27)],1,median),
  Aceitacao = apply(bcope[, c(21,25)],1,median),
  Expressao_Sentimentos = apply(bcope[, c(10,22)],1,median),
  Negacao = apply(bcope[, c(4,9)],1,median),
  Autodistracao = apply(bcope[, c(2,20)],1,median),
  Desinvestimento_comportamental = apply(bcope[, c(7 ,17)],1,median),
  Uso_Substancias = apply(bcope[, c(5,12)],1,median),
  Humor = apply(bcope[, c(19,29)],1,median)
)

################
## Using SUM ## 
###############
#14 Brief COPE scales
bcope_scales_sums <- data.frame(
  ICT = bcope$ICT_FINAL_GRUPO,
  Coping_ativo = rowSums(bcope[, c(3,8)]), 
  Planeamento = rowSums(bcope[, c(15,26)]),
  Apoio_instrumental = rowSums(bcope[, c(11,24)]),
  Apoio_emocional = rowSums(bcope[, c(6,16)]),
  Religiao = rowSums(bcope[, c(23,28)]),
  Reinterpretacao_positiva = rowSums(bcope[,c(13,18)]),
  Autoculpabilizacao = rowSums(bcope[, c(14,27)]),
  Aceitacao = rowSums(bcope[, c(21,25)]),
  Expressao_Sentimentos = rowSums(bcope[, c(10,22)]),
  Negacao = rowSums(bcope[, c(4,9)]),
  Autodistracao = rowSums(bcope[, c(2,20)]),
  Desinvestimento_comportamental = rowSums(bcope[, c(7 ,17)]),
  Uso_Substancias = rowSums(bcope[, c(5,12)]),
  Humor = rowSums(bcope[, c(19,29)])
)

######################################
# Sociodemographic Characterization #
####################################
gtsummary::tbl_summary(mydata_clean)

#Data that it's not needed
rm(data1, mydata_health, my.file, levels_order, mydata1, bcope_scales_means, bcope_scales_median, bcope, path) 

#Since we are working on variables described on a four-position Likert scale, it would be more appropriate 
#to work on the 14 scales created using the sum. 

########################
# Test and Train Data #
######################
bcope_scales_sums$sexo <- mydata_clean$sexo
bcope_scales_sums$literacia <- mydata_clean$hliterárias
bcope_scales_sums$descricao <- mydata_clean$Descricao
bcope_scales_sums$idade_cat <- mydata_clean$IdadeCat
bcope_scales_sums$anos_trabalho_cat <- mydata_clean$Anos_trabalhoCat
bcope_scales_sums$idade <- mydata_clean$Idade
bcope_scales_sums$anos_trabalho <- mydata_clean$Anos_trabalho

#Seeing how our ICT variable is distributed to have a insight about the number of clusters
table(bcope_scales_sums$ICT) # Pobre (12) Moderada (160) Boa (481) Excelente (256)
#As the frequency of poor people is very low, we decided to combine poor and moderate for the CA
bcope_scales_sums$ICT_cat <- factor(
  ifelse(bcope_scales_sums$ICT == "Pobre", "Pobre/Moderada",
         ifelse(bcope_scales_sums$ICT == "Moderada", "Pobre/Moderada",
                ifelse(bcope_scales_sums$ICT == "Boa", "Boa", 
                       ifelse(bcope_scales_sums$ICT == "Excelente", "Excelente", NA)))),
  levels = c("Pobre/Moderada", "Boa", "Excelente"))
table(bcope_scales_sums$ICT_cat) # Pobre/Moderada (172) Boa (481) Excelente (356)

##Transforming in a 1 to 3 caracterization
bcope_scales_sums$ICT <- factor(
  ifelse(bcope_scales_sums$ICT_cat == "Pobre/Moderada", "1",
         ifelse(bcope_scales_sums$ICT_cat == "Boa", "2", 
                ifelse(bcope_scales_sums$ICT_cat == "Excelente", "3", NA))),
  levels = c("1", "2", "3"))
table(bcope_scales_sums$ICT) # 1 (172) 2 (481) 3 (256)

#Outliers detection
boxplot(bcope_scales_sums[, 2:15], las = 2, ces.axis = 0.8)
#Distance mahalanobis (uses a covariance matrix)
dist1.Mah <- mahalanobis(bcope_scales_sums[, 2:15], center=colMeans(bcope_scales_sums[, 2:15]), cov=cov(bcope_scales_sums[, 2:15]))
# Cutoff value for distances from Chi-Square Dist.
cutoff1 <- qchisq(p = 0.99 , df = ncol(bcope_scales_sums[, 2:15]))
## Display observation whose distance greater than cutoff value
clean_data <- bcope_scales_sums[dist1.Mah <= cutoff1,]
boxplot(clean_data[, 2:15], las = 2, ces.axis = 0.8) 
#We removed 50 observations as outliers. Our database has know 859 obs.

#Setting a seed to have the same results
set.seed(123)
#Data for our test and train data
tr <- createDataPartition(clean_data$ICT, p = .7, list = FALSE, times = 1)
#Train data
tr_data <- clean_data[ tr,] #602 entries, 20 total columns
#Test data
te_data  <- clean_data[-tr,] #257 entries, 20 total columns

custom_palette <- c("#313B5F", "#1F77B4", "#4682B4", "#87CEFA", "#B0C4DE")
#################################
# Principal Component Analysis #
###############################
#The correlation matrix
pca_cor <- cor(tr_data[,2:15], method = "pearson")
pca_cor

#Assumptions
#The Bartlett test
#H0: Correlation matrix equals identity matrix
#H1: Correlation matrix is different from the identity matrix
cortest.bartlett(pca_cor,n=nrow(tr_data)) 
#X2(91) = 1693.33; p-value = 2.262447e-293; p-value is lesser than 0.05, so we can reject H0.
#The KMO score
KMO(pca_cor) #KMO=0.74; This is a good KMO, because it's higher than 0.70.
#The determinant 
det(pca_cor) #det=0.05821898; It's higher than  0.

#PCA calculation by function PCA()
##################################
#Trying with ncp = number of columns in the dataset
model.PCA <- PCA(tr_data[,2:15], ncp = ncol(tr_data[,2:15]), graph = FALSE, scale.unit = TRUE)
model.PCA
summary(model.PCA)

#Eigenvalues / Variances
eig.val <- get_eigenvalue(model.PCA) #Want the ones that are higher than 1 
val <- as.data.frame(eig.val)
PCA_ncp <- sum(val['eigenvalue'] > 1) #we have 4 PC with eigenvalue higher than 1
#cumulative variance explained: 54.553%

#the PCA() function always calculates for the total number of variables available. Thus, 
#we have to focus our results on those with an eigenvalue greater than 1, which in this case are
#the first 4

####################
## Vizualization ##
###################
#Contributions to the principal components
PC_combinations <- combn(1:PCA_ncp, 2, simplify = FALSE)
#Creating Biplots for each combination
plots <- list()
for (i in seq_along(PC_combinations)) {
  PC_pair <- PC_combinations[[i]]  
  
  biplt <- fviz_pca_var(
    model.PCA,
    axes = PC_pair,        
    col.var = "contrib",   
    gradient.cols = custom_palette,
    repel = TRUE
  ) +
    labs(title = paste("Biplot - Contribuição de Variávies:", 
                       paste("CP", PC_pair[1], "-", "CP", PC_pair[2], sep = "")),
         x = paste("CP", PC_pair[1], sep = ""),
         y = paste("CP", PC_pair[2], sep = "")) +
    theme_void()
  
  #biplot_pca <- paste0("Biplot_CP", PC_pair[1], "_CP", PC_pair[2], ".png")
  #ggsave(biplot_pca, plot = biplt, width = 8, height = 6)
  
  plots[[i]] <- biplt
}
#Display biplots
grid.arrange(grobs = plots, ncol = 3)

#Quality of representation of variables for each PC
corrplot(model.PCA$var$cos2[,1:PCA_ncp], 
         is.corr=FALSE, 
         col=RColorBrewer::brewer.pal(9, "Blues"),
         tl.col='black')

# Create an empty list to store plots
contrib_plots <- list()

# Loop through each PC and generate the contribution plot
for (i in 1:PCA_ncp) {
  contrib_plots[[i]] <- fviz_contrib(model.PCA, choice = "var", fill = "#313B5F",
                                     color = "black", axes = i, top = 14) +
    labs(title = paste("Contribuição das Variáveis na CP",i),
         x = paste("Variáveis"),
         y = paste("Contribuições (%)")) +
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
          axis.title.x = element_text(margin = margin(t = 10)),
          plot.margin = margin(10, 10, 10, 10))
}

# Display all plots in a grid
grid.arrange(grobs = contrib_plots, ncol = 2)

###########################
# Supplementary elements #
#########################
#Suplemmentary data
suplementary_data <- tr_data
suplementary_data <- suplementary_data[,-c(18,19,20,23)]

model2.PCA <- PCA(suplementary_data, 
                  quanti.sup = c(18,19), quali.sup = c(1,16,17), graph=FALSE)
summary(model2.PCA)
#quanti.sup -> quantitative variables -> idade (18) , anos_de_trabalho (19)
#quali.sup -> qualitative/categorical variables -> sexo (16), literacia (17), ICT (1) 

eig.val.sup <- get_eigenvalue(model2.PCA) #Want the ones that are higher than 1 
val.sup <- as.data.frame(eig.val.sup)
PCA_ncp_sup <- sum(val['eigenvalue'] > 1) 
####################
## Visualization ##
###################
#Showing active variables and supplementary quantitative variables
PC_combinations <- combn(1:PCA_ncp_sup, 2, simplify = FALSE)
#Creating Biplots for each combination
plots <- list()
for (i in seq_along(PC_combinations)) {
  PC_pair <- PC_combinations[[i]]  
  
  biplt <- fviz_pca_var(
    model2.PCA,
    axes = PC_pair,        
    col.var = "contrib",   
    gradient.cols = custom_palette,
    repel = TRUE
  ) +
    labs(title = paste("Biplot - Contribuição de Variávies Ativas e Suplementares:", 
                       paste("CP", PC_pair[1], "-", "CP", PC_pair[2], sep = "")),
         x = paste("CP", PC_pair[1], sep = ""),
         y = paste("CP", PC_pair[2], sep = "")) +
    theme_void()
  
  #biplot_pca <- paste0("Biplot_sup_CP", PC_pair[1], "_CP", PC_pair[2], ".png")
  #ggsave(biplot_pca, plot = biplt, width = 8, height = 6)
  
  plots[[i]] <- biplt
}
#Display biplots
grid.arrange(grobs = plots, ncol = 3)

## PCA Data ##
#Further in the analysis, we are going to use the data from PCA. 
#That way, we decide to create a data frame with this data.

#PCA data for further use
PCA_train <- model.PCA$ind$coord[, 1:PCA_ncp]
PCA_train <- as.data.frame(PCA_train)
colnames(PCA_train) <- paste0("PC", 1:PCA_ncp)
PCA_train$ICT <- as.numeric(as.character(tr_data$ICT))

#Data from PC1 to PC4 according my test data, using predict
PCA_test <- predict(model.PCA, newdata = te_data[, 2:15])$coord[, 1:PCA_ncp]
PCA_test <- as.data.frame(PCA_test)
colnames(PCA_test) <- paste0("PC", 1:PCA_ncp)
PCA_test$ICT <- as.numeric(as.character(te_data$ICT))

#Implementing linear regression
model_lm <- lm(ICT ~ PC1 + PC2 + PC3 + PC4, data = PCA_train)
summary(model_lm)
predictions <- predict(model_lm, newdata = PCA_test)

actual_values <- PCA_test$ICT
mse <- mean((predictions - actual_values)^2) #0.4093
rmse <- sqrt(mse) #0.6398
cat("MSE:", mse, "\nRMSE:", rmse, "\n")

#Removing unnecessary data
rm(biplt, eig.val, model.PCA, PC_combinations, plots, val, i, PC_pair, dist1.Mah, 
   predictions, suplementary_data, tr, PCA_ncp, model2.PCA, pca_cor, contrib_plots, 
   cutoff1, biplot_pca, actual_values, eig.val.sup, model_lm, mse, rmse, val.sup, PCA_ncp_sup)

#####################
# Cluster Analysis # 
###################
#To perform Cluster Analysis, we are going to use a 2-step approach:
#First: we are going to extract the optimal number of clusters with Hierarchical 
#Clustering method
#Second: we are going to perform clusters with Non Hierarchical Clustering method

#Scaled Data
scaled_data <- scale(tr_data[,2:15])  
scaled_data <- as.data.frame(scaled_data)
#Adding ICT to our data frame
scaled_data$ICT <- tr_data$ICT
str(scaled_data)

#####################################
## Hierarchical Clustering method ##
####################################
#Calculate distance matrix with euclidean distance
eucledian <- dist(scaled_data[, 1:14], method="euclidean")
#Ward
model1.WardD2 <- hclust(eucledian, method = "ward.D2")
#Dendrogram
#png("dendrogram_plot.png", width = 800, height = 600)
plot(model1.WardD2, main = "Dendrograma", hang = 0.1, ylab = 
       "Peso", xlab = "Indivíduos", sub = "")
#Cluster membership
#2 clusters
model1.2 = cutree(model1.WardD2, 2)
table(model1.2) #1 (427) 2 (165)
rect.hclust(model1.WardD2, k=2, border="#1F77B4")
#3 clusters
model1.3 = cutree(model1.WardD2,3) 
table(model1.3) #1 (161) 2(165) 3(266)
rect.hclust(model1.WardD2, k=3, border="#313B5F")
#Taking into account the results obtained by the dendograms, we can't decide if 
#we should use 2 or 3 clusters. Therefore, we will implement both.
#dev.off()

#########################################
## Non Hierarchical Clustering method ##
########################################
#K-Means
#2 clusters
model1.k2 <- kmeans(scaled_data[, 1:14], centers = 2, nstart = 20)
#png("cluster_k2_plot.png", width = 800, height = 600)
fviz_cluster(model1.k2, data = scaled_data[, 1:14], 
             ellipse.type = "convex", 
             palette = custom_palette, 
             ggtheme = theme_minimal(),
             labelsize = 0)
#dev.off()
#3 clusters
model1.k3 <- kmeans(scaled_data[, 1:14], centers = 3, nstart = 20)
#png("cluster_k3_plot.png", width = 800, height = 600)
fviz_cluster(model1.k3, data = scaled_data[, 1:14], 
             ellipse.type = "convex", 
             palette = custom_palette, 
             ggtheme = theme_minimal(),
             labelsize = 0)
#dev.off()

#Scaled Test Data
scaled_te_data <- scale(te_data[, 2:15])
scaled_te_data <- as.data.frame(scaled_te_data)
scaled_te_data$ICT <- te_data$ICT
scaled_te_data$ICT_cat <- te_data$ICT_cat
scaled_te_data$sexo <- te_data$sexo
scaled_te_data$idade <- te_data$idade
scaled_te_data$literacia <- te_data$literacia

#Applying cluster model with k=3 to test data to try to see if matches with ICT
#Calculate the closest cluster for each row of data
test_clusters_k3 <- apply(scaled_te_data[, 1:14], 1, function(row) {
  which.min(colSums((t(model1.k3$centers) - row)^2))
})

#CA data
CA_data <- scaled_te_data
CA_data$Cluster_k3 <- as.factor(test_clusters_k3)

#Create confusion matrix
table(CA_data$ICT_cat, CA_data$Cluster_k3)

#Data for further use
PCA_train$cluster_k2 <- as.factor(model1.k2$cluster)
PCA_train$cluster_k3 <- as.factor(model1.k3$cluster)

#Data that is not needed
rm(eucledian, model1.2, model1.3, model1.k2, model1.k3, model1.WardD2, CA_data,
   scaled_data, scaled_te_data, test_clusters_k3)

###########################
# Inferential Statistics # 
#########################
manova_pca <- PCA_train
manova_pca$sexo <- tr_data$sexo
manova_pca$ICT_cat <- tr_data$ICT_cat
manova_pca$idade_cat <- tr_data$idade_cat
manova_pca$anos_trabalho <- tr_data$anos_trabalho_cat
str(manova_pca) #ours sociodemographic variables are all set as factor.

table(manova_pca$idade_cat)
#As "[55-65]", "[65-90]" have few observations, we decide to make a recodification
manova_pca$idade_cat <- forcats::fct_collapse(manova_pca$idade_cat, '[45-90]' = 
                                                c("[45-55]", "[55-65]", "[65-90]"))
table(manova_pca$idade_cat)

table(manova_pca$anos_trabalho)
manova_pca$anos_trabalho <- forcats::fct_collapse(manova_pca$anos_trabalho, 
                                                  ']10-20]' = c("]10;15]", "]15;20]"),
                                                  '>20' = c("]20;25]", "]25;30]", ">30"))
table(manova_pca$anos_trabalho)

attach(manova_pca)
#####################
## ONE-WAY MANOVA ##
###################
cor_man_pca <- cor(manova_pca[,1:4], method = "pearson")
#p Values for correlations in the matrix can be obtained using the corr.p() function
print(corr.p(cor_man_pca,n=nrow(manova_pca))) 

#Assumptions
#Multivariate Normality
#H0: The variables follow a multivariate normal distribution.
#H1: The variables do not follow a multivariate normal distribution.
mvn(manova_pca[, 1:4], mvn_test = "mardia", univariate_test = "SW")
#p-value ~ 0; p-value is lesser than 0.05, so we reject H0. 
#Therefore, the variables do not follow a multivariate normal distribution.

#Dependent variables 
y <- cbind(PC1, PC2, PC3, PC4)

##########
## ICT ##
#########
#Box-M test for the variance-covariace groups matrix
#H0:The Covariance matrices are homogeneous
#H1:The Covariance matrices are not homogeneous
boxM(y ~ ICT_cat, data=manova_pca)
#X2(20) = 44.744; p-value = 0.001195 ; p-value is less than 0.05, so we can reject H0.
#Therefore, The Covariance matrices are not homogeneous.

#As we are only testing one factor, the choice of functions is random, since Sum squares of type I is equal to 
#Sum squares of type III
#Therefore, using manova() function, we are working with Sum squares of type I
model1_pca <- manova(y ~ ICT_cat, data = manova_pca)
#Effect size interpretation
interpret_eta_squared(eta_squared(model1_pca), rules="cohen1992") #small

#H0: (μ = μi) i = 1,2,3
#H1: ⱻi μai ≠ μ i = 1,2,3 (There is at least one group whose average is different from the others)
#Pillai's Trace (more robust to violations of assumptions)
summary(model1_pca, test = "Pillai")
#F(8;1266)=9.40; p-value = 1.122e-12; p-value is less than 0.05, so we reject H0.

####################
## ANOS_TRABALHO ##
###################
#Assumptions
#Box-M test for the variance-covariace groups matrix
boxM(y ~ anos_trabalho, data=manova_pca)
#X2(30)=30.956; p-value = 0.4176; p-value is greater than 0.05, so we can't reject H0.
#Therefore, The Covariance matrices are homogeneous.

model2_pca <- manova(y ~ anos_trabalho, data = manova_pca)
#Effect size interpretation
interpret_eta_squared(eta_squared(model2_pca), rules="cohen1992") #small

#Pillai's Trace (more robust to violations of assumptions)
summary(model2_pca, test = "Pillai")
#F(12;1791)=3.192; p-value = 0.0001533; p-value is less than 0.05, so we reject H0.

#############
### SEXO ###
############
#Assumptions
#Box-M test for the variance-covariace groups matrix
boxM(y ~ sexo, data=manova_pca)
#X2(10) = 28.108; p-value = 0.001735; p-value is less than 0.05, so we reject H0.
#Therefore, The Covariance matrices are not homogeneous.

model3_pca <- manova(y ~ sexo, data = manova_pca)
#Effect size interpretation
interpret_eta_squared(eta_squared(model3_pca), rules="cohen1992") #small

#Pillai's Trace (more robust to violations of assumptions)
summary(model3_pca, test = "Pillai")
#F(4;597)=16.093; p-value = 1.598e-12; p-value is less than 0.05, so we reject H0.

##############
### IDADE ###
#############
#Assumptions
#Box-M test for the variance-covariace groups matrix
boxM(y ~ idade_cat, data=manova_pca)
#X2(30) = 37.98; p-value = 0.1503; p-value is greater than 0.05, so we can´t reject H0.
#Therefore, The Covariance matrices are homogeneous.

model4_pca <- manova(y ~ idade_cat, data = manova_pca)
#Effect size interpretation
interpret_eta_squared(eta_squared(model4_pca), rules="cohen1992") #small

#Pillai's Trace (more robust to violations of assumptions)
summary(model4_pca, test = "Pillai")
#F(12;1791)=3.7869; p-value = 1.03e-05; p-value is less than 0.05, so we reject H0.

#################
### Clusters ###
################
#Assumptions
#Box-M test for the variance-covariace groups matrix
boxM(y ~ cluster_k3, data=manova_pca)
#X2(20) = 140.27; p-value < 2.2e-16; p-value is less than 0.05, so we reject H0.
#Therefore, The Covariance matrices are not homogeneous.

model5_pca <- manova(y ~ cluster_k3, data = manova_pca)
#Effect size interpretation
interpret_eta_squared(eta_squared(model5_pca), rules="cohen1992") #large

#Pillai's Trace (more robust to violations of assumptions)
summary(model5_pca, test = "Pillai")
#F(8;1194)=186.57; p-value < 2.2e-16; p-value is less than 0.05, so we reject H0.

#Data not needed
rm(model1_pca,model2_pca, model3_pca, model4_pca, model5_pca, y, cor_man_pca)
detach(manova_pca)

#############################
# ANÁLISE DE SENSIBILIDADE #
############################
data <- clean_data
table(data$descricao)
#Dropping levels with 0 observations
data$descricao <- forcats::fct_drop(data$descricao)
data$descricao <- forcats::fct_collapse(data$descricao, 'Técnicos' = 
                                          c("Tecn. cardiopneumologia", "Tecn. Medicina nuclear",
                                            "Tecn. Radiologia", "Tecn. Radioterapia"))

##################
## Enfermeiros ##
#################
data_nurses <- subset(data, data$descricao=="Enfermeiro")

#################################
# Principal Component Analysis #
###############################
#The correlation matrix
pca_cor <- cor(data_nurses[,2:15], method = "pearson")
pca_cor

#Assumptions
#The Bartlett test
cortest.bartlett(pca_cor,n=nrow(data_nurses)) 
#X2(91) = 256.6353; p-value = 1.055351e-17; p-value is less than 0.05, so we can reject H0.
#The KMO score
KMO(pca_cor) #KMO=0.65; This is a median KMO, because it's lower than 0.70.
#The determinant 
det(pca_cor) #det=0.07978443; It's higher than  0.


model.PCA <- PCA(data_nurses[,2:15], ncp = ncol(data_nurses[,2:15]), graph = FALSE, scale.unit = TRUE)
summary(model.PCA)

#Eigenvalues / Variances
eig.val <- get_eigenvalue(model.PCA) #Want the ones that are higher than 1 
val = as.data.frame(eig.val)
PCA_ncp <- sum(val['eigenvalue'] > 1) #we have 4 PC with eigenvalue higher than 1

#Contributions to the principal components
PC_combinations <- combn(1:PCA_ncp, 2, simplify = FALSE)
#Creating Biplots for each combination
plots <- list()
for (i in seq_along(PC_combinations)) {
  PC_pair <- PC_combinations[[i]]  
  
  biplt <- fviz_pca_var(
    model.PCA,
    axes = PC_pair,        
    col.var = "contrib",   
    gradient.cols = custom_palette,
    repel = TRUE
  ) +
    labs(title = paste("Biplot - Contribuição de Variávies:", 
                       paste("CP", PC_pair[1], "-", "CP", PC_pair[2], sep = "")),
         x = paste("CP", PC_pair[1], sep = ""),
         y = paste("CP", PC_pair[2], sep = "")) +
    theme_void()
  
  #biplot_pca <- paste0("Biplot_enf_CP", PC_pair[1], "_CP", PC_pair[2], ".png")
  #ggsave(biplot_pca, plot = biplt, width = 8, height = 6)
  
  plots[[i]] <- biplt
}
#Display biplots
grid.arrange(grobs = plots, ncol = 3)

#Data that it's not needed
rm(biplt, eig.val, model.PCA, PC_combinations, plots, val, i, PC_pair, 
   PCA_ncp, pca_cor)

##############
## Médicos ##
#############
data_doc <- subset(data, data$descricao=="Médico")

#################################
# Principal Component Analysis #
###############################
#The correlation matrix
pca_cor <- cor(data_doc[,2:15], method = "pearson")
pca_cor

#Assumptions
#The Bartlett test
cortest.bartlett(pca_cor,n=nrow(data_doc)) 
#X2(91) = 196.4885; p-value = 9.919068e-10; p-value is less than 0.05, so we can reject H0.
#The KMO score
KMO(pca_cor) #KMO=0.64; This is a median KMO, because it's lower than 0.70.
#The determinant 
det(pca_cor) #det=0.01461717; It's higher than  0.


model.PCA <- PCA(data_doc[,2:15], ncp = ncol(data_doc[,2:15]), graph = FALSE, scale.unit = TRUE)
summary(model.PCA)

#Eigenvalues / Variances
eig.val <- get_eigenvalue(model.PCA) #Want the ones that are higher than 1 
val = as.data.frame(eig.val)
PCA_ncp <- sum(val['eigenvalue'] > 1) #we have 5 PC with eigenvalue higher than 1

#Contributions to the principal components
PC_combinations <- combn(1:PCA_ncp, 2, simplify = FALSE)
#Creating Biplots for each combination
plots <- list()
for (i in seq_along(PC_combinations)) {
  PC_pair <- PC_combinations[[i]]  
  
  biplt <- fviz_pca_var(
    model.PCA,
    axes = PC_pair,        
    col.var = "contrib",   
    gradient.cols = custom_palette,
    repel = TRUE
  ) +
    labs(title = paste("Biplot - Contribuição de Variávies:", 
                       paste("CP", PC_pair[1], "-", "CP", PC_pair[2], sep = "")),
         x = paste("CP", PC_pair[1], sep = ""),
         y = paste("CP", PC_pair[2], sep = "")) +
    theme_void()
  
  #biplot_pca <- paste0("Biplot_medico_CP", PC_pair[1], "_CP", PC_pair[2], ".png")
  #ggsave(biplot_pca, plot = biplt, width = 8, height = 6)
  
  plots[[i]] <- biplt
}
#Display biplots
grid.arrange(grobs = plots, ncol = 3)

#Data that it's not needed
rm(biplt, eig.val, model.PCA, PC_combinations, plots, val, i, PC_pair, 
   PCA_ncp, pca_cor)

###############
## Técnicos ##
##############
data_tec <- subset(data, data$descricao=="Técnicos")

#################################
# Principal Component Analysis #
###############################
#The correlation matrix
pca_cor <- cor(data_tec[,2:15], method = "pearson")
pca_cor

#Assumptions
#The Bartlett test
cortest.bartlett(pca_cor,n=nrow(data_tec)) 
#X2(91) = 321.969; p-value = 1.500863e-27; p-value is less than 0.05, so we can reject H0.
#The KMO score
KMO(pca_cor) #KMO=0.67; This is a median KMO, because it's lower than 0.70.
#The determinant 
det(pca_cor) #det=0.03680042; It's higher than  0.


model.PCA <- PCA(data_tec[,2:15], ncp = ncol(data_tec[,2:15]), graph = FALSE, scale.unit = TRUE)
summary(model.PCA)

#Eigenvalues / Variances
eig.val <- get_eigenvalue(model.PCA) #Want the ones that are higher than 1 
val = as.data.frame(eig.val)
PCA_ncp <- sum(val['eigenvalue'] > 1) #we have 4 PC with eigenvalue higher than 1

#Contributions to the principal components
PC_combinations <- combn(1:PCA_ncp, 2, simplify = FALSE)
#Creating Biplots for each combination
plots <- list()
for (i in seq_along(PC_combinations)) {
  PC_pair <- PC_combinations[[i]]  
  
  biplt <- fviz_pca_var(
    model.PCA,
    axes = PC_pair,        
    col.var = "contrib",   
    gradient.cols = custom_palette,
    repel = TRUE
  ) +
    labs(title = paste("Biplot - Contribuição de Variávies:", 
                       paste("CP", PC_pair[1], "-", "CP", PC_pair[2], sep = "")),
         x = paste("CP", PC_pair[1], sep = ""),
         y = paste("CP", PC_pair[2], sep = "")) +
    theme_void()
  
  #biplot_pca <- paste0("Biplot_tec_CP", PC_pair[1], "_CP", PC_pair[2], ".png")
  #ggsave(biplot_pca, plot = biplt, width = 8, height = 6)
  
  plots[[i]] <- biplt
}
#Display biplots
grid.arrange(grobs = plots, ncol = 3)

#Data that it's not needed
rm(biplt, eig.val, model.PCA, PC_combinations, plots, val, i, PC_pair, 
   PCA_ncp, pca_cor)
