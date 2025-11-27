
library(dplyr)
library(corrplot)
library(fastDummies)
library(polycor)
library(table1)
library(readr)

##Data preparation______________________________________________________________
#load data
load("./denoising_methy/EWAS_pubmep_denoising.RData")
load("./denoising_methy/covars_no_denoising_EWAS.RData")
load("./denoising_methy/experimental_groups.RData")

rm(mVals_BMIQ_prepuber, mVals_BMIQ_puber)

mVals_BMIQ_prepuber = read_rds("./mVals_BMIQ_prepuber_denoised.RDS")
mVals_BMIQ_puber = read_rds("./mVals_BMIQ_puber_denoised.RDS")

print("Data loaded")

#Prepare covariates df
rownames(prepuber_covars_nodenoising) <- prepuber_covars_nodenoising$Code
rownames(puber_covars_nodenoising) <-puber_covars_nodenoising$Code

covars_cross_prepuber <- dummy_cols(covars_cross_prepuber, 
                                    select_columns = c("batch"), 
                                    remove_first_dummy = F, 
                                    remove_selected_columns = F)

covars_cross_puber <- dummy_cols(covars_cross_puber, 
                                 select_columns = c("batch"), 
                                 remove_first_dummy = F,
                                 remove_selected_columns = F)

colnames(prepuber_covars_nodenoising)[2:10] <- c("counts_CD8T", "counts_CD4T", "counts_NK",
                                                 "counts_Bcell", "counts_Mono", "counts_Neu",
                                                 "Sex", "Hospital_center", "Age")

colnames(puber_covars_nodenoising)[2:10] <- c("counts_CD8T", "counts_CD4T", "counts_NK",
                                                 "counts_Bcell", "counts_Mono", "counts_Neu",
                                                 "Sex", "Hospital_center", "Age")

#Set denoise vars
denoise_vars <- colnames(covars_cross_prepuber)[8:12]

#Prepare mvals df
mVals_BMIQ_prepuber <- t(mVals_BMIQ_prepuber)
mVals_BMIQ_puber <- t(mVals_BMIQ_puber)
mVals_BMIQ_prepuber <- mVals_BMIQ_prepuber[, colSums(is.na(mVals_BMIQ_prepuber)) == 0]
mVals_BMIQ_puber <- mVals_BMIQ_puber[, colSums(is.na(mVals_BMIQ_puber)) == 0]

print("Data checks")
#corr_matrix
##prepuber
pca_prepuber <- prcomp(mVals_BMIQ_prepuber, center = F, scale. = F)
var_ratio_prepuber <- (pca_prepuber$sdev^2) / sum(pca_prepuber$sdev^2)
cumvar_prepuber <- cumsum(var_ratio_prepuber)
df_prepuber <- cbind(pca_prepuber$x[,1:10], covars_cross_prepuber[7:ncol(covars_cross_prepuber)])
df_prepuber <- cbind(df_prepuber, prepuber_covars_nodenoising[2:10])

prepuber_cor_matrix<-hetcor(data.frame(droplevels(df_prepuber)))

# pdf("./prepuber_corrplot_denoised.pdf")
corrplot.mixed(prepuber_cor_matrix$correlations,
               upper = "ellipse", 
               lower = "number",
               tl.pos = "lt", 
               tl.col = "black", 
               tl.offset=1, 
               tl.srt = 40, 
               tl.cex = 0.5, 
               number.cex = 0.5)
# dev.off()

##puber
pca_puber <- prcomp(mVals_BMIQ_puber, center = F, scale. = F)
var_ratio_puber <- (pca_puber$sdev^2) / sum(pca_puber$sdev^2)
cumvar_puber <- cumsum(var_ratio_puber)
df_puber <- cbind(pca_puber$x[,1:10], covars_cross_puber[7:ncol(covars_cross_puber)])
df_puber <- cbind(df_puber, puber_covars_nodenoising[2:10])

puber_cor_matrix<-hetcor(data.frame(droplevels(df_puber)))

# pdf("./puber_corrplot.pdf")
corrplot.mixed(puber_cor_matrix$correlations,
               upper = "ellipse", 
               lower = "number",
               tl.pos = "lt", 
               tl.col = "black", 
               tl.offset=1, 
               tl.srt = 40, 
               tl.cex = 0.5,
               number.cex = 0.5)
# dev.off()
