#Denoising methy
################################################################
#################### Methylation  denoising ####################
################################################################

library(doSNOW) 
library(lme4)
library(fastDummies)

print("Packages loaded")

##Data preparation______________________________________________________________
#load data
load("./denoising_methy/EWAS_pubmep_denoising.RData")
print("Data loaded")
rm(mVals_BMIQ_prepuber)

#Prepare covariates df
covars_cross_prepuber <- dummy_cols(covars_cross_prepuber,
                                    select_columns = c("batch"),
                                    remove_first_dummy = F,
                                    remove_selected_columns = F)

covars_cross_puber <- dummy_cols(covars_cross_puber, 
                                 select_columns = c("batch"), 
                                 remove_first_dummy = F,
                                 remove_selected_columns = F)

#Set denoise vars
denoise_vars <- colnames(covars_cross_prepuber)[8:12]

#Prepare mvals df
mVals_BMIQ_puber <- t(mVals_BMIQ_puber)
col = colnames(mVals_BMIQ_puber)
row = row.names(mVals_BMIQ_puber)

#Comprovations 
print("Data checks")
all(covars_cross_puber$Code==rownames(mVals_BMIQ_puber))
print("Data prepared")

# Set up parallel backend con progress bar
n_cores <- parallel::detectCores() - 10
cl <- makeCluster(n_cores, type = "SOCK")
registerDoSNOW(cl)
print(paste("Using", n_cores, "cores for parallel processing"))

# Create progress bar
num_cols <- length(colnames(mVals_BMIQ_puber))
pb <- txtProgressBar(min = 0, max = num_cols, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

#Puber
print("Denoising puber")
mVals_BMIQ_puber <- foreach(
  x = colnames(mVals_BMIQ_puber), 
  .combine = cbind, 
  .packages = "lme4", 
  .options.snow = opts
  ) %dopar% {
 df <- data.frame(y = mVals_BMIQ_puber[, x], covars_cross_puber)
 formula <- as.formula(paste0("y ~ ", paste0("(1 | ", denoise_vars, ")", collapse = " + ")))
 fit <- lme4::lmer(formula, data = df, REML = FALSE, na.action = na.exclude)
residuals(fit)
                            }

print("Denoising puber DONE")

colnames(mVals_BMIQ_puber) = col
rownames(mVals_BMIQ_puber) = row
# write.xlsx(mVals_BMIQ_puber, "./mVals_BMIQ_puber.xlsx")
# write.csv(mVals_BMIQ_puber, "./mVals_BMIQ_puber.csv", row.names = FALSE)
mVals_BMIQ_puber = as.data.frame(t(mVals_BMIQ_puber))
saveRDS(mVals_BMIQ_puber, "mVals_BMIQ_puber_denoised.RDS")
print("Denoising puber saved")

# Stop cluster
stopCluster(cl)

