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
rm(mVals_BMIQ_puber)

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
mVals_BMIQ_prepuber <- t(mVals_BMIQ_prepuber)
col = colnames(mVals_BMIQ_prepuber)
row = row.names(mVals_BMIQ_prepuber)

#Comprovations 
print("Data checks")
all(covars_cross_prepuber$Code==rownames(mVals_BMIQ_prepuber))
print("Data prepared")

# Set up parallel backend con progress bar
n_cores <- parallel::detectCores() - 10
cl <- makeCluster(n_cores, type = "SOCK")
registerDoSNOW(cl)
print(paste("Using", n_cores, "cores for parallel processing"))

# Create progress bar
num_cols <- length(colnames(mVals_BMIQ_prepuber))
pb <- txtProgressBar(min = 0, max = num_cols, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Denoising prepuber con progress bar
print("Denoising prepuber")
mVals_BMIQ_prepuber <- foreach(
  x = colnames(mVals_BMIQ_prepuber),
  .combine = cbind,
  .packages = "lme4",
  .options.snow = opts
) %dopar% {
  df <- data.frame(y = mVals_BMIQ_prepuber[, x], covars_cross_prepuber)
  formula <- as.formula(paste0("y ~ ", paste0("(1 | ", denoise_vars, ")", collapse = " + ")))
  fit <- lme4::lmer(formula, data = df, REML = FALSE, na.action = na.exclude)
  residuals(fit)
}

close(pb)
print("Denoising prepuber DONE")



colnames(mVals_BMIQ_prepuber) = col
rownames(mVals_BMIQ_prepuber) = row
# write.xlsx(mVals_BMIQ_prepuber, "./mVals_BMIQ_prepuber.xlsx")
# write.csv(mVals_BMIQ_prepuber, "./mVals_BMIQ_prepuber.csv", row.names = FALSE)
mVals_BMIQ_prepuber = as.data.frame(t(mVals_BMIQ_prepuber))
saveRDS(mVals_BMIQ_prepuber, "mVals_BMIQ_prepuber_denoised.RDS")
print("Denoising prepuber saved")

# Stop cluster
stopCluster(cl)


