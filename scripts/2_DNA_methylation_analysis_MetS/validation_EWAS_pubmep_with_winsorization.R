
# Load packages -------
library(lm.beta)
library(dplyr)
library(broom)
library(readxl)
library(purrr)
library(tidyr)
library(tibble)
library(gtsummary)
library(DescTools) # install.packages("DescTools_0.99.50.tar.gz", repos = NULL, type = "source")

# Key function to iterate the linear models --------
source("./scripts/recursive_lm_function.R")

# Import datasets ----
data_t1 = readRDS("./data/processed/EWAS_pubmep_t1_updated.RDS")
data_t2 = readRDS("./data/processed/EWAS_pubmep_t2_updated.RDS")

# Exclude a fasting glucose value of 294 ----
data_t2 = data_t2 %>% 
  dplyr::mutate(Glucose_zscore = case_when(Glucose_zscore > 28 ~ NA, 
                                           TRUE ~ Glucose_zscore ))

# Select the outcomes -----
phenotypes = data_t1 %>% 
  dplyr::select(BMI_zscore, WC_zscore, FMI, Glucose_zscore, 
                Insulin_zscore, HOMA_IR_zscore, 
                QUICKI, DBP_zscore_Stavnsbo, 
                SBP_zscore_Stavnsbo,
                TAG_zscore, HDLc_zscore, LDLc_zscore) %>% 
  colnames() 



# 1) Prepubertal -------
col = colnames(data_t1)
# dna methylation prepubertal
col[3:122] = base::paste(col[3:122],"prepubertal", sep="_") # 195 paper vasn
col[3:122] = janitor::make_clean_names(col[3:122], case = "parsed")
colnames(data_t1) = col
# CpGs predictors
predictors = colnames(data_t1)[3:122] 

data_t1 = data_t1 %>% 
  dplyr::mutate(across(all_of(predictors), 
                       ~ DescTools::Winsorize(.x, probs = c(0.05, 0.95), na.rm = TRUE)))

assoc = data.frame(Outcome = as.character(), 
                   Predictor = as.character(), 
                   estimate = as.numeric(), 
                   std_estimate = as.numeric(), 
                   p.value = as.numeric(), 
                   all = as.character())




## Iterative linear models  ----
for (i in 1:length(phenotypes)){
  assoc2 = lapply(predictors, recursive_lm, outcome = phenotypes[i], 
                    data = data_t1, formula = "+ Age + Sex + Origen + CD8T + CD4T + NK + Bcell + Mono + Neu" )
  assoc2 = purrr::map_dfr(assoc2, ~.x, bind_rows)  %>% 
    dplyr::arrange(p.value) %>% # FDR
    dplyr::mutate(fdr = stats::p.adjust(p.value, method = "fdr")) %>% 
    dplyr::relocate(fdr, .after = p.value)
  assoc = rbind(assoc, assoc2)
}




# 2) Pubertal ------

col = colnames(data_t2)
# dna methylation prepubertal 
col[3:122] = base::paste(col[3:122],"pubertal", sep="_") # 195 paper vasn
col[3:122] = janitor::make_clean_names(col[3:122], case = "parsed")
colnames(data_t2) = col
# CpGs predictors
predictors = colnames(data_t2)[3:122] 

data_t2 = data_t2 %>% 
  dplyr::mutate(across(all_of(predictors), 
                       ~ DescTools::Winsorize(.x, probs = c(0.05, 0.95), na.rm = TRUE)))

## Iterative linear models ----
for (i in 1:length(phenotypes)){
  if(i == 3){
    assoc2 = lapply(predictors, recursive_lm, outcome = phenotypes[i], 
                    data = data_t2, formula = "+ Age + Sex + CD8T + CD4T + NK + Bcell + Mono + Neu" )
  } else{
    assoc2 = lapply(predictors, recursive_lm, outcome = phenotypes[i], 
                    data = data_t2, formula = "+ Age + Sex + Origen + CD8T + CD4T + NK + Bcell + Mono + Neu" )
  }
    assoc2 = purrr::map_dfr(assoc2, ~.x, bind_rows)  %>% 
    dplyr::arrange(p.value) %>% # FDR
    dplyr::mutate(fdr = stats::p.adjust(p.value, method = "fdr")) %>% 
    dplyr::relocate(fdr, .after = p.value)
  assoc = rbind(assoc, assoc2)
}

assoc_fdr_pooled = assoc %>% 
  dplyr::arrange(p.value) %>% # FDR
  dplyr::mutate(fdr = stats::p.adjust(p.value, method = "fdr")) %>% 
  dplyr::filter(fdr <  0.05)

assoc_fdr = assoc %>% 
  dplyr::filter(fdr <  0.05)

## Export results -----
# write.csv(assoc, "./results/2025_11_03_pubmep_associations_between_CpGs_and_outcomes_winsorization.csv",
#            row.names = FALSE)






