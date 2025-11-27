

library(DMRcate)
library(dplyr)
library(limma)

load("./data/processed/DMR_analysis.RData")

COVARS_FENOS_CONTINUOS_PREPUBER$Origen_T1 = as.factor(COVARS_FENOS_CONTINUOS_PREPUBER$Origen_T1)

COVARS_FENOS_CONTINUOS_PREPUBER = COVARS_FENOS_CONTINUOS_PREPUBER %>% 
  dplyr::mutate(IR = case_when(HOMA_0_AUG_Time == "IR" ~ 1, 
                               HOMA_0_AUG_Time == "No-IR" ~ 0), 
                IR = as.factor(IR))

design = model.matrix( ~ 0 + IR + 
                         counts.CD8T + counts.CD4T + counts.NK + counts.Bcell + 
                         counts.Mono + Age_Time + Sex_Time + Origen_T1,  
                       data = COVARS_FENOS_CONTINUOS_PREPUBER)
colnames(design)

# fit = lmFit(MICROARRAY_PREPUBER, design)

contMatrix = makeContrasts(nonIRvsIR = IR0 - IR1, 
                           levels =  design)




# ny annotation------
myannotation = cpg.annotate(datatype = "array", 
                            object = as.matrix(MICROARRAY_PREPUBER), 
                            what = "M",
                            arraytype = "EPIC",
                            analysis.type = "differential", 
                            design = design, 
                            contrasts = TRUE, 
                            cont.matrix = contMatrix,
                            fdr = 0.05,
                            coef = "nonIRvsIR")

myannotation2 = changeFDR(annot = myannotation, FDR = 0.99)

dmrcoutput =  dmrcate(myannotation, pcutoff = 0.001, lambda = 1000, C = 2)

results.ranges = extractRanges(dmrcoutput, genome = "hg19")
results.ranges_df = as.data.frame(results.ranges)

# write.csv(results.ranges_df, 
#           file = "./DMR_analysis_IR_results.csv", row.names = FALSE)




