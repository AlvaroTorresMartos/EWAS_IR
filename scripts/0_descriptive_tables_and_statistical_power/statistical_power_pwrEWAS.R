
library(pwrEWAS)
# help("pwrEWAS")
set.seed(123456789)
results_targetDelta = pwrEWAS(minTotSampleSize = 20,
                              maxTotSampleSize = 100,
                              SampleSizeSteps = 20,
                              NcntPer = 0.5,
                              targetDelta = c(0.1, 0.15, 0.2, 0.25),
                              J = 838417,
                              targetDmCpGs = 100,
                              tissueType = "Blood 5 year olds",
                              detectionLimit = 0.1,
                              DMmethod = "limma",
                              FDRcritVal = 0.05,
                              core = 1,
                              sims = 50)

pwrEWAS_powerPlot(results_targetDelta$powerArray, sd = FALSE)
# saveRDS(results_targetDelta, "./results/results_targetDelta.RDS")