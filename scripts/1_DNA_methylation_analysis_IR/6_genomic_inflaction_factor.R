
# GENOMIC INFLACTION FACTOR------

library(bacon)
library(readr)

within1 = read_csv("./RESULTS_ALVARO_denoised/G1_DMPs_significants_adjusted_NW_non_IR_no_change_WITHIN.csv")
within3 = read_csv("./RESULTS_ALVARO_denoised/G3_DMPs_significants_adjusted_OB_OW_IR_to_non_IR_WITHIN.csv")
within4 = read_csv("./RESULTS_ALVARO_denoised/G4_DMPs_significants_adjusted_OB_OW_non_IR_to_IR_WITHIN.csv")

betweeng3g5 = read_csv("./RESULTS_ALVARO_denoised/Between_G3vsG5_between_OBIRNOCHANGE_VS_OBIRtononIR.csv")
betweeng3g4 = read_csv("./RESULTS_ALVARO_denoised/Between_G3vsG4_between_OBOWIRtononIR_VS_OBOWnonIRtoIR.csv")
betweeng2g4 = read_csv("./RESULTS_ALVARO_denoised/Between_G2vsG4_between_OBOWnonIRnochange_VS_OBOWnonIRtoIR.csv")

cross_sect1 = read_csv("./RESULTS_ALVARO_denoised/IR_VS_NOIR_PUBER.csv")
cross_sect2 = read_csv("./RESULTS_ALVARO_denoised/NoIRNormopesoVSIRSobOb_PUBER.csv")
cross_sect3 = read_csv("./RESULTS_ALVARO_denoised/NoIRSobObVSIRSobOb_PUBER.csv")

statistics = data.frame("G3vsG5" = betweeng3g5$t,
                        "G3vsG4" = betweeng3g4$t,
                        "G3vsG4" = betweeng2g4$t,
                        "G1" = within1$t,
                        "G3" = within3$t,
                        "G4" = within4$t,
                        "IRvsnoIR" = cross_sect1$t,
                        "IRowobvsnoIRnw" = cross_sect2$t,
                        "IRowobnoIRowob" = cross_sect3$t
                        )

bacon = bacon(teststatistics = statistics)
inflation(bacon)

# saveRDS(bacon, "./genomic_inflation_factor.RDS")