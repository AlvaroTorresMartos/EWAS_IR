

library(dplyr)
library(stringr)
library(readr)
library(Hmisc)

within1 = read_csv("./RESULTS_ALVARO_denoised/G1_DMPs_significants_adjusted_NW_non_IR_no_change_WITHIN.csv") 
within3 = read_csv("./RESULTS_ALVARO_denoised/G3_DMPs_significants_adjusted_OB_OW_IR_to_non_IR_WITHIN.csv") 
within4 = read_csv("./RESULTS_ALVARO_denoised/G4_DMPs_significants_adjusted_OB_OW_non_IR_to_IR_WITHIN.csv")

betweeng3g5 = read_csv("./RESULTS_ALVARO_denoised/Between_G3vsG5_between_OBIRNOCHANGE_VS_OBIRtononIR.csv")
betweeng3g4 = read_csv("./RESULTS_ALVARO_denoised/Between_G3vsG4_between_OBOWIRtononIR_VS_OBOWnonIRtoIR.csv")
betweeng2g4 = read_csv("./RESULTS_ALVARO_denoised/Between_G2vsG4_between_OBOWnonIRnochange_VS_OBOWnonIRtoIR.csv")

within1 = within1 %>%
  dplyr::mutate(comparison = "within_g1") %>% 
  # dplyr::filter(P.Value < 0.0001) %>% 
  dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
                                              TRUE ~ UCSC_RefGene_Name), 
                GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
                                                TRUE ~ GencodeCompV12_NAME)) %>%
  dplyr::mutate(key = case_when(
    !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
    !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
                        TRUE ~  Name))

within3 = within3 %>% 
  dplyr::mutate(comparison = "within_g3") %>%
  # dplyr::filter(P.Value < 0.0001) %>% 
  dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
                                              TRUE ~ UCSC_RefGene_Name), 
                GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
                                                TRUE ~ GencodeCompV12_NAME)) %>%
  dplyr::mutate(key = case_when(
    !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
    !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
    TRUE ~  Name))

within4 = within4 %>% 
  dplyr::mutate(comparison = "within_g4")  %>% 
  # dplyr::filter(P.Value < 0.0001) %>% 
  dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
                                              TRUE ~ UCSC_RefGene_Name), 
                GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
                                                TRUE ~ GencodeCompV12_NAME)) %>%
  dplyr::mutate(key = case_when(
    !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
    !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
    TRUE ~  Name))

betweeng3g5 = betweeng3g5 %>% 
  dplyr::mutate(comparison = "between_g3_g5") %>% 
  # dplyr::filter(P.Value < 0.0001) %>% 
  dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
                                              TRUE ~ UCSC_RefGene_Name), 
                GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
                                                TRUE ~ GencodeCompV12_NAME)) %>%
  dplyr::mutate(key = case_when(
    !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
    !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
    TRUE ~  Name))

betweeng3g4 = betweeng3g4 %>% 
  dplyr::mutate(comparison = "between_g3_g4")  %>% 
  # dplyr::filter(P.Value < 0.0001) %>% 
  dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
                                              TRUE ~ UCSC_RefGene_Name), 
                GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
                                                TRUE ~ GencodeCompV12_NAME)) %>%
  dplyr::mutate(key = case_when(
    !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
    !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
    TRUE ~  Name))

betweeng2g4 = betweeng2g4 %>% 
  dplyr::mutate(comparison = "betweeng2g4")  %>% 
  # dplyr::filter(P.Value < 0.0001) %>% 
  dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
                                              TRUE ~ UCSC_RefGene_Name), 
                GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
                                                TRUE ~ GencodeCompV12_NAME)) %>%
  dplyr::mutate(key = case_when(
    !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
    !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
    TRUE ~  Name))


cross_sect1 = read_csv("./RESULTS_ALVARO_denoised/IR_VS_NOIR_PUBER.csv") %>% 
  dplyr::mutate(comparison = "Pubertal_noIR_vs_IR")  %>% 
  # dplyr::filter(P.Value < 0.0001) %>% 
  dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
                                              TRUE ~ UCSC_RefGene_Name), 
                GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
                                                TRUE ~ GencodeCompV12_NAME)) %>%
  dplyr::mutate(key = case_when(
    !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
    !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
    TRUE ~  Name))

cross_sect2 = read_csv("./RESULTS_ALVARO_denoised/NoIRNormopesoVSIRSobOb_PUBER.csv") %>% 
  dplyr::mutate(comparison = "Pubertal_noIRNw_vs_IRSobOb")  %>% 
  # dplyr::filter(P.Value < 0.0001) %>% 
  dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
                                              TRUE ~ UCSC_RefGene_Name), 
                GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
                                                TRUE ~ GencodeCompV12_NAME)) %>%
  dplyr::mutate(key = case_when(
    !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
    !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
    TRUE ~  Name))

cross_sect3 = read_csv("./RESULTS_ALVARO_denoised/NoIRSobObVSIRSobOb_PUBER.csv") %>% 
  dplyr::mutate(comparison = "Pubertal_noIRSobOb_vs_IRSobOb")  %>% 
  # dplyr::filter(P.Value < 0.0001) %>% 
  dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
                                              TRUE ~ UCSC_RefGene_Name), 
                GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
                                                TRUE ~ GencodeCompV12_NAME)) %>%
  dplyr::mutate(key = case_when(
    !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
    !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
    TRUE ~  Name))


all_lists = rbind(within1, within3, within4, betweeng2g5, betweeng3g4, betweeng3g5, 
                  cross_sect1, cross_sect2, cross_sect3) %>% 
  dplyr::arrange(P.Value) %>% 
  dplyr::mutate(fdr = stats::p.adjust(P.Value, method = "fdr"), 
                key = case_when(key %in% "AC005592.2" ~ "SPRY4-AS1",
                                key %in% "GS1-756B1.2" ~ "LINC00708",
                                key %in% "hsa-mir-1977" ~ "mir1977",
                                TRUE ~ key))

write.csv(all_lists, "./all_lists.csv", row.names = FALSE)

# within_g1_fdr = all_lists %>% 
#   dplyr::filter(comparison == "within_g1") %>%
#   dplyr::filter(fdr < 0.05)
# 
# all_lists_def = all_lists %>% 
#   dplyr::filter(comparison != "within_g1") %>% 
#   dplyr::filter(Name %nin% within_g1_fdr$Name) %>% 
#   dplyr::mutate(key = case_when(key %in% "AC005592.2" ~ "SPRY4-AS1", 
#                                 key %in% "GS1-756B1.2" ~ "LINC00708", 
#                                 key %in% "hsa-mir-1977" ~ "mir1977",
#                                 TRUE ~ key))

all_lists_fdr = all_lists %>%
  dplyr::filter(fdr < 0.05)

within_g1_fdr = all_lists_fdr %>%
  dplyr::filter(comparison == "within_g1")

all_lists_fdr2 = all_lists_fdr %>%
  dplyr::filter(comparison != "within_g1") %>%
  dplyr::filter(Name %nin% within_g1_fdr$Name) %>%
  dplyr::mutate(key = case_when(key %in% "AC005592.2" ~ "SPRY4-AS1",
                                key %in% "GS1-756B1.2" ~ "LINC00708",
                                key %in% "hsa-mir-1977" ~ "mir1977",
                                TRUE ~ key))



# write.csv(all_lists_fdr2, "./definitive_list.csv", row.names = TRUE)


