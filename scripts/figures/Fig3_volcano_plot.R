# Paper list ----
library(dplyr)
library(stringr)
library(readr)
library(Hmisc)
library(ggplot2)
library(ggrepel)
library(patchwork)
# 
# within1 = read_csv("./RESULTS_ALVARO_denoised/G1_DMPs_significants_adjusted_NW_non_IR_no_change_WITHIN.csv") 
# within3 = read_csv("./RESULTS_ALVARO_denoised/G3_DMPs_significants_adjusted_OB_OW_IR_to_non_IR_WITHIN.csv") 
# within4 = read_csv("./RESULTS_ALVARO_denoised/G4_DMPs_significants_adjusted_OB_OW_non_IR_to_IR_WITHIN.csv")
# 
# betweeng3g5 = read_csv("./RESULTS_ALVARO_denoised/Between_G3vsG5_between_OBIRNOCHANGE_VS_OBIRtononIR.csv")
# betweeng3g4 = read_csv("./RESULTS_ALVARO_denoised/Between_G3vsG4_between_OBOWIRtononIR_VS_OBOWnonIRtoIR.csv")
# betweeng2g5 = read_csv("./RESULTS_ALVARO_denoised/Between_G2vsG4_between_OBOWnonIRnochange_VS_OBOWnonIRtoIR.csv")
# 
# within1 = within1 %>%
#   dplyr::mutate(comparison = "within_g1") %>% 
#   # dplyr::filter(P.Value < 0.0001) %>% 
#   dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
#                                               TRUE ~ UCSC_RefGene_Name), 
#                 GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
#                                                 TRUE ~ GencodeCompV12_NAME)) %>%
#   dplyr::mutate(key = case_when(
#     !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
#     !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
#     TRUE ~  Name))
# 
# within3 = within3 %>% 
#   dplyr::mutate(comparison = "within_g3") %>%
#   # dplyr::filter(P.Value < 0.0001) %>% 
#   dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
#                                               TRUE ~ UCSC_RefGene_Name), 
#                 GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
#                                                 TRUE ~ GencodeCompV12_NAME)) %>%
#   dplyr::mutate(key = case_when(
#     !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
#     !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
#     TRUE ~  Name))
# 
# within4 = within4 %>% 
#   dplyr::mutate(comparison = "within_g4")  %>% 
#   # dplyr::filter(P.Value < 0.0001) %>% 
#   dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
#                                               TRUE ~ UCSC_RefGene_Name), 
#                 GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
#                                                 TRUE ~ GencodeCompV12_NAME)) %>%
#   dplyr::mutate(key = case_when(
#     !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
#     !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
#     TRUE ~  Name))
# 
# betweeng3g5 = betweeng3g5 %>% 
#   dplyr::mutate(comparison = "between_g3_g5") %>% 
#   # dplyr::filter(P.Value < 0.0001) %>% 
#   dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
#                                               TRUE ~ UCSC_RefGene_Name), 
#                 GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
#                                                 TRUE ~ GencodeCompV12_NAME)) %>%
#   dplyr::mutate(key = case_when(
#     !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
#     !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
#     TRUE ~  Name))
# 
# betweeng3g4 = betweeng3g4 %>% 
#   dplyr::mutate(comparison = "between_g3_g4")  %>% 
#   # dplyr::filter(P.Value < 0.0001) %>% 
#   dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
#                                               TRUE ~ UCSC_RefGene_Name), 
#                 GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
#                                                 TRUE ~ GencodeCompV12_NAME)) %>%
#   dplyr::mutate(key = case_when(
#     !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
#     !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
#     TRUE ~  Name))
# 
# betweeng2g5 = betweeng2g5 %>% 
#   dplyr::mutate(comparison = "between_g2_g5")  %>% 
#   # dplyr::filter(P.Value < 0.0001) %>% 
#   dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
#                                               TRUE ~ UCSC_RefGene_Name), 
#                 GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
#                                                 TRUE ~ GencodeCompV12_NAME)) %>%
#   dplyr::mutate(key = case_when(
#     !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
#     !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
#     TRUE ~  Name))
# 
# 
# cross_sect1 = read_csv("./RESULTS_ALVARO_denoised/IR_VS_NOIR_PUBER.csv") %>% 
#   dplyr::mutate(comparison = "Pubertal_noIR_vs_IR")  %>% 
#   # dplyr::filter(P.Value < 0.0001) %>% 
#   dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
#                                               TRUE ~ UCSC_RefGene_Name), 
#                 GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
#                                                 TRUE ~ GencodeCompV12_NAME)) %>%
#   dplyr::mutate(key = case_when(
#     !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
#     !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
#     TRUE ~  Name))
# 
# cross_sect2 = read_csv("./RESULTS_ALVARO_denoised/NoIRNormopesoVSIRSobOb_PUBER.csv") %>% 
#   dplyr::mutate(comparison = "Pubertal_noIRNw_vs_IRSobOb")  %>% 
#   # dplyr::filter(P.Value < 0.0001) %>% 
#   dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
#                                               TRUE ~ UCSC_RefGene_Name), 
#                 GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
#                                                 TRUE ~ GencodeCompV12_NAME)) %>%
#   dplyr::mutate(key = case_when(
#     !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
#     !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
#     TRUE ~  Name))
# 
# cross_sect3 = read_csv("./RESULTS_ALVARO_denoised/NoIRSobObVSIRSobOb_PUBER.csv") %>% 
#   dplyr::mutate(comparison = "Pubertal_noIRSobOb_vs_IRSobOb")  %>% 
#   # dplyr::filter(P.Value < 0.0001) %>% 
#   dplyr::mutate(UCSC_RefGene_Name = case_when(str_detect(UCSC_RefGene_Name, ";") ~ str_replace(UCSC_RefGene_Name, ";.*", ""), 
#                                               TRUE ~ UCSC_RefGene_Name), 
#                 GencodeCompV12_NAME = case_when(str_detect(GencodeCompV12_NAME, ";") ~ str_replace(GencodeCompV12_NAME, ";.*", ""), 
#                                                 TRUE ~ GencodeCompV12_NAME)) %>%
#   dplyr::mutate(key = case_when(
#     !is.na(UCSC_RefGene_Name) ~  UCSC_RefGene_Name,
#     !is.na(GencodeCompV12_NAME) ~  GencodeCompV12_NAME,                         
#     TRUE ~  Name))
# 
# 
# all_lists = rbind(within1, within3, within4, betweeng2g5, betweeng3g4, betweeng3g5, 
#                   cross_sect1, cross_sect2, cross_sect3) %>% 
#   dplyr::arrange(P.Value) %>% 
#   dplyr::mutate(fdr = stats::p.adjust(P.Value, method = "fdr"))
# 
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
#                                 TRUE ~ key), 
#                 label = case_when(stringr::str_detect(key, pattern = "cg") ~ key, 
#                                   TRUE ~ base::paste(key, Name, sep="_"))
#                 )
# 
# saveRDS(all_lists_def, "./all_lists_volcanoplot.RDS")

all_lists_def = read_rds("./all_lists_volcanoplot.RDS")

# significant_cpgs = all_lists_def %>%
#   dplyr::filter(fdr < 0.05)

# options(bitmapType = "cairo")

# pastel_cols <- c(
#   "pastel_blue"     = "#A7C7E7",
#   "pastel_coral"    = "#F7A8A6",
#   "pastel_mint"     = "#B6E3C8",
#   "pastel_lilac"    = "#D7BCE8",
#   "pastel_yellow"   = "#F7E4A2",
#   "pastel_orange"   = "#F6C39B",
#   "pastel_turquoise"= "#A7E3E7"
# )

# plot 1------
p1_hyper = all_lists_def  %>%
  dplyr::filter(comparison %in% c("within_g3", "within_g4")) %>%
  dplyr::filter(logFC > 0) %>%
  dplyr::arrange(fdr) %>%
  dplyr::slice(1:10)

p1_hypo = all_lists_def  %>%
  dplyr::filter(comparison %in% c("within_g3", "within_g4")) %>%
  dplyr::filter(logFC < 0) %>%
  dplyr::arrange(fdr) %>%
  dplyr::slice(1:10)

# cols = c("Higher DNA methylation" = "#f46d43", #"#FB9A99", 
#          "Lower DNA methylation" = "#4575b4", #"#A6CEE3", 
#          "ns" =  "#FAF3E0")
sizes = c("Higher DNA methylation" = 3.5, 
          "Lower DNA methylation" = 3.5, 
          "ns" = 1.5) 
alphas = c("Higher DNA methylation" = 1, 
           "Lower DNA methylation" = 1, 
           "ns" = 0.1) # 0.05

shapes = c(
  "within_g3" = 21,   # circle
  "within_g4" = 22)   # square

p1 = all_lists_def  %>% 
  dplyr::arrange(fdr) %>% 
  dplyr::filter(comparison %in% c("within_g3", "within_g4")) %>% 
  # dplyr::slice(1:1000) %>%
  # dplyr::slice(1:500000) %>% 
  dplyr::mutate(
    direction = case_when(logFC > 0 & fdr < 0.05  ~ "Higher DNA methylation", 
                          logFC < 0 & fdr < 0.05  ~ "Lower DNA methylation", 
                          TRUE ~ "ns"), 
    label = case_when(fdr < 0.05 ~ label, 
                      TRUE ~ NA), 
    label = case_when(label %in% p1_hyper$label ~ label, 
                      label %in% p1_hypo$label ~ label, 
                      TRUE ~ NA), 
    sig_comp = ifelse(fdr < 0.05, as.character(comparison), NA_character_)  
  ) %>% 
  ggplot(aes(x = logFC, 
             y = -log10(fdr), 
             fill = sig_comp,
             size = direction,
             alpha = direction,
             label = label, 
             shape = factor(comparison))) + 
  geom_point(colour = "black", stroke = 0.4) + # shape = 21, 
  guides(
    shape = guide_legend(
      title = "Longitudinal within-group comparisons",
      override.aes = list(size = 5, colour = "black", alpha = 1)
    )) + 
  scale_size_manual(values = sizes, guide = "none") + 
  scale_alpha_manual(values = alphas, guide = "none") +
  scale_shape_manual(
    name   = "Longitudinal within-group comparisons",   
    values = shapes, 
    breaks = c("within_g3", "within_g4"), 
    labels = c("G3: Ov/Ob IR to non-IR", 
               "G4: Ov/Ob non-IR to IR")
  ) +
  scale_fill_manual(
    name   = "Longitudinal within-group comparisons",   
    values = c(
      "within_g3" = "#A7C7E7",     
      "within_g4" = "#F7A8A6"       
    ),
    breaks = c("within_g3", "within_g4"),
    labels = c("G3: Ov/Ob IR to non-IR", 
               "G4: Ov/Ob non-IR to IR"),
    na.value = "white"             
  ) +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)) +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, 0.5)) +
  geom_label_repel(max.overlaps = Inf, size = 5, 
                   box.padding = 0.8, point.padding = 0.4, 
                   segment.color = 'grey50', 
                   fill = "white",                  
                   colour = "black",
                   force = 2, force_pull = 0.1, seed = 123) +
  labs(title = "Longitudinal within-group comparisons", 
       x = expression(log[2](FoldChange)),
       y = expression(-log[10](FDR))) + 
  theme_light(
    base_size = 14) + 
  theme(
    plot.caption = element_text(size = 10),
    axis.text = element_text(color = "black"), 
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) 

# ggsave("p1.png", p1,
#        width = 59.4, height = 42, units = "cm")

# p1

# plot 2------
shapes = c(
  "between_g2_g5" = 21,   # circle
  "between_g3_g4" = 22,  # square
  "between_g3_g5" = 23)   

p2_hyper = all_lists_def  %>%
  dplyr::filter(comparison %in% c("between_g2_g5", "between_g3_g4", "between_g3_g5")) %>%
  dplyr::filter(logFC > 0) %>%
  dplyr::arrange(fdr) %>%
  dplyr::slice(1:10)

p2_hypo = all_lists_def  %>%
  dplyr::filter(comparison %in% c("between_g2_g5", "between_g3_g4", "between_g3_g5")) %>%
  dplyr::filter(logFC < 0) %>%
  dplyr::arrange(fdr) %>%
  dplyr::slice(1:10)

p2 = all_lists_def  %>% 
  dplyr::arrange(fdr) %>% 
  dplyr::filter(comparison %in% c("between_g2_g5", "between_g3_g4", "between_g3_g5")) %>% 
  # dplyr::slice(1:1000) %>%
  dplyr::mutate(
    direction = case_when(
      logFC > 0 & fdr < 0.05  ~ "Higher DNA methylation", 
      logFC < 0 & fdr < 0.05  ~ "Lower DNA methylation", 
      TRUE ~ "ns"), 
    label = case_when(
      fdr < 0.05 ~ label, 
      TRUE ~ NA), 
    label = case_when(
      label %in% p2_hyper$label ~ label, 
      label %in% p2_hypo$label ~ label, 
      TRUE ~ NA), 
    sig_comp = ifelse(fdr < 0.05, as.character(comparison), NA_character_)) %>% 
  ggplot(aes(x = logFC, 
             y = -log10(fdr), 
             fill = sig_comp,
             size = direction,
             alpha = direction,
             label = label, 
             shape = factor(comparison))) + 
  geom_point(colour = "black", stroke = 0.4) +  
  guides(
    shape = guide_legend(
      title = "Longitudinal between-group comparisons",
      override.aes = list(size = 5, colour = "black", alpha = 1))) + 
  scale_size_manual(values = sizes, guide = "none") + 
  scale_alpha_manual(values = alphas, guide = "none") +
  scale_shape_manual(
    name   = "Longitudinal between-group comparisons", 
    values = shapes, 
    breaks = c("between_g2_g5", "between_g3_g4", "between_g3_g5"), 
    labels = c(expression("G2: Ov/Ob non-IR no change " ~ italic("vs.") ~ " G4: Ov/Ob non-IR to IR"),
               expression("G3: Ov/Ob IR to non-IR " ~ italic("vs.") ~ " G4: Ov/Ob non-IR to IR"), 
               expression("G3: Ov/Ob IR to non-IR "  ~ italic("vs.") ~ "G5: Ov/Ob IR no change"))) +
  scale_fill_manual(
    name   = "Longitudinal between-group comparisons", 
    values = c(
      "between_g2_g5" = "#F7E4A2",     
      "between_g3_g4" = "#B6E3C8", 
      "between_g3_g5" = "#D7BCE8"), 
    breaks = c("between_g2_g5", "between_g3_g4", "between_g3_g5"), 
    labels = c(
      expression("G2: Ov/Ob non-IR no change " ~ italic("vs.") ~ " G4: Ov/Ob non-IR to IR"),
      expression("G3: Ov/Ob IR to non-IR " ~ italic("vs.") ~ " G4: Ov/Ob non-IR to IR"), 
      expression("G3: Ov/Ob IR to non-IR "  ~ italic("vs.") ~ "G5: Ov/Ob IR no change")),
    na.value = "white") +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)) +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, 0.5)) +
  geom_label_repel(max.overlaps = Inf, size = 5, 
                   box.padding = 0.7, point.padding = 0.4, 
                   segment.color = 'grey50', 
                   fill = "white",                  
                   colour = "black",
                   force = 2, force_pull = 0.1, seed = 123) +
  labs(title = "Longitudinal between-group comparisons", 
       x = expression(log[2](FoldChange)),
       y = expression(-log[10](FDR))) + 
  theme_light(base_size = 14) + 
  theme(
    plot.caption = element_text(size = 10),
    axis.text = element_text(color = "black"),  
    # legend.position = "bottom",  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) 

# p2

# plot 3------
shapes = c(
  "noIR vs IR" = 21,   # circle
  "Nw noIR vs OwOb IR" = 22  # square
)   


p3_hyper = all_lists_def  %>%
  dplyr::filter(comparison %in% c("Pubertal_noIR_vs_IR", "Pubertal_noIRNw_vs_IRSobOb")) %>%
  dplyr::filter(logFC > 0) %>%
  dplyr::arrange(fdr) %>%
  dplyr::slice(1:10)

p3_hypo = all_lists_def  %>%
  dplyr::filter(comparison %in% c("Pubertal_noIR_vs_IR", "Pubertal_noIRNw_vs_IRSobOb")) %>%
  dplyr::filter(logFC < 0) %>%
  dplyr::arrange(fdr) %>%
  dplyr::slice(1:10)

p3 = all_lists_def  %>% 
  dplyr::arrange(fdr) %>% 
  dplyr::filter(comparison %in% c("Pubertal_noIR_vs_IR", "Pubertal_noIRNw_vs_IRSobOb")) %>% 
  # dplyr::slice(1:1000) %>%
  dplyr::mutate(
    direction = case_when(logFC > 0 & fdr < 0.05  ~ "Higher DNA methylation", 
                          logFC < 0 & fdr < 0.05  ~ "Lower DNA methylation", 
                          TRUE ~ "ns"), 
    label = case_when(fdr < 0.05 ~ label, 
                      TRUE ~ NA), 
    label = case_when(label %in% p3_hyper$label ~ label, 
                      label %in% p3_hypo$label ~ label, 
                      TRUE ~ NA), 
    comparison = case_when(comparison %in% c("Pubertal_noIRNw_vs_IRSobOb") ~ "Nw noIR vs OwOb IR", 
                           comparison %in% c("Pubertal_noIR_vs_IR") ~ "noIR vs IR", 
    ), 
    sig_comp = ifelse(fdr < 0.05, as.character(comparison), NA_character_)
  ) %>% 
  ggplot(aes(x = logFC, 
             y = -log10(fdr), 
             fill = sig_comp,
             size = direction,
             alpha = direction,
             label = label, 
             shape = factor(comparison))) + 
  geom_point(colour = "black", stroke = 0.4) + 
  guides(
    shape = guide_legend(
      title = "Pubertal cross-sectional comparisons",
      override.aes = list(size = 5, colour = "black", alpha = 1)
    )
  ) +
  scale_size_manual(values = sizes, guide = "none") + 
  scale_alpha_manual(values = alphas, guide = "none") +
  scale_shape_manual(
    name   = "Pubertal cross-sectional comparisons", 
    values = shapes, 
    breaks = c("Nw noIR vs OwOb IR", "noIR vs IR"), 
    labels = c(
      expression("noIR (Nw) " ~ italic("vs.") ~ " IR (Ov/Ob)"), 
      expression("noIR (all) " ~ italic("vs.") ~ "IR (all)"))) +
  scale_fill_manual(
    name   = "Pubertal cross-sectional comparisons", 
    values = c(
      "Nw noIR vs OwOb IR" = "#F6C39B",
      "noIR vs IR" = "#A7E3E7"   ), 
    breaks = c("Nw noIR vs OwOb IR", "noIR vs IR"), 
    labels = c(
      expression("noIR (Nw) " ~ italic("vs.") ~ " IR (Ov/Ob)"), 
      expression("noIR (all) " ~ italic("vs.") ~ "IR (all)")),
    na.value = "white") +
  
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)) +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, 0.5)) +
  
  geom_label_repel(max.overlaps = Inf, size = 5, 
                   box.padding = 0.8, point.padding = 0.4, 
                   segment.color = 'grey50', 
                   fill = "white",                  
                   colour = "black",
                   force = 2, force_pull = 0.1, seed = 123) +
  labs(title = "Pubertal cross-sectional comparisons", 
       x = expression(log[2](FoldChange)),
       y = expression(-log[10](FDR))) + 
  theme_light(base_size = 14) + 
  theme(
    plot.caption = element_text(size = 10),
    axis.text = element_text(color = "black"),  # Texto del eje en negro para mejor contraste
    # legend.position = "bottom",  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) 

# p3

# ggsave("p1.pdf", p1, device = "pdf", width = 29.7, height = 21, units = "cm")
# # -3 a 4 (pero se podria apurar a 3)
# ggsave("p2.pdf", p2, device = "pdf", width = 42, height = 29.7, units = "cm")
# # -3 a 2.5
# ggsave("p3.pdf", p3, device = "pdf", width = 29.7, height = 21, units = "cm")
# # -3 a 3

fig3 = p1 / p2 / p3 + plot_annotation(tag_levels = 'A')

# saving png ------
ggsave("Fig3.png", fig3,
       width = 59.4, height = 42, units = "cm")

# saving pdf ------
ggsave("Fig3.pdf", fig3,
       width = 59.4, height = 42, units = "cm")

