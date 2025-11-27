
library(dplyr)
library(stringr)
library(readr)
library(Hmisc)
library(ggplot2)
library(ggrepel)
library(patchwork)


all_lists_def = read_rds("./all_lists_volcanoplot.RDS")


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

cols = c("Higher DNA methylation" = "#f46d43", #"#FB9A99", 
         "Lower DNA methylation" = "#4575b4", #"#A6CEE3", 
         "ns" =  "#FAF3E0")
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
  dplyr::mutate(
  direction = case_when(logFC > 0 & fdr < 0.05  ~ "Higher DNA methylation", 
                        logFC < 0 & fdr < 0.05  ~ "Lower DNA methylation", 
                         TRUE ~ "ns"), 
  label = case_when(fdr < 0.05 ~ label, 
                    TRUE ~ NA), 
  label = case_when(label %in% p1_hyper$label ~ label, 
                    label %in% p1_hypo$label ~ label, 
                    TRUE ~ NA)
  ) %>% 
  ggplot(aes(x = logFC, y = -log10(fdr), 
             fill = direction,
             size = direction,
             alpha = direction,
             label = label, 
             shape = factor(comparison))) + 
  geom_point(colour = "black", stroke = 0.4) + # shape = 21, 
  scale_shape_manual(values = shapes, 
                     breaks = c("within_g3", "within_g4"), 
                     labels = c("G3: Ov/Ob IR to non-IR", 
                                "G4: Ov/Ob non-IR to IR")) +
  scale_size_manual(values = sizes, guide = "none") + 
  scale_fill_manual(values = cols, guide = "none") +
  scale_alpha_manual(values = alphas, guide = "none") +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)) +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, 0.5)) +
  guides(
    shape = guide_legend(
      title = "Within comparison",
      override.aes = list(size = 5, fill = "grey80", colour = "black", alpha = 1)
    )
  ) + 
  geom_label_repel(max.overlaps = Inf, size = 3.5, 
                   box.padding = 0.6, point.padding = 0.4, 
                   segment.color = 'grey50', 
                   fill = "white",                  
                   colour = "black",
                   force = 2, force_pull = 0.1, seed = 123) +
  labs(title = "Longitudinal within comparisons", 
       x = expression(log[2](FoldChange)),
       y = expression(-log[10](FDR))) + 
  theme_light(base_size = 14) + 
  theme(
    plot.caption = element_text(size = 10),
    axis.text = element_text(color = "black"), 
    legend.position = "bottom",  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) 

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
  dplyr::mutate(
    direction = case_when(logFC > 0 & fdr < 0.05  ~ "Higher DNA methylation", 
                          logFC < 0 & fdr < 0.05  ~ "Lower DNA methylation", 
                          TRUE ~ "ns"), 
    label = case_when(fdr < 0.05 ~ label, 
                      TRUE ~ NA), 
    label = case_when(label %in% p2_hyper$label ~ label, 
                      label %in% p2_hypo$label ~ label, 
                      TRUE ~ NA)
  ) %>% 
  ggplot(aes(x = logFC, y = -log10(fdr), 
             fill = direction,
             size = direction,
             alpha = direction,
             label = label, 
             shape = factor(comparison))) + 
  geom_point(colour = "black", stroke = 0.4) + # shape = 21, 
  scale_shape_manual(values = shapes, 
                     breaks = c("between_g2_g5", "between_g3_g4", "between_g3_g5"), 
                     labels = c("G2: Ov/Ob non-IR no change VS G4: Ov/Ob non-IR to IR",
                                "G3: Ov/Ob IR to non-IR VS G4: Ov/Ob non-IR to IR", 
                                "G3: Ov/Ob IR to non-IR VS G5: Ov/Ob IR no change")) +
  scale_size_manual(values = sizes, guide = "none") + 
  scale_fill_manual(values = cols, guide = "none") +
  scale_alpha_manual(values = alphas, guide = "none") +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)) +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, 0.5)) +
  guides(
    shape = guide_legend(
      title = "Between comparison",
      override.aes = list(size = 5, fill = "grey80", colour = "black", alpha = 1)
    )
  ) + 
  geom_label_repel(max.overlaps = Inf, size = 3.5, 
                   box.padding = 0.6, point.padding = 0.4, 
                   segment.color = 'grey50', 
                   fill = "white",                  
                   colour = "black",
                   force = 2, force_pull = 0.1, seed = 123) +
  labs(title = "Longitudinal between comparisons", 
       x = expression(log[2](FoldChange)),
       y = expression(-log[10](FDR))) + 
  theme_light(base_size = 14) + 
  theme(
    plot.caption = element_text(size = 10),
    axis.text = element_text(color = "black"),  
    legend.position = "bottom",  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) 


shapes = c(
  "noIR vs IR" = 21,   # circle
  "Nw noIR vs OwOb IR" = 22  # square
  )   
# "Pubertal_noIRSobOb_vs_IRSobOb" = 23

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
                           )
  ) %>% 
  ggplot(aes(x = logFC, y = -log10(fdr), 
             fill = direction,
             size = direction,
             alpha = direction,
             label = label, 
             shape = factor(comparison))) + 
  geom_point(colour = "black", stroke = 0.4) + # shape = 21, 
  scale_shape_manual(values = shapes, 
                     breaks = c("Nw noIR vs OwOb IR", "noIR vs IR"), 
                     labels = c("noIR (Nw) vs IR (OwOb)", "noIR (all) vs IR (all)")) +
  scale_size_manual(values = sizes, guide = "none") + 
  scale_fill_manual(values = cols, guide = "none") +
  scale_alpha_manual(values = alphas, guide = "none") +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)) +
  scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, 0.5)) +
  guides(
    shape = guide_legend(
      title = "Pubertal cross-sectional comparison",
      override.aes = list(size = 5, fill = "grey80", colour = "black", alpha = 1)
    )
  ) + 
  geom_label_repel(max.overlaps = Inf, size = 3.5, 
                   box.padding = 0.6, point.padding = 0.4, 
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
    legend.position = "bottom",  
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) 

# ggsave("p1.pdf", p1, device = "pdf", width = 29.7, height = 21, units = "cm")
# # -3 a 4 (pero se podria apurar a 3)
# ggsave("p2.pdf", p2, device = "pdf", width = 42, height = 29.7, units = "cm")
# # -3 a 2.5
# ggsave("p3.pdf", p3, device = "pdf", width = 29.7, height = 21, units = "cm")
# # -3 a 3

fig3 = p1 / p2 / p3 + plot_annotation(tag_levels = 'A')

ggsave("Fig3.png", fig3,
       width = 59.4, height = 42, units = "cm")

