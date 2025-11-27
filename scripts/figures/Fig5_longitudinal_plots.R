

library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)
library(forcats)
library(patchwork)

bVals_BMIQ = readr::read_rds("./PRIMER_ANALISIS/BASES_DATOS_METILACION_GENERADAS_PREPROCESADAS/bVals_BMIQ_NEW.RDS") %>% 
  tibble::column_to_rownames(var = colnames(.)[1])

targets = readr::read_csv("./MethylationEPIC_Sample_Sheet_ConcepcionAguilera_ALL_SAMPLES_to_1_1_2020.csv")

targets$Sample_Name <- gsub("_",".",targets$Sample_Name)

OLD <- c("R04767","R06071","R15536","R15604","R19729","R21247","R23281","R24959","R33384","R33551","R38002","R38183","R39751","R39962","R41181","R48442","R50197","R66312","R68913","R73191","R75314","R79081","R84145","R87905","R91724","R94897","R97793","S00681","S04885","S07203","S07989","S08206","S09513","S10605","S10751","S11300","S12854","S17163","S17875","S18220","S18312","S20789","S22181","S23321","S24114","S24971","S26140","S28200","S28967","S29760","S34730","S35116","S35901","S37056","S37925","S39280","S39767","S40403","S40754","S40755","S41894","S42693","S43108","S43120","S43130","S43518","S43541","S44237","S44251","S44281","S45131","S47677","S47760","S48108","S49990","S50050","S50144","S52236","S52889","S53656","S54613","S56185","S58315","S58671","S60493","S60709","S60829","S64632","S66605","S66870","S67718","S69608","S70326","S71367","S72403","S72647","S72682","S73741","S74848","S74908","S755143","S75679","S75961","S77036","S77980","S78405","S78549","S79522","S795613","S79880","S81492","S82063","S82098","S825037","S82734","S85555","S86176","S86393","S87205","S87406","S88575","S88810","S89592","S90413","S91084","S92224","S92904","S93897","S95885","S98047","S98733","Z004.2","Z005.2","Z006.2","Z010.2","Z011.2","Z013.2","Z014.2","Z015.2","Z016.2","Z021.1","Z021.2","Z022.2","Z027.2","Z030.2","Z032.1","Z033.2","Z034.2","Z035.1","Z041.2","Z043.2","Z044.2","Z048.2","Z051.2","Z055.2","Z070.1","Z070.2","Z074.1","Z074.2","Z077.1","Z079.2","Z080.1","Z081.1","Z091.1","Z093.1","Z093.2","Z094.2","Z095.1","Z099.2","Z101.1","Z101.2","Z102.1","Z102.2","Z104.2","Z105.1","Z105.2","Z106.1","Z112.1","Z114.1","Z114.2","Z115.2","Z118.2","Z120.1","Z120.2","Z124.2","Z127.1","Z129.2","Z130.1","Z131.1","Z133.2","Z136.1","Z136.2","Z137.1","Z137.2","Z138.2","Z139.2","Z140.1","Z140.2","Z141.2","Z142.2","Z143.2","Z144.2","Z147.2","Z148.2","Z149.2","Z153.2","Z156.2","Z160.2","Z166.2","Z167.2","Z176.2","Z180.2","Z42")

NEW <- c("MR021","MR017","MR020","MR007","MR001","MR004","MR025","MR012","MR015","MR014","MR009","MR027","MR003","MR011","MR002","MR024","MR023","MR019","MR010","MR022","MR018","MR008","MR016","MR013","MR005","MR006","MR026","MS265","MS215","MS243","MS282","MS273","MS272","MS224","MS277","MS280","MS226","MS248","MS211","MS276","MS297","MS299","MS236","MS278","MS255","MS216","MS214","MS250","MS202","MS285","MS261","MS252","MS293","MS225","MS269","MS229","MS271","MS246","MS222","MS223","MS233","MS258","MS207","MS205","MS208","MS203","MS230","MS206","MS200","MS201","MS305","MS218","MS260","MS267","MS303","MS228","MS295","MS247","MS249","MS204","MS279","MS227","MS244","MS296","MS212","MS289","MS287","MS270","MS283","MS291","MS251","MS300","MS240","MS241","MS235","MS275","MS210","MS259","MS268","MS254","MS217","MS231","MS221","MS298","MS257","MS237","MS262","MS288","MS238","MS274","MS239","MS302","MS234","MS281","MS264","MS232","MS256","MS301","MS266","MS253","MS220","MS284","MS242","MS213","MS286","MS290","MS245","MS219","MS304","MS292","MS263","Z478","Z437","Z436","Z476","Z446","Z408","Z475","Z465","Z469","Z472","Z440","Z471","Z473","Z477","Z459","Z442","Z443","Z441","Z474","Z454","Z455","Z420","Z480","Z448","Z466","Z410","Z401","Z470","Z416","Z483","Z402","Z406","Z461","Z407","Z434","Z435","Z419","Z444","Z457","Z424","Z400","Z423","Z452","Z427","Z462","Z415","Z458","Z404","Z445","Z414","Z439","Z453","Z484","Z421","Z468","Z438","Z433","Z403","Z405","Z417","Z431","Z432","Z422","Z426","Z425","Z451","Z467","Z464","Z463","Z486","Z487","Z413","Z412","Z430","Z479","Z447","Z482","Z449","Z450","Z460","Z411","Z456")

targets$Sample_Time
targets$Sample_Time[targets$Sample_Name %in% NEW] <- 2
targets$Sample_Time[targets$Sample_Name %in% OLD] <- 1
targets$Sample_Time

targets$Sample_Group <- as.factor(targets$Sample_Group)
targets$Sample_Group <- factor(targets$Sample_Group,levels(targets$Sample_Group)[c(1,6,4,3,5,2)])
table(targets$Sample_Group)

targets = targets %>% 
  dplyr::filter(Sample_Group != "OB/OW non-IR to NW non-IR") %>% 
  tidyr::drop_na(Sample_Group) %>% 
  droplevels()

definitive_lists = readr::read_csv("./definitive_list.csv")

definitive_lists = definitive_lists %>% 
  dplyr::mutate(
  label = case_when(stringr::str_detect(key, pattern = "cg") ~ key, 
                    TRUE ~ base::paste(key, Name, sep="_")))

bVals_BMIQ = bVals_BMIQ %>% 
  tibble::rownames_to_column(var = "CpG") %>% 
  dplyr::filter(CpG %in% definitive_lists$Name) %>%
  pivot_longer(-CpG, names_to = "Code", values_to = "Beta") %>% 
  pivot_wider(names_from = CpG, values_from = Beta) 

melted <- read.csv2("./BASES_METILACION/COPIASEGURIDAD_02_06_2020/BASE_PUBMEP_LONGITUDINAL_LONGformat_NIÑAS_Y_NIÑOS_213inds_02_06_2020_EPIC.csv", header=TRUE, sep=";", stringsAsFactors=F, dec=",", na.strings=c(""," ","NaN","NA"))
melted$Code <- gsub("-",".",melted$Code)
melted$Code_old <- melted$Code

melted1 = melted %>% 
  dplyr::select(Code, Code_old)

melted2 = melted %>% 
  dplyr::select(Code_new_T2, Code_old) %>% 
  dplyr::rename(Code = Code_new_T2)


targets = targets %>% 
  dplyr::select(Sample_Name, Sample_Group, Sample_Time) %>% 
  dplyr::rename(Code = Sample_Name)


bVals_BMIQ = bVals_BMIQ %>% 
  dplyr::mutate(Code = gsub("-", ".", Code)) %>% 
  dplyr::left_join(targets, by = "Code") %>% 
  tidyr::drop_na(Sample_Group) %>% 
  dplyr::left_join(melted1, by = "Code") %>% 
  dplyr::left_join(melted2, by = "Code") 


bVals_BMIQ = bVals_BMIQ %>% 
  dplyr::mutate(Code_old = coalesce(Code_old.x, Code_old.y)) %>% 
  dplyr::select(-c(Code_old.x, Code_old.y))

bVals_BMIQ = bVals_BMIQ %>% 
  dplyr::mutate(Sample_Time = case_when(Sample_Time == 1 ~ "Baseline", 
                          Sample_Time == 2 ~ "Follow-up"), 
                Sample_Group = case_when(Sample_Group == "NW non-IR no change" ~ "G1: Nw non-IR no change", 
                                         Sample_Group == "OB/OW non-IR no change" ~ "G2: Ow/Ob non-IR no change", 
                                         Sample_Group == "OB/OW IR to non-IR" ~ "G3: Ow/Ob IR to non-IR", 
                                         Sample_Group == "OB/OW non-IR to IR" ~ "G4: Ow/Ob non-IR to IR", 
                                         Sample_Group == "OB/OW IR no change" ~ "G5: Ow/Ob IR no change", 
                                         ), 
                Sample_Time = as.factor(Sample_Time),
                Sample_Group = as.factor(Sample_Group)
                )

rename_map = setNames(definitive_lists$label, definitive_lists$Name)

bVals_BMIQ <- bVals_BMIQ %>%
  rename_with(~ rename_map[.x], .cols = any_of(names(rename_map)))

labels = rename_map %>% as.character()



# for (i in labels) {
#   p = ggplot(bVals_BMIQ, aes(x = Sample_Time, y = .data[[i]], group = Code_old)) +
#     # geom_line(alpha = 0.4) + # removing the lines of all the individuals
#     stat_smooth(aes(group = 1), method = "lm", se = TRUE) +
#     stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 17, size = 3) +
#     facet_grid(. ~ Sample_Group) +
#     labs(x = "", y = i)
#   
#   ggsave(
#     filename = file.path("longitudinal_plots", paste0(i, ".pdf")),
#     plot = p, device = "pdf",
#     width = 29.7, height = 21, units = "cm"
#   )
# }


# Multiplanel of longitudinal plots 

# bVals_BMIQ = bVals_BMIQ %>% 
#   dplyr::mutate(
#     Sample_Group = case_when(Sample_Group == "G1: Nw non-IR no change" ~ "G1", 
#                              Sample_Group == "G2: Ow/Ob non-IR no change" ~ "G2",
#                              Sample_Group == "G3: Ow/Ob IR to non-IR" ~ "G3",
#                              Sample_Group == "G4: Ow/Ob non-IR to IR" ~ "G4",
#                              Sample_Group == "G5: Ow/Ob IR no change" ~ "G5"),
#     Sample_Time = case_when(Sample_Time == "Baseline" ~ "T1", 
#                                         Sample_Time == "Follow-up" ~ "T2", 
#                                         ))

p1 = ggplot(bVals_BMIQ, aes(x = Sample_Time, y = EHD2_cg16860712, group = Code_old)) +
  stat_smooth(aes(group = 1), method = "lm", se = TRUE) +
  stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 17, size = 3) +
  facet_grid(. ~ Sample_Group) +
  labs(x = "", y = "EHD2 cg16860712") + 
  theme(
    axis.text.x = element_text(size = 6),        
    strip.text.x = element_text(size = 4.5)    
  )

# p2 = ggplot(bVals_BMIQ, aes(x = Sample_Time, y = LINC00708_cg15584606, group = Code_old)) +
#   stat_smooth(aes(group = 1), method = "lm", se = TRUE) +
#   stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 17, size = 3) +
#   facet_grid(. ~ Sample_Group) +
#   labs(x = "", y = "LINC00708 cg15584606") + 
#   theme(
#     axis.text.x = element_text(size = 6),        
#     strip.text.x = element_text(size = 4.5)    
#   )

p2 = ggplot(bVals_BMIQ, aes(x = Sample_Time, y = PEPD_cg08085561, group = Code_old)) +
  stat_smooth(aes(group = 1), method = "lm", se = TRUE) +
  stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 17, size = 3) +
  facet_grid(. ~ Sample_Group) +
  labs(x = "", y = "PEPD cg08085561")  + 
  theme(
    axis.text.x = element_text(size = 6),        
    strip.text.x = element_text(size = 4.5)    
  )

p3 = ggplot(bVals_BMIQ, aes(x = Sample_Time, y = SLC2A9_cg16147221, group = Code_old)) +
  stat_smooth(aes(group = 1), method = "lm", se = TRUE) +
  stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 17, size = 3) +
  facet_grid(. ~ Sample_Group) +
  labs(x = "", y = "SLC2A9 cg16147221")  + 
  theme(
    axis.text.x = element_text(size = 6),        
    strip.text.x = element_text(size = 4.5)    
  )

p4 = ggplot(bVals_BMIQ, aes(x = Sample_Time, y = VASN_cg00041083, group = Code_old)) +
  stat_smooth(aes(group = 1), method = "lm", se = TRUE) +
  stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 17, size = 3) +
  facet_grid(. ~ Sample_Group) +
  labs(x = "", y = "VASN cg00041083")  + 
  theme(
    axis.text.x = element_text(size = 6),        
    strip.text.x = element_text(size = 4.5)    
  )

p5 = ggplot(bVals_BMIQ, aes(x = Sample_Time, y = TSC2_cg26819590, group = Code_old)) +
  stat_smooth(aes(group = 1), method = "lm", se = TRUE) +
  stat_summary(aes(group = 1), geom = "point", fun = mean, shape = 17, size = 3) +
  facet_grid(. ~ Sample_Group) +
  labs(x = "", y = "TSC2 cg26819590")  + 
  theme(
    axis.text.x = element_text(size = 6),        
    strip.text.x = element_text(size = 4.5)    
  )

patchwork = p1 + p2 + p3 + p4 + p5 + plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A')

patchwork
