library(tidyverse)
library(readxl)

source("code/threat_functs.R")

path <- "data/Latin America/"
files <- dir(path, pattern = ".xlsx", all.files =  T)

i <- 4 # COLOMBIA
pattr <- "colombia"
dir.create(paste0("output/", pattr,"/"))

paste0(path, files[i]) %>% excel_sheets() # file sheet names
raw_data <- read_excel(path = paste0(path, files[i]), 
                        sheet = "Database", col_names = T) %>% 
  data.frame()

head(raw_data)
names(raw_data)

# summary data values
raw_data %>% 
  select(Threatens_Primates:Threatens_Primate_._Primate_Habitats_as_Consequence_Fragmentation) %>% 
  pivot_longer(cols = 1:5, names_to = "cols", values_to = "scores") %>% 
  pull(scores) %>% table()

# transform data to numbers and sum vals
data_tr <- 
  raw_data %>% select(Threats.to.Biodiversity:Region) %>% 
  mutate(thre_prim = rec_prim_val(Threatens_Primates),
         thre_habs = rec_prim_val(Threatens_Primate_Habitats),
         caus_frag = rec_prim_val(Causes_Fragmentation),
         exac_frag = rec_prim_val(Exacerbates_Fragmentation),
         thr_as_co = rec_prim_val(Threatens_Primate_._Primate_Habitats_as_Consequence_Fragmentation),
         cat_thr_n = parse_number(Threats.to.Biodiversity),
         cat_intge = factor(floor(cat_thr_n)),
         gen_threa = rec_thre_bio(cat_intge),
         Threats.to.Biodiversity = str_trim(Threats.to.Biodiversity, side = "both")) 

data_tr <- data_tr %>% rowwise() %>% 
  mutate(cond_sum = sum(c_across(thre_prim:thr_as_co)))

# threat ranking ####
var <- "Region" #"Forest_Type" # "Region" #
cat2filt <- data_tr %>% pull(var) %>% unique()

dir.create(paste0("output/", pattr,"/", pattr, "_", var))
for(i in 1:length(cat2filt)){
  data_tr_ss <- data_tr %>% filter(get(var) == cat2filt[i])
  # variability metrics per assessor
  assessor_mets <- 
    data_tr_ss %>% select(Threats.to.Biodiversity, Assessor,
                          thre_prim:thr_as_co, cond_sum) %>% 
    filter(Assessor == "A3") %>%
    group_by(Assessor) %>% 
    summarise(cond_mean = mean(cond_sum, na.rm = T), cond_sd = sd(cond_sum, na.rm = T))
  
  # varibaility metrics per general threat
  threat_mets <- 
    data_tr_ss %>% select(Threats.to.Biodiversity, Assessor,
                          thre_prim:thr_as_co, cond_sum, gen_threa) %>% 
    filter(Assessor == "A3") %>% filter(!is.na(gen_threa)) %>% 
    group_by(gen_threa) %>% 
    summarise(cond_mean = mean(cond_sum, na.rm = T), cond_sd = sd(cond_sum, na.rm = T))
  
  # conditional standardized values
  condit_tab <- 
    data_tr_ss %>% filter(Assessor == "A3") %>%
    select(Threats.to.Biodiversity, Assessor, 
           thre_prim:thr_as_co, cond_sum, gen_threa) %>% 
    group_by(Assessor) %>% 
    mutate(cond_mean = mean(cond_sum, na.rm = T), cond_sd = sd(cond_sum, na.rm = T), 
           norm_val = (cond_sum - cond_mean)/cond_sd)
  
  # to check mean = 0, sd = 1
  condit_tab %>% summarise(mean(norm_val, na.rm = T), sd(norm_val, na.rm = T))
  
  # threat detailed
  res_tab <- 
    condit_tab %>% ungroup() %>% 
    select(Threats.to.Biodiversity, Assessor, thre_prim:thr_as_co, cond_sum:norm_val) %>% 
    group_by(Threats.to.Biodiversity) %>% 
    mutate(score = mean(norm_val, na.rm = T)) %>%
    distinct(Threats.to.Biodiversity, .keep_all = T) %>% 
    arrange(desc(score)) %>% filter(!is.na(score))
  
  if(dim(res_tab)[1]==0){next(); print(paste0("empty table for: ", cat2filt[i]))}
  
  res_tab %>% 
    write.csv(file = paste0(paste0("output/", pattr,"/", pattr, "_", var,"/"), 
                            "res_tab_", cat2filt[i],"_detail.csv"))
  
  # thread general
  condit_tab %>% ungroup() %>% 
    select(Threats.to.Biodiversity, Assessor, thre_prim:thr_as_co, cond_sum:norm_val, gen_threa) %>% 
    group_by(gen_threa) %>% 
    mutate(score = mean(norm_val, na.rm = T)) %>%
    distinct(gen_threa, .keep_all = T) %>% 
    arrange(desc(score)) %>% filter(!is.na(score)) %>% 
    write.csv(file = paste0(paste0("output/", pattr,"/", pattr, "_", var,"/"), 
                            "res_tab_", cat2filt[i],"_general.csv"))
  
}
