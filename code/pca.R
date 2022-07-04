# PCA #

# build general matrix
mat_thr <- 
  data_tr %>% filter(Assessor == "A3") %>% ungroup() %>% 
  select(Threats.to.Biodiversity, thre_prim:thr_as_co, gen_threa) %>% 
  filter(thre_prim != "", Threats.to.Biodiversity != "") %>% 
  group_by(gen_threa) %>% 
  summarise(thre_prim_s = mean(thre_prim, na.rm = T), 
            thre_hab_s  = mean(thre_habs, na.rm = T),
            caus_frag_s = mean(caus_frag, na.rm = T), 
            exac_frag_s = mean(exac_frag, na.rm = T), 
            thr_as_co_s = mean(thr_as_co, na.rm = T))

tt_r <- filter(mat_thr, !is.na(gen_threa))[,-1] %>% data.frame %>% t()
colnames(tt_r) <- filter(mat_thr, !is.na(gen_threa))[,1] %>% data.frame() %>% pull()
row.names(tt_r) <- c("Threatens_Primates", "Threatens_Primate_Habitats", "Causes_Fragmentation", 
                     "Exacerbates_Fragmentation", "Primate_Habitats_as_Consequence_Fragmentation")

# specific matrix
data_tr <- data_tr %>% filter(Assessor == "A3") %>% 
  mutate(thr2bio = gsub("*\\d.", "", Threats.to.Biodiversity)) 

mat_thr <- 
  data_tr %>% filter(Assessor == "A3") %>% ungroup() %>%
  filter(thre_prim != "", Threats.to.Biodiversity != "") %>% 
  select(thr2bio, thre_prim:thr_as_co) %>% 
  group_by(thr2bio)

tt_r <- filter(mat_thr, !is.na(thr2bio))[,-1] %>% data.frame %>% t()
colnames(tt_r) <- filter(mat_thr, !is.na(thr2bio))[,1] %>% data.frame() %>% pull() %>% 
  gsub(pattern = " ", replacement = "_", x = .)
row.names(tt_r) <- c("Threatens_Primates", "Threatens_Primate_Habitats", "Causes_Fragmentation", 
                     "Exacerbates_Fragmentation", "Primate_Habitats_as_Consequence_Fragmentation")

# run pca
library(vegan); library(factoextra); library(FactoMineR)
pca_res <- PCA(tt_r, graph = T)

fviz_pca_biplot(pca_res, col.var = "cos2", #col.ind = "contrib", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = T) + theme_classic()

fviz_eig(pca_res, addlabels = T)
pca_vars <- get_pca_var(pca_res)

fviz_pca_var(pca_res, col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T) + theme_classic()

fviz_contrib(pca_res, choice = "var", axes = 1, top = 10) # Vars contributions to PC1
fviz_contrib(pca_res, choice = "var", axes = 2, top = 10) # Vars contributions to PC2

fviz_contrib(pca_res, choice = "var", axes = 1:2, top = 10) # Vars contributions to PC1&2

# pca plot most important contrubuting vars
fviz_pca_var(pca_res, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             alpha.var = "contrib",
             repel = T) + theme_classic()

