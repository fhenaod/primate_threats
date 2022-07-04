# to transform categorical evals to numbers 
rec_prim_val=function(x){
  l = 1 ; m = 2; h = 3 # L = 1, M = 2, H = 3
  x %>% 
    recode(L = l, M = m, H = h, 
           "H, M" = mean(c(h, m)), "M, H" = mean(c(h, m)), "Yes [M,H]" = mean(c(h, m)),
           "M, L" = mean(c(m, l)), "L, M" = mean(c(m, l)), "M,L" = mean(c(m, l)),
           "H, L" = mean(c(h, l)), "L, H" = mean(c(h, l)), "H,L" = mean(c(h, l)),
           "H, M, L" = mean(c(h, m, l)),
           "No" = 0, "Unknown" = 0, "Expected" = 0, "Uncertain" = 0
           #, .missing = NA
    ) 
}

#  to transform numerical threats to character names
rec_thre_bio=function(x){
  x %>% 
    recode("1" = "Residentian_Commercial",  "2" = "Agriculture_Aquaculture", "3" = "Energy",
           "4" = "Transportation_Service", "5" = "Biological_use", "6" = "Human_intrusion",
           "7" = "Natural_modifications", "8" = "Invasive_species", "9" = "Pollution", 
           "10" = "Geology", "11" = "Climate_change"
    ) 
}