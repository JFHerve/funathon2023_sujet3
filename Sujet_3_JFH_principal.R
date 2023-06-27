# Lecture du fichier requirements.txt
requirements <- readLines("requirements_R.txt")

# Installation des packages
for (package in requirements) {
  install.packages(package)
}

library(aws.s3)
library(dplyr)
library(readr)
library(tidyverse)

bucket <- "projet-funathon"
path_data <- "2023/sujet3/diffusion"

description_indiv <- s3read_using(read_delim, object = paste(path_data, "description-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
habitudes_indiv <- s3read_using(read_delim, object = paste(path_data, "habitudes-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
actphys_sedent <- s3read_using(read_delim, object = paste(path_data, "actphys-sedent.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
fpq <- s3read_using(read_delim, object = paste(path_data, "fpq.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)


#pondérations
ponderations <- description_indiv %>%
  select(
    NOIND,
    pond_indiv_adu_pop2,
    pond_indiv_enf_pop1,
    region_adm_12cl,
    agglo_5cl,
    sex_PS,
    tage_PS,
    RUC_4cl,
    nbpers,
    etude_4cl_interv,
    diplome_interv,
    trav_nuit_2cl_interv,
    PCS_4cl_interv,
    vacances_interv,
    poids,
    taille,
    fume
  )

#join
act_phys <- actphys_sedent %>% 
  left_join (ponderations,by="NOIND") %>% 
  mutate_at(c('pond_indiv_adu_pop2','pond_indiv_enf_pop1'), ~replace_na(.,0)) %>% 
  mutate(pond=pond_indiv_adu_pop2+pond_indiv_enf_pop1,
         act_jardin=activite_jardiner_score+activite_tondre_score+activite_arroser_score+activite_becher_score)

#  mutate_at(c('pond_indiv_adu_pop2','pond_indiv_enf_pop1'), ~replace_na(.,0)) %>% 
#   replace(is.na(.), 0) %>% 
# 
#test
test <-act_phys %>% 
  select(pond_indiv_adu_pop2,pond_indiv_enf_pop1,pond)
view(test)

view(act_phys$activite_jardiner_score)
#activités physiques
#boxplot
res <-act_phys %>% 
  group_by(agglo_5cl) %>% 
  summarise(jardiner=mean(activite_jardiner_score), n=n(activite_jardiner_score), na.rm=TRUE)
res

res <- act_phys %>% 
  replace(is.na(.), 0) %>% 
  group_by(tage_PS,agglo_5cl) %>% 
  summarise(mean=mean(act_jardin)) %>% 
  mutate(mean=round(mean, digits=2)) %>% 
  spread(agglo_5cl,mean,fill=0)

