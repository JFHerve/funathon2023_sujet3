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


# Import des librairies
library(ggplot2)
library(ggcorrplot)
library(sf)
# Option d'affichage
options(dplyr.width = Inf)
options(repr.plot.width=20, repr.plot.height=10)

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
  replace(is.na(.), 0) %>% 
  mutate(pond=pond_indiv_adu_pop2+pond_indiv_enf_pop1,
         act_jardin=activite_jardiner_score+activite_tondre_score+activite_arroser_score+activite_becher_score) %>% 
  mutate(categorie_agglo = case_when(agglo_5cl==1 ~ "a-Rural",
                                     agglo_5cl==2 ~ "b-2000 - 19 999 hab",
                                     agglo_5cl==3 ~ "c-20 000 - 99 999 hab",
                                     agglo_5cl==4 ~ "d-+ 100 000 hab",
                                     agglo_5cl==5 ~ "e-Paris"))

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

#graph
# Recodage de la variable de type d'agglomération

ponderations <- ponderations %>% mutate(categorie_agglo = case_when(agglo_5cl==1 ~ "a-Rural",
                                                                              agglo_5cl==2 ~ "b-2000 - 19 999 hab",
                                                                              agglo_5cl==3 ~ "c-20 000 - 99 999 hab",
                                                                              agglo_5cl==4 ~ "d-+ 100 000 hab",
                                                                              agglo_5cl==5 ~ "e-Paris"))


counts_agglo <- act_phys %>% group_by(categorie_agglo) %>% 
  summarise(n=n(),nbpond=sum(pond))
act_phys %>% 
  summarise(n=n(),nbpond=sum(pond))

tage <- act_phys %>% group_by(tage_PS) %>% 
  filter(tage_PS >=7) %>% 
  replace(is.na(.), 0) %>% 
  summarise(n=n(),nbpond=sum(pond),act_jardinmean=mean(act_jardin))
act_phys %>% 
  summarise(n=n(),nbpond=sum(pond))

#histogrammes
# Générer le graphique en barres horizontales
ggplot(data=counts_agglo,aes(x=categorie_agglo,y=n))+
  geom_histogram(stat="identity")+
  coord_flip()+ 
  labs(title="Histogramme des types d'agglomération",
       x="Type d'agglomération",
       y="Nombre d'individus")

#ne fonctionne pas
ggplot(data=counts_agglo,aes(x=categorie_agglo,y=n,weight=nbpond))+
  geom_histogram(stat="identity")+
  coord_flip()+ 
  labs(title="Histogramme des types d'agglomération",
       x="Type d'agglomération",
       y="Nombre d'individus")

ggplot(data=counts_agglo,aes(x=categorie_agglo,y=nbpond))+
  geom_histogram(stat="identity")+
  coord_flip()+ 
  labs(title="Histogramme des types d'agglomération",
       x="Type d'agglomération",
       y="Nombre d'individus")

#boxplots

act_phys2 <- act_phys %>% 
  replace(is.na(.), 0) %>% 
  select(RUC_4cl, tage_PS, categorie_agglo, act_jardin,
         activite_jardiner_score,activite_tondre_score,
         activite_arroser_score,activite_becher_score) %>% 
  mutate(tage_PS=as.character(tage_PS),
         RUC_4cl=as.character(RUC_4cl))

e <- ggplot(act_phys, aes(x = categorie_agglo, y = act_jardin))
e + geom_boxplot()

e <- ggplot(act_phys2, aes(x = tage_PS, y = act_jardin))
e + geom_boxplot() 

e <- ggplot(act_phys2, aes(x = RUC_4cl, y = act_jardin))
e + geom_boxplot() 


#correlations
df_num <- act_phys %>% select(where(is.numeric))

df_num <- df_num %>% select(c("RUC_4cl", "tage_PS", "agglo_5cl", "act_jardin",
         "activite_jardiner_score","activite_tondre_score",
         "activite_arroser_score","activite_becher_score"))
matrice_correlation <- model.matrix(~0+., data=df_num) %>% 
  cor(use="pairwise.complete.obs")
matrice_correlation %>%   ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=7)

df_num2 <- df_num %>% 
  filter(tage_PS >=7)
matrice_correlation2 <- model.matrix(~0+., data=df_num2) %>% 
  cor(use="pairwise.complete.obs")
matrice_correlation2 %>%
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=7)
