#LOAD PACKAGES
library(tidyverse)
library(haven)
library(readr)

#Load data
WimmerFeinsteinReplication <- read_dta("~/Google Drive/R Projects/decolonization/WimmerFeinsteinReplication.dta")
navco <- read_csv("~/Google Drive/R Projects/decolonization/navco.csv")

#Format for merge
colnames(navco)[5]<-"cowcode"
WimmerFeinsteinReplication$cowcode <- as.integer(WimmerFeinsteinReplication$cowcode)
WimmerFeinsteinReplication$year <- as.integer(WimmerFeinsteinReplication$year)
WimmerFeinsteinReplication <- filter(WimmerFeinsteinReplication,year>1944)

# Remove all non-anti-occupation resistance movements
navco<-filter(navco,target=="Soviet occupation"|
                target=="French occupation"|
                target=="British Rule"|
                target=="British rule"|
                target=="Spanish occupation"|
                target=="British occupation"|
                target=="UK occupation"|
                target=="Portuguese occupation"|
                target=="Dutch occpuation"|
                target=="French/Spanish occupation"|
                target=="Belgian occupation"|
                target=="British and Aden administration"
              )
unique(navco$target)

#merge
decolonization<-merge(WimmerFeinsteinReplication,navco,all.x=TRUE)

#only include indepndent countries or countries that were part of european colonialism
decolonization<-filter(decolonization,Netherlands==1|Portugal==1|Spain==1|indepstate==1|French==1|British==1) %>% 
  select(-Yugoslavia,-Ottoman,-KKAustria,-SU,-Romanov)

#remove countries that were independent in 1945
independence_vector <- summarise(group_by(decolonization,cowcode),always_ind=min(indepstate))
decolonization <- merge(decolonization,independence_vector) %>% 
  filter(always_ind==0) 

rm(independence_vector)

#remove all observations after a country receives independence
decolonization <- decolonization[!is.na(decolonization$ns5emp),]

#create vectors indicating if a resistance movement was active
decolonization$any_resist <- as.numeric(!is.na(decolonization$resis_meth))
decolonization$violent_resist <- as.numeric(decolonization$resis_meth == 0)
decolonization$violent_resist[is.na(decolonization$violent_resist)] <- 0

decolonization$nonviolent_resist<-as.numeric(decolonization$resis_meth > 0)
decolonization$nonviolent_resist[is.na(decolonization$nonviolent_resist)] <- 0

decolonization<-filter(decolonization,year<1976)
