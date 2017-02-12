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

#need to match on location
unique(navco$location)[!(unique(navco$location) %in% unique(WimmerFeinsteinReplication$country))]
navco$location[navco$location=="Guinea-Biaasu"] <- "Guinea-Bissau"
navco$location[navco$location=="Palestinian Territories"] <- "Israel"
WimmerFeinsteinReplication$country[WimmerFeinsteinReplication$country=="Northern Ireland"] <- "Ireland"


colnames(WimmerFeinsteinReplication)[2] <- "location"
WimmerFeinsteinReplication <- select(WimmerFeinsteinReplication,-cowcode)
navco <- select(navco,-cowcode,-tccode)

#merge
decolonization<-merge(WimmerFeinsteinReplication,navco,all.x=TRUE)

#only include indepndent countries or countries that were part of european colonialism
decolonization<-filter(decolonization,Netherlands==1|Portugal==1|Spain==1|indepstate==1|French==1|British==1) %>% 
  select(-Yugoslavia,-Ottoman,-KKAustria,-SU,-Romanov)

#remove countries that were independent in 1945
independence_vector <- summarise(group_by(decolonization,location),always_ind=min(indepstate))
decolonization <- merge(decolonization,independence_vector) %>% 
  filter(always_ind==0) 

rm(independence_vector)

#remove all observations after a country receives independence
decolonization <- decolonization[!is.na(decolonization$ns5emp),]

#create vectors indicating if a resistance movement was active
decolonization$any_resist <- as.numeric(!is.na(decolonization$resis_meth))
decolonization$violent_resist <- as.numeric(decolonization$prim_method == 0)
decolonization$violent_resist[is.na(decolonization$violent_resist)] <- 0

decolonization$nonviolent_resist<-as.numeric(decolonization$prim_method == 1)
decolonization$nonviolent_resist[is.na(decolonization$nonviolent_resist)] <- 0

decolonization$in_media[is.na(decolonization$in_media)]<-0
decolonization$ab_inter_con[is.na(decolonization$ab_inter_con)]<-0
decolonization$ab_inter_con[decolonization$ab_inter_con==(-99)]<-0

decolonization<-filter(decolonization,year<1976)

decolonization$ab_inter_con[is.na(decolonization$ab_inter_con)] <- 0
decolonization$ab_domestic_con[is.na(decolonization$ab_domestic_con)] <- 0
decolonization$ab_inter_reper[is.na(decolonization$ab_inter_reper)] <- 0

decolonization$regime_support[decolonization$regime_support==-99]<-"NA"
colnames(decolonization)[1]<-"country"

#match with Easterly and Levine's settler population data
easterly_levine_pop <- read_csv("~/Google Drive/R Projects/decolonization/easterly_levine_pop.csv", 
                                col_types = cols(Blacks = col_integer(), 
                                                 Euroshare = col_number(), Others = col_integer(), 
                                                 Total = col_integer(), Whites = col_integer(), 
                                                 X10 = col_skip(), X4 = col_skip()))

easterly_levine_pop<-select(easterly_levine_pop,Country,Year,Whites,Euroshare) %>% filter(Year>1944)
easterly_levine_pop<-summarise(group_by(easterly_levine_pop,Country,Year),whites=mean(na.omit(Whites)),euroshare=mean(na.omit(Euroshare)))

colnames(easterly_levine_pop)[1:2]<-c("country","year")

easterly_levine_pop<-(summarise(group_by(easterly_levine_pop,country),settler_pop=sum(na.omit(whites))))

decolonization <- merge(decolonization,easterly_levine_pop,all.x=TRUE)
decolonization <- select(decolonization,-contains("Itime"))
decolonization$settler_pop[is.na(decolonization$settler_pop)]<-0
decolonization$settler_pop <- log(decolonization$settler_pop+1)

#fix morocco
decolonization$autonNS[decolonization$country=="Morocco" & decolonization$year=="1956"]<-1
decolonization<-cbind(as.numeric(decolonization$country=="Morocco" & decolonization$year>1956),decolonization)
colnames(decolonization)[1]<-"moroccofilter"
decolonization<-filter(decolonization,moroccofilter==0) %>% select(-moroccofilter)


#fix bahrain
decolonization$autonNS[decolonization$country=="Bahrain" & decolonization$year=="1971"]<-1
decolonization <- cbind(as.numeric(decolonization$country=="Bahrain" & decolonization$year>1971),decolonization)
colnames(decolonization)[1]<-"bahrainfilter"
decolonization<-filter(decolonization,bahrainfilter==0) %>% select(-bahrainfilter)

#fix kuwait
decolonization$autonNS[decolonization$country=="Kuwait" & decolonization$year=="1961"]<-1
decolonization <- cbind(as.numeric(decolonization$country=="Kuwait" & decolonization$year>1961),decolonization)
colnames(decolonization)[1]<-"kuwaitfilter"
decolonization<-filter(decolonization,kuwaitfilter==0) %>% select(-kuwaitfilter)

#remove eritrea and zimbabwe and United Arab Emirates
decolonization<-filter(decolonization,country!="Eritrea",country!="Zimbabwe",country!="United Arab Emirates")

#fix libya
decolonization$autonNS[decolonization$country=="Libya" & decolonization$year=="1951"]<-1
decolonization <- cbind(as.numeric(decolonization$country=="Libya" & decolonization$year>1951),decolonization)
colnames(decolonization)[1]<-"libyafilter"
decolonization<-filter(decolonization,libyafilter==0) %>% select(-libyafilter)

