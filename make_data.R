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
                target=="Dutch occupation"|
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

#correct Indonesia
decolonization$Netherlands[decolonization$location=="Indonesia"]<-1
decolonization$ns5emp[decolonization$location=="Indonesia" & decolonization$year==1949] <- 1

#only include indepndent countries or countries that were part of european colonialism
decolonization<-filter(decolonization,Netherlands==1|Portugal==1|Spain==1|indepstate==1|French==1|British==1) %>% 
  select(-Yugoslavia,-Ottoman,-KKAustria,-SU,-Romanov)

#remove countries that were independent in 1945
independence_vector <- summarise(group_by(decolonization,location),always_ind=min(indepstate))
decolonization <- merge(decolonization,independence_vector) 
decolonization$always_ind[decolonization$location=="Indonesia"]<-0
decolonization<-filter(decolonization,always_ind==0) 

test<-filter(decolonization,location=="Indonesia")

rm(independence_vector)


#remove all observations after a country receives independence
decolonization <- decolonization[!is.na(decolonization$ns5emp),]

#correct Indonesia
decolonization$autonNS[decolonization$location=="Indonesia" & decolonization$year==1949]<-1
decolonization$autonNS[decolonization$location=="Indonesia" & decolonization$year==1950]<-100
decolonization<-filter(decolonization,autonNS!=100)

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

decolonization$camp_conf_intensity[is.na(decolonization$camp_conf_intensity)]<-0

#match with Easterly and Levine's settler population data
easterly_levine_pop <- read_csv("~/Google Drive/R Projects/decolonization/easterly_levine_pop.csv", 
                                col_types = cols(Blacks = col_integer(), 
                                                 Euroshare = col_number(), Others = col_integer(), 
                                                 Total = col_integer(), Whites = col_integer(), 
                                                 X10 = col_skip(), X4 = col_skip()))

easterly_levine_pop<-select(easterly_levine_pop,Country,Year,Whites,Euroshare) %>% filter(Year>1944)
easterly_levine_pop<-summarise(group_by(easterly_levine_pop,Country,Year),whites=mean(na.omit(Whites)),euroshare=mean(na.omit(Euroshare)))

colnames(easterly_levine_pop)[1:2]<-c("country","year")

decolonization <- merge(decolonization,easterly_levine_pop[,-3],all.x=TRUE)

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

#fix Algeria
decolonization$autonNS[decolonization$country=="Algeria" & decolonization$year=="1962"]<-1
decolonization <- cbind(as.numeric(decolonization$country=="Algeria" & decolonization$year==1963),decolonization)
colnames(decolonization)[1]<-"algeriafilter"
decolonization<-filter(decolonization,algeriafilter==0) %>% select(-algeriafilter)

#fix yemen
decolonization$autonNS[decolonization$country=="Yemen" & decolonization$year=="1962"] <- 0


#create dummy variable controlling for if the resistance switched from nonviolent to violent
unique(select(filter(decolonization,violent_resist==1),country))
unique(select(filter(decolonization,nonviolent_resist==1),country))

decolonization$switched_from_non_to_violent<-as.numeric(decolonization$country=="Algeria"|
                                                          decolonization$country=="Mozambique"|
                                                          decolonization$country=="Tunisia"|
                                                          decolonization$country=="Cyprus")
decolonization$switched_from_violent_to_non<-as.numeric(decolonization$country=="Cameroon"|
                                                          decolonization$country=="Cyprus")

#lag method of resistance

decolonization<-decolonization %>% group_by(country) %>% mutate(violent_resist.l1 = lag(violent_resist,n=1, order_by =year))
decolonization<-decolonization %>% group_by(country) %>% mutate(nonviolent_resist.l1 = lag(nonviolent_resist,n=1, order_by =year))
decolonization<-decolonization %>% group_by(country) %>% mutate(violent_resist.l2 = lag(violent_resist,n=2, order_by =year))
decolonization<-decolonization %>% group_by(country) %>% mutate(nonviolent_resist.l2 = lag(nonviolent_resist,n=2, order_by =year))



#include pop
library(readxl)
WPP2015_POP_F09_2_PERCENTAGE_OF_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_MALE <- read_excel("~/Google Drive/R Projects/decolonization/WPP2015_POP_F09_2_PERCENTAGE_OF_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_MALE.xls", 
                                                                                       col_types = c("blank", "text", "text", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "numeric", "numeric", "numeric", 
                                                                                                     "text", "text"))

decolonization$cown<-as.numeric(countrycode::countrycode(decolonization$country,"country.name","cown"))

WPP2015_POP_F09_2_PERCENTAGE_OF_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_MALE$cown<-as.numeric(countrycode::countrycode(WPP2015_POP_F09_2_PERCENTAGE_OF_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_MALE$`Country code`,"iso3n","cown"))
colnames(WPP2015_POP_F09_2_PERCENTAGE_OF_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_MALE)[4]<-"year"
WPP2015_POP_F09_2_PERCENTAGE_OF_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_MALE$year<-as.integer(WPP2015_POP_F09_2_PERCENTAGE_OF_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_MALE$year)

decolonization<-merge(decolonization,WPP2015_POP_F09_2_PERCENTAGE_OF_TOTAL_POPULATION_BY_BROAD_AGE_GROUP_MALE,all.x=TRUE)

#pop density

WPP2015_POP_F06_POPULATION_DENSITY <- read_excel("~/Google Drive/R Projects/decolonization/WPP2015_POP_F06_POPULATION_DENSITY.xls")

colnames(WPP2015_POP_F06_POPULATION_DENSITY)[1]<-"cown"
WPP2015_POP_F06_POPULATION_DENSITY$cown<-as.numeric(countrycode::countrycode(WPP2015_POP_F06_POPULATION_DENSITY$`Country code`,"iso3n","cown"))
WPP2015_POP_F06_POPULATION_DENSITY<-WPP2015_POP_F06_POPULATION_DENSITY[,-2]
WPP2015_POP_F06_POPULATION_DENSITY<-na.omit(WPP2015_POP_F06_POPULATION_DENSITY)
WPP2015_POP_F06_POPULATION_DENSITY$cown<-countrycode::countrycode(WPP2015_POP_F06_POPULATION_DENSITY$cown,"cown","country.name") 
WPP2015_POP_F06_POPULATION_DENSITY$cown<-as.factor(WPP2015_POP_F06_POPULATION_DENSITY$cown)

WPP2015_POP_F06_POPULATION_DENSITY<-reshape2::melt(WPP2015_POP_F06_POPULATION_DENSITY,ivar=cown)

WPP2015_POP_F06_POPULATION_DENSITY$cown<-countrycode::countrycode(WPP2015_POP_F06_POPULATION_DENSITY$cown,"country.name","cown")

colnames(WPP2015_POP_F06_POPULATION_DENSITY)<-c("cown","year","density")
WPP2015_POP_F06_POPULATION_DENSITY$year<-as.numeric(as.character(WPP2015_POP_F06_POPULATION_DENSITY$year))

decolonization<-merge(decolonization,WPP2015_POP_F06_POPULATION_DENSITY,all.x=TRUE)
#WPP2015_POP_F06_POPULATION_DENSITY<-reshape2::melt(WPP2015_POP_F06_POPULATION_DENSITY)

#interpolate
library(zoo)
decolonization<-decolonization[order(decolonization$country,decolonization$year),]

decolonization<-decolonization %>% 
 group_by(cown) %>% 
  mutate(age_imputed=na.approx(`18-23`,rule=2))

decolonization<-decolonization %>% 
  group_by(cown) %>% 
  mutate(density_imputed=na.approx(density,rule=2))

decolonization<-decolonization %>% 
  group_by(cown) %>% 
  mutate(euroshare_imputed=na.locf.default(euroshare,na.rm=FALSE))

