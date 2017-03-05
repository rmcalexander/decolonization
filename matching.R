library(MatchIt)
matchdf <- dplyr::select(decolonization,autonNS,violent_resist,density_imputed,settler_pop,nsf5neigh,rr_std,csyear1,British,French,africa,middleeast,lamerica,Netherlands,nonviolent_resist) %>% na.omit() %>% as.data.frame()
m.out <- matchit(violent_resist ~ density_imputed  + settler_pop + nsf5neigh + rr_std + csyear1 + British 
                 + French + africa +middleeast + lamerica+Netherlands+nonviolent_resist, 
                 data=matchdf,
                 method = "nearest")
summary(m.out)

m.data <- MatchIt::match.data(m.out)

t.test(m.data$autonNS[m.data$violent_resist==1],m.data$autonNS[m.data$violent_resist==0],paired=TRUE)

mean(m.data$autonNS[m.data$violent_resist==1])
mean(m.data$autonNS[m.data$violent_resist==0])
