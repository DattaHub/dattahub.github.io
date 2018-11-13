rm(list = ls())

library(ggmap)
setwd("C:/Users/jd033/Google Drive/LittleRock_2015_PredictionProjects/CrimeData/2017")
crime.gc <- read.csv(file="2017_LR_Crime.csv",header=T,sep=",",na.strings='NULL')


qmplot(LONGITUDE, LATITUDE, data = crime.gc, maptype = "toner-lite", color = I("red"))

library(dplyr)
agg.gc <- crime.gc %>% filter(OFFENSE_DESCRIPTION == "AGGRAVATED ASSAULT")

qmplot(LONGITUDE, LATITUDE, data = agg.gc,
       geom = "blank", maptype = "toner-lite", legend = "right") + stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .5, color = NA) +
  scale_fill_gradient2("Homicide\nPropensity", low = "white", mid = "yellow", high = "red", midpoint = 60)

crime.gc$OFFENSE_DESCRIPTION <- plyr::revalue(crime.gc$OFFENSE_DESCRIPTION, 
                                     c("MOTOR VEHICLE THEFT"="THEFT-MOTOR", 
                                       "THEFT FROM MOTOR VEHICLE"= "THEFT-MOTOR",
                                       "THEFT FROM BUILDING" = "OTHER-THEFT",
                                       "THEFT OF MOTOR VEHICLE PARTS" = "THEFT-MOTOR",
                                       "ALL OTHER LARCENY" = "OTHER-THEFT"))

library(dplyr)
violent.gc <- crime.gc %>%
  filter(OFFENSE_DESCRIPTION %in% c("AGGRAVATED ASSAULT",
                                    "BURGLARY/B&E",
                                    "THEFT-MOTOR",
                                    "OTHER-THEFT"))

qmplot(LONGITUDE, LATITUDE, data = violent.gc,
       geom = "blank", maptype = "toner-lite", legend = "right") + 
  stat_density_2d(aes(x = LONGITUDE, y = LATITUDE, fill = ..level..,alpha = .25), geom = "polygon", color = NA) +
  # scale_fill_gradient2("Violent Crimes\nPropensity", low = "white", mid = "yellow", high = "red", midpoint = 60)+
  facet_wrap(~OFFENSE_DESCRIPTION)

library(lubridate)
# str(crime.gc)
crime.gc$INCIDENT_DATE <- as.character(crime.gc$INCIDENT_DATE)
crime.gc$INCIDENT_DATE <- as.Date(crime.gc$INCIDENT_DATE,format="%m/%d/%Y")
crime.gc$IncidentWeek <- lubridate::week(ymd(crime.gc$INCIDENT_DATE))

library(dplyr)

crime.ct <- crime.gc %>% group_by(OFFENSE_DESCRIPTION, IncidentWeek) %>%
  dplyr::summarise(count = n()) 

## All crime plots 

library(ggplot2)
crime.plot <- ggplot(crime.ct, aes(x = IncidentWeek, y = count, group = OFFENSE_DESCRIPTION, colour = OFFENSE_DESCRIPTION))+
  geom_line() +ylab("Incident Count")+
  xlab("Weeks") + #+theme_bw()+
  # theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  #facet_grid(OffenseDes~.,scales="free_y")+
  theme(legend.position="bottom")
crime.plot <- crime.plot + theme(axis.title.y = element_text(size = rel(1), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1)))
crime.plot<- crime.plot+ theme(axis.text = element_text(size = rel(1)))
crime.plot <- crime.plot+theme(strip.text.x = element_text(size=12, face="bold"),strip.text.y = element_text(size=12, face="bold"))
print(crime.plot)


violent.ct <- crime.ct %>%
  filter(OFFENSE_DESCRIPTION %in% c("AGGRAVATED ASSAULT","MURDER & NONNEGLIGENT MANSLAUGHTER","RAPE","ROBBERY"))

library(ggplot2)
crime.plot <- ggplot(violent.ct, aes(x = IncidentWeek, y = count, group = OFFENSE_DESCRIPTION, colour = OFFENSE_DESCRIPTION))+
  geom_line() +ylab("Incident Count")+
  xlab("Weeks") + #+theme_bw()+
  # theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  #facet_grid(OffenseDes~.,scales="free_y")+
  theme(legend.position="bottom")
crime.plot <- crime.plot + theme(axis.title.y = element_text(size = rel(1), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1)))
crime.plot<- crime.plot+ theme(axis.text = element_text(size = rel(1)))
crime.plot <- crime.plot+theme(strip.text.x = element_text(size=12, face="bold"),strip.text.y = element_text(size=12, face="bold"))
print(crime.plot)

library(changepoint)
agg.ct <- crime.ct %>%
  filter(OFFENSE_DESCRIPTION == "AGGRAVATED ASSAULT")

# results <- cpt.mean(agg.ct$count,penalty = "BIC", method="PELT")
results <- cpt.mean(agg.ct$count,penalty="None",method="AMOC")
cat("Location of change-points \n", cpts(results))

par(mfrow=c(1,1))
plot(results,cpt.col="blue",xlab="Week",ylab="Incident Counts", main =  "Agg-Assault", cpt.width=4)

# library(changepoint)
# murder.ct <- filter(crime.ct, OFFENSE_DESCRIPTION == "BURGLARY/B&E")
# # results <- cpt.mean(robbery.ct$count,penalty = "BIC", method="PELT")
# results <- cpt.mean(murder.ct$count,penalty="None",method="AMOC")
# cat("Location of change-points \n", cpts(results))
# plot(results,cpt.col="blue",xlab="Week",ylab="Incident Counts", main =  "Agg-Assault-Family", cpt.width=4)


library(dplyr)
crime.dis <- violent.gc %>% group_by(OFFENSE_DESCRIPTION, LOCATION_DISTRICT) %>%
  dplyr::summarise(ccount = n()) 

ggplot(crime.dis, aes(x = reorder(LOCATION_DISTRICT , -ccount), y = ccount, group = OFFENSE_DESCRIPTION)) + 
  geom_bar(stat="identity", position = position_stack(reverse = TRUE))+
  coord_flip() + scale_x_discrete(breaks = unique(crime.dis$LOCATION_DISTRICT),
                                  labels = unique(crime.dis$LOCATION_DISTRICT))+
  facet_wrap(~OFFENSE_DESCRIPTION)

library(dplyr)

agg.gc$INCIDENT_DATE <- as.character(agg.gc$INCIDENT_DATE)
agg.gc$INCIDENT_DATE <- as.Date(agg.gc$INCIDENT_DATE,format="%m/%d/%Y")
agg.gc$IncidentWeek <- lubridate::week(ymd(agg.gc$INCIDENT_DATE))

agg.ct <- crime.gc %>% filter(LOCATION_DISTRICT %in% c(52,64,81,54)) %>% 
  group_by(LOCATION_DISTRICT, IncidentWeek) %>% dplyr::summarise(count = n()) 

library(ggplot2)
crime.plot <- ggplot(agg.ct, aes(x = IncidentWeek, y = count, group = LOCATION_DISTRICT))+
  geom_line() +ylab("Incident Count")+ geom_vline(xintercept = 33) +
  xlab("Weeks") + #+theme_bw()+
  # theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  facet_grid(LOCATION_DISTRICT ~., scales="free")+
  theme(legend.position="bottom")
crime.plot <- crime.plot + theme(axis.title.y = element_text(size = rel(1), angle = 90))+
  theme(axis.title.x = element_text(size = rel(1)))
crime.plot<- crime.plot+ theme(axis.text = element_text(size = rel(1)))
crime.plot <- crime.plot+theme(strip.text.x = element_text(size=12, face="bold"),strip.text.y = element_text(size=12, face="bold"))
print(crime.plot)

top.districts <- c(52,64,81,54)
par(mfrow=c(2,2))
for (i in 1:4){
  dis.id <- top.districts[i]
  crime.top <- crime.gc %>% filter(LOCATION_DISTRICT == dis.id, OFFENSE_DESCRIPTION =="AGGRAVATED ASSAULT") %>% group_by(LOCATION_DISTRICT, IncidentWeek) %>% dplyr::summarise(count = n()) 
  results <- cpt.mean(crime.top$count,penalty="None",method="AMOC")
  cat("The Location of change-point for district ", dis.id, "is week ", cpts(results), "\n")
  plot(results, cpt.col="blue", lty=2, xlab="Week",ylab="Incident Counts", main =  paste("District",dis.id), cpt.width=4)
}
par(mfrow=c(1,1))

library(dplyr)
districts <- crime.gc %>% filter(LOCATION_DISTRICT %in% c(54,64,81,52))

qmplot(LONGITUDE, LATITUDE, data = districts,
       maptype = "toner-lite", geom = "blank", legend = "right", darken = 0.3) +   
  stat_density_2d(aes(fill = ..level..), alpha = 0.2, geom = "polygon", color = NA) + 
  scale_fill_gradient2("Homicide\n Propensity",low = "white", mid = "yellow", high = "red", midpoint = 60) + 
  facet_wrap(~LOCATION_DISTRICT) 



crime.64 <- crime.gc %>% filter(LOCATION_DISTRICT == 64)%>% group_by(OFFENSE_DESCRIPTION, LOCATION_ADDRESS)%>% dplyr::summarise(count = n()) 
crime.64 <- crime.64 %>% filter(count>5) %>% filter(OFFENSE_DESCRIPTION %in% c("AGGRAVATED ASSAULT","THEFT-MOTOR","OTHER-THEFT","SHOPLIFTING"))

# qmplot(LONGITUDE, LATITUDE, data = crime.64,
#        maptype = "toner-lite", legend = "right") +   
#   # stat_density_2d(aes(fill = ..level..), alpha = 0.2, geom = "polygon", color = NA) + 
#   # scale_fill_gradient2("Homicide\n Propensity",low = "white", mid = "yellow", high = "red", midpoint = 60) + 
#   facet_wrap(~OFFENSE_DESCRIPTION) 

ggplot(crime.64, aes(x = reorder(LOCATION_ADDRESS , -count), y = count, group = OFFENSE_DESCRIPTION)) + 
  geom_bar(stat="identity", position = position_stack(reverse = TRUE))+
  coord_flip() + scale_x_discrete(breaks = unique(crime.64$LOCATION_ADDRESS),
                                  labels = unique(crime.64$LOCATION_ADDRESS))+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=14,face="bold"))+
 facet_wrap(~OFFENSE_DESCRIPTION)

ggsave("LR_2017_counts_overall.png", height = 9, width = 16)


## ADDRESS SPECIFIC

crime.walmart <- crime.64%>%filter(LOCATION_ADDRESS=="2700 S SHACKLEFORD RD")

top.address <- c("9601 BAPTIST HEALTH DRIVE","11400 W MARKHAM ST",
                 "3400 S BOWMAN ROAD","2700 S SHACKLEFORD RD")
addresses <- crime.gc%>%filter(LOCATION_ADDRESS=="2700 S SHACKLEFORD RD")
# %>% filter(LOCATION_ADDRESS %in% top.address)
# %>% group_by(OFFENSE_DESCRIPTION, LONGITUDE, LATITUDE, LOCATION_ADDRESS)%>% dplyr::summarise(count = n()) 

qmplot(LONGITUDE, LATITUDE, data = addresses,
       maptype = "toner-lite", geom = "blank", legend = "right") +   
  stat_density_2d(aes(fill = ..level..), alpha = 0.2, geom = "polygon", color = NA) + 
  scale_fill_gradient2("Homicide\n Propensity",low = "white", mid = "yellow", high = "red", midpoint = 10) + 
  facet_wrap(~LOCATION_ADDRESS)
