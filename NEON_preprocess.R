library(lubridate)
library(plyr)

## Create mosquito data
setwd("C:/Users/Amanda B. Young/Documents/R/mosquitoes/mosquito_count")
filenames <- list.files(pattern="*.csv", full.names=FALSE)
ldf <- lapply(filenames, read.csv, header=TRUE)
names(ldf) <- filenames
for(i in filenames) 
  ldf[[i]]$Source = i
All_M<-plyr::ldply(ldf, rbind)


All_MM<-All_M %>% 
  select(domainID, siteID, plotID, collectDate, individualCount,family,genus, scientificName, sex) %>% 
  separate(collectDate, into = c("Year", "Month", "Day","Time"), sep ="[-,T]") %>% 
  unite("date2", c(Year, Month, Day), sep = "-", remove = FALSE) %>%   
  mutate(date2 = as.Date(date2),
         doy = yday(date2),
         mutate(Year=as.numeric(Year)))%>%
  group_by(siteID,Year, doy)


#All_mos<-All_MM %>% 
#  mutate(Year = as.numeric(Year),
#         doy = as.numeric(doy),
#        individualCount = as.numeric(individualCount))

write.csv(All_MM, "C:/Users/Amanda B. Young/Documents/R/mosquitoes/All_Mos.csv")
