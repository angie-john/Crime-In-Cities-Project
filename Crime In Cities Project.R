library(ggplot2)
library(dplyr)
library(tidyverse)
setwd("/Users/angelina/Documents/stat's project")

san_fran <- read.csv("san_francisco.csv")
dallas_5000 <- read.csv("dallas_5000.csv")
#reading data from both files

san_fran_sub <- subset(san_fran, Incident.Year <= 2022)
#subsetting sanfrancisco data set



san_fran_sub2 <- subset(san_fran_sub, Incident.Category != "Non-Criminal" & 
                          Incident.Category != "Lost Property" & 
                          Incident.Category != "Recovered Vehicle" &
                          Incident.Category != "Other Offenses" & 
                          Incident.Category != "Missing Person" & 
                          Incident.Category != "Warrant" & 
                          Incident.Category != "Suspicious Occ" &
                          Incident.Category != "Fire Report" &
                          Incident.Category != "Case Closure" &
                          Incident.Category != "Courtesy Report" &
                          Incident.Category != "Suicide" &
                          Incident.Category != "Other"  &
                          Incident.Category != "Miscellaneous Investigation" &
                          Incident.Category != "" &
                          Police.District != "Out of SF", 
                          select = c(Incident.Date, Incident.Year, 
                                     Incident.Day.of.Week, Incident.Category, 
                                     Incident.Description, Police.District, 
                                     Incident.Time))
#cleaning data

set.seed(10)
san_fran_5000 <- san_fran_sub2[sample(nrow(san_fran_sub2), 
                                      size = 5000), ]
#taking random sample of 5000
san_fran_5000$type <- " "
san_fran_5000$type[which(san_fran_5000$Incident.Category == "Assault" |
                         san_fran_5000$Incident.Category == "Robbery" |
                         san_fran_5000$Incident.Category == "Disorderly Conduct" |
                         san_fran_5000$Incident.Category == "Weapons Offense" |
                         san_fran_5000$Incident.Category == "Weapons Carrying Etc" |
                         san_fran_5000$Incident.Category == "Homicide" |
                         san_fran_5000$Incident.Category == "Rape" |
                         san_fran_5000$Incident.Category == "Sex Offense"
                         )] <- "Violent"
#cleaning data -> classifying violent crimes
san_fran_5000$type[which(san_fran_5000$Incident.Category != "Assault" &
                           san_fran_5000$Incident.Category != "Robbery" &
                           san_fran_5000$Incident.Category != "Disorderly Conduct" &
                           san_fran_5000$Incident.Category != "Weapons Offense" &
                           san_fran_5000$Incident.Category != "Weapons Carrying Etc" &
                           san_fran_5000$Incident.Category != "Homicide" &
                           san_fran_5000$Incident.Category != "Rape" &
                           san_fran_5000$Incident.Category != "Sex Offense" 
)] <- "Property"
#cleaning data -> classifying property crimes
san_fran_5000$type[which(san_fran_5000$Incident.Description == 
                           "Kidnapping (Adult victim)"
                           )] <- "Violent"
san_fran_5000$type <- factor(san_fran_5000$type, levels = c("Violent", "Property"))

san_fran_5000$violent <- ""
san_fran_5000$violent[which(san_fran_5000$type == "Violent")] <- TRUE
san_fran_5000$violent[which(san_fran_5000$type != "Violent")] <- FALSE
#boolean to hold if a crime is violent
san_fran_5000$city <- "San Francisco"
#variable to hold city
san_fran_5000$property <- ""
san_fran_5000$property[which(san_fran_5000$type == "Property")] <- TRUE 
san_fran_5000$property[which(san_fran_5000$type != "Property")] <- FALSE 
#boolean to hold if a crime is property
san_fran_5000$drug <- ""
san_fran_5000$drug[which(san_fran_5000$Incident.Category == "Drug Offense")] <- TRUE
san_fran_5000$drug[which(san_fran_5000$Incident.Category != "Drug Offense")] <- FALSE
#boolean to hold if a crime is drug related
san_fran_5000$fire_arm <- FALSE
san_fran_5000$fire_arm[which(san_fran_5000$Incident.Description == 
"Firearm, Discharging Within City Limits")] <- TRUE
san_fran_5000$fire_arm[which(san_fran_5000$Incident.Description == 
                               "Theft, Grand, of Firearm")] <- TRUE
san_fran_5000$fire_arm[which(san_fran_5000$Incident.Description == 
                               "Firearm, Possession of Loaded")] <- TRUE
san_fran_5000$fire_arm[which(san_fran_5000$Incident.Description == 
                               "Firearm, Possession By Prohibited Person")] <- TRUE
san_fran_5000$fire_arm[which(san_fran_5000$Incident.Description == 
                               "Firearm, Armed While Possessing Controlled Substance")] <- TRUE
san_fran_5000$fire_arm[which(san_fran_5000$Incident.Description == 
                               "Firearm, Loaded, in Vehicle, Possession or Use")] <- TRUE
san_fran_5000$fire_arm[which(san_fran_5000$Incident.Description == 
                               "Firearm, Discharging in Grossly Negligent Manner")] <- TRUE
san_fran_5000$fire_arm[which(san_fran_5000$Incident.Description == 
                               "Firearm, Discharging in Grossly Negligent Manner")] <- TRUE

san_fran_5000$fire_arm[which(san_fran_5000$Incident.Description ==
                               "Firearm, Discharging at Occupied Bldg, Vehicle, or Aircraft")]<- TRUE
san_fran_5000$fire_arm[which(san_fran_5000$Incident.Description ==
                               "Firearm, Tampering With Marks")]<- TRUE

dallas_5000$fire_arm[which(dallas_5000$category ==
                               "THEFT OF FIREARM")]<- TRUE
dallas_5000$fire_arm[which(dallas_5000$category ==
                             "DISCHARGE FIREARM IN CERTAIN MUNICIPALITIES")]<- TRUE
dallas_5000$fire_arm[which(dallas_5000$category ==
                             "DEADLY CONDUCT DISCHARGE FIREARM")]<- TRUE
dallas_5000$fire_arm[which(dallas_5000$category ==
                             "UNLAWFUL POSS FIREARM BY FELON")]<- TRUE
dallas_5000$fire_arm[which(dallas_5000$category ==
                             "INJURED PERSON - FIREARM INJURY (NO OFFENSE)")]<- TRUE
dallas_5000$fire_arm[which(dallas_5000$category ==
                             "ASSAULT (AGG) -DISCH FIREARM  OCC BLDG/HOUSE/VEH (AGG)")]<- TRUE
dallas_5000$fire_arm[which(dallas_5000$category ==
                             "DISORDERLY CONDUCT DISCHARGE/DISPLAY FIREARM")]<- TRUE
dallas_5000$fire_arm[which(dallas_5000$category ==
                             "UNLAWFUL CARRYING WEAPON")]<- TRUE
dallas_5000$fire_arm[which(dallas_5000$category ==
                             "HOMICIDE (POLICE SHOOTING)")]<- TRUE
#boolean to hold if a crime is firearm related












san_fran_5000$month <- substring(san_fran_5000$Incident.Date, 6,7)
san_fran_5000$income_level <- " "
san_fran_5000$income_level[which(san_fran_5000$Police.District == "Richmond")] <- "very high"
san_fran_5000$income_level[which(san_fran_5000$Police.District == "Southern")] <- "very high"
san_fran_5000$income_level[which(san_fran_5000$Police.District == "Northern")] <- "very high"
san_fran_5000$income_level[which(san_fran_5000$Police.District == "Mission")] <- "very high"
san_fran_5000$income_level[which(san_fran_5000$Police.District == "Taraval")] <- "very high"
san_fran_5000$income_level[which(san_fran_5000$Police.District == "Park")] <- "very high"
san_fran_5000$income_level[which(san_fran_5000$Police.District == "Central")] <- "medium"
san_fran_5000$income_level[which(san_fran_5000$Police.District == "Bayview")] <- "medium"
san_fran_5000$income_level[which(san_fran_5000$Police.District == "Tenderloin")] <- "low"
san_fran_5000$income_level[which(san_fran_5000$Police.District == "Ingleside")] <- "high"
san_fran_5000$income_level <- factor(san_fran_5000$income_level, 
                                     levels = c("low", "medium", "high", "very high"))
#variable to hold income level based on police district


names(san_fran_5000)[names(san_fran_5000) == "Incident.Year"] <- "year"
names(san_fran_5000)[names(san_fran_5000) == "Incident.Day.of.Week"] <- "day_of_week"
names(san_fran_5000)[names(san_fran_5000) == "Police.District"] <- "district"
names(san_fran_5000)[names(san_fran_5000) == "Incident.Time"] <- "time"
names(san_fran_5000)[names(san_fran_5000) == "Incident.Category"] <- "category"
names(san_fran_5000)[names(san_fran_5000) == "Incident.Description"] <- "description"


san_fran_5000 <- san_fran_5000 %>% dplyr::select(!description)

san_fran_5000 <- san_fran_5000 %>% dplyr::select(!Incident.Date)
dallas_5000 <- dallas_5000 %>% dplyr::select(!X)
combined <- rbind(san_fran_5000, dallas_5000)
#combines san francisco and dallas datasets
varCount <- combined %>% dplyr::select(!time)
varCount <- varCount %>% dplyr::select(!category)
varCount <- varCount %>% dplyr::select(!type)
#gets rid of uncessary variables


countProp <- combined%>%group_by(property, city)%>%tally()
#counts number of property and violent crimes
countProp$property[which(countProp$property== "FALSE")] <- "Violent"
countProp$property[which(countProp$property== "TRUE")] <- "Property"

countDay <- combined%>%group_by(city, day_of_week)%>%tally()
#counts number pf crimes committed on each day of the weak

ggplot(data = countProp, aes(x = city, y = n, fill = property)) +
  geom_bar(stat = "identity" ) + scale_fill_manual(values = c("#ffde59", "#cf4946")) +
  theme_dark() +
  labs(title = "Property v Violent Crimes for 
  Dallas and San Francisco", 
       x = "City", y = "Amount", fill = " ")
#plots property vs violent crimes for dallas and san francisco in a bar pllot

countViolent <- combined%>%group_by(violent, city)%>%tally()
countViolent$violent[which(countViolent$violent== "FALSE")] <- "Non-violent"
countViolent$violent[which(countViolent$violent== "TRUE")] <- "Violent"

ggplot(data = countViolent, aes(x = city, y = n, fill = violent)) +
  geom_bar(stat = "identity" ) + labs(title = "violent")

countDrug <- combined%>%group_by(drug, city)%>%tally()
countDrug <- filter(countDrug, drug == TRUE)
countDrug$percent <- countDrug$n/5000
countDrug$percent <- countDrug$percent * 1000
ggplot(data = countDrug, aes(x = city, y = percent, fill = city)) +
  geom_bar(stat = "identity" ) + theme_dark() +
  labs(title = "Percentage of Drug Related Crimes for
       Dallas and San Francisco", x = "City", y = "Percent", fill = " ") + 
  scale_fill_manual(values = c("#ffde59", "#cf4946")) 
#plots percentage of drug related crimes for dallas and san francisco in a bar 
#plot

countGun <- combined%>%group_by(fire_arm, city)%>%tally()
countGun <- filter(countGun, fire_arm == TRUE)
countGun$percent <- countGun$n/5000
countGun$percent <- countGun$percent * 1000
ggplot(data = countGun, aes(x = city, y = percent, fill = city)) +
  geom_bar(stat = "identity" ) + theme_dark() +
  labs(title = "Percentage of Gun Related Crimes for
       Dallas and San Francisco", x = "City", y = "Percent", fill = " ") +
  scale_fill_manual(values = c("#ffde59", "#cf4946"))
#plots percentage of drug related crimes for dallas and san francisco in a bar
#plot

countDay$day_of_week[which(countDay$day_of_week == "Friday")] <- "Fri"
countDay$day_of_week[which(countDay$day_of_week == "Monday")] <- "Mon"
countDay$day_of_week[which(countDay$day_of_week == "Tuesday")] <- "Tue"
countDay$day_of_week[which(countDay$day_of_week == "Wednesday")] <- "Wed"
countDay$day_of_week[which(countDay$day_of_week == "Thursday")] <- "Thu"
countDay$day_of_week[which(countDay$day_of_week == "Saturday")] <- "Sat"
countDay$day_of_week[which(countDay$day_of_week == "Sunday")] <- "Sun"
countDay$percent <- countDay$n/5000
countDay$percent <- countDay$percent*100
countDay$day_of_week <- factor(countDay$day_of_week, 
                               levels = c("Mon", "Tue", "Wed", "Thu", "Fri",
                                          "Sat", "Sun"), ordered = TRUE)
ggplot(data = countDay, aes(x = day_of_week, y = percent, fill = city)) + 
  geom_bar(stat = "identity", position = position_dodge()) + theme_dark() +
  labs(title = "Percentage of Crime for Days of Week", y = "Percent", x = "Day of Week") +
  scale_fill_manual(values = c("#ffde59", "#cf4946"))
#plots percentage of crime for days of the week in a bar plot


countDist <- combined%>%group_by(type, city, district, income_level)%>%tally()
countDistProp <- filter(countDist, type == "Property")
countDistPropDal <- filter(countDistProp, city == "Dallas")
countDistPropSF <- filter(countDistProp, city == "San Francisco")
countDistViolent <- filter(countDist, type == "Violent")
countDistViolentDal <- filter(countDistViolent, city == "Dallas")
countDistViolentSF <- filter(countDistViolent, city == "San Francisco")
#counts property/violent crime per district

ggplot(data = countDistPropDal, aes(x = income_level, y = n, fill = district)) + 
  geom_bar(stat = "identity") + labs(title = "Amount of Property Crime 
  v Income Level of Districts Of Dallas", x = "Income Level", 
                                     y = "Amount", fill = "District") 
ggplot(data = countDistViolentDal, aes(x = income_level, y = n, fill = district)) + 
  geom_bar(stat = "identity") + labs(title = "Amount of Violent Crime 
  v Income Level Districts Of Dallas", x = "Income Level", 
                                     y = "Amount", fill = "District") 
ggplot(data = countDistPropSF, aes(x = income_level, y = n, fill = district)) + 
  geom_bar(stat = "identity") + labs(title = "Amount of Property Crime 
  v Income Level of Districts Of San Francisco", x = "Income Level", 
                                     y = "Amount", fill = "District") 
ggplot(data = countDistViolentSF, aes(x = income_level, y = n, fill = district)) + 
  geom_bar(stat = "identity") + labs(title = "Amount of Violent Crime 
  v Income Level Districts Of San Francisco", x = "Income Level", 
                                     y = "Amount", fill = "District") 

ggplot(data = countDistPropDal, aes(x = district, y = n, fill = income_level)) +
  geom_bar(stat = "identity") + labs(title = "Amount of Property Crime for 
  Districts of Dallas", y = "Amount", fill = "Income Level") +
  theme_dark() + scale_fill_manual(values = c("#ffde59", "#cf4946", "#e1a060"))
ggplot(data = countDistViolentDal, aes(x = district, y = n, fill = income_level)) +
  geom_bar(stat = "identity") + labs(title = "Amount of Violent Crime for 
  Districts of Dallas", y = "Amount", fill = "Income Level") +
  theme_dark() + scale_fill_manual(values = c("#ffde59", "#cf4946", "#e1a060"))
ggplot(data = countDistPropSF, aes(x = district, y = n, fill = income_level)) +
  geom_bar(stat = "identity") + labs(title = "Amount of Property Crime for 
  Districts of San Francisco", y = "Amount", fill = "Income Level") +
  theme_dark() + scale_fill_manual(values = c("#ffde59", "#cf4946", "#e1a060", "#4d2b1d"))
ggplot(data = countDistViolentSF, aes(x = district, y = n, fill = income_level)) +
  geom_bar(stat = "identity") + labs(title = "Amount of Violent Crime for 
  Districts of San Francisco", y = "Amount", fill = "Income Level") +
  theme_dark() + scale_fill_manual(values = c("#ffde59", "#cf4946", "#e1a060", "#4d2b1d"))
#plots property/violent crime for districts of dallas and san francisco, 
#comparing it with the income level of each district

countMonth <- combined%>%group_by(type, city, month)%>%tally()
countMonth$monthNum <- 0
countMonth$monthNum[which(countMonth$month == "01")] <- 1
countMonth$monthNum[which(countMonth$month == "January")] <- 1
countMonth$monthNum[which(countMonth$month == "02")] <- 2
countMonth$monthNum[which(countMonth$month == "February")] <- 2
countMonth$monthNum[which(countMonth$month == "03")] <- 3
countMonth$monthNum[which(countMonth$month == "March")] <- 3
countMonth$monthNum[which(countMonth$month == "04")] <- 4
countMonth$monthNum[which(countMonth$month == "April")] <- 4
countMonth$monthNum[which(countMonth$month == "05")] <- 5
countMonth$monthNum[which(countMonth$month == "May")] <- 5
countMonth$monthNum[which(countMonth$month == "06")] <- 6
countMonth$monthNum[which(countMonth$month == "June")] <- 6
countMonth$monthNum[which(countMonth$month == "07")] <- 7
countMonth$monthNum[which(countMonth$month == "July")] <-7
countMonth$monthNum[which(countMonth$month == "08")] <- 8
countMonth$monthNum[which(countMonth$month == "August")] <- 8
countMonth$monthNum[which(countMonth$month == "09")] <- 9
countMonth$monthNum[which(countMonth$month == "September")] <- 9
countMonth$monthNum[which(countMonth$month == "10")] <- 10
countMonth$monthNum[which(countMonth$month == "October")] <- 10
countMonth$monthNum[which(countMonth$month == "11")] <- 11
countMonth$monthNum[which(countMonth$month == "November")] <- 11
countMonth$monthNum[which(countMonth$month == "12")] <- 12
countMonth$monthNum[which(countMonth$month == "December")] <- 12

ggplot(data = countMonth, aes(x = monthNum, y = n, color = city, shape = type)) +
  geom_line() + geom_point() + 
  scale_x_continuous(name = "Month", breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                     limits = c(1,12)) +

  theme_dark() + scale_color_manual(values = c("#ffde59", "#cf4946")) +
  labs(title = "Amount of Violent and Property Crime for
       Dallas and San Francisco for each Month", x = "Month", y = "Amount")

ggplot(data = countMonth, aes(x = month, y = n, color = city)) + geom_point(aes(shape = type)) +
  geom_line() +
theme_dark() + scale_color_manual(values = c("#ffde59", "#cf4946")) 

#plots amount of violent and property crime per month for both cities

combined$month[which(combined$month == "01")] <- "Jan"
combined$month[which(combined$month == "January")] <- "Jan"
combined$month[which(combined$month == "02")] <- "Feb"
combined$month[which(combined$month == "February")] <- "Feb"
combined$month[which(combined$month == "03")] <- "Mar"
combined$month[which(combined$month == "March")] <- "Mar"
combined$month[which(combined$month == "04")] <- "Apr"
combined$month[which(combined$month == "April")] <- "Apr"
combined$month[which(combined$month == "05")] <- "May"
combined$month[which(combined$month == "06")] <- "Jun"
combined$month[which(combined$month == "June")] <- "Jun"
combined$month[which(combined$month == "07")] <- "Jul"
combined$month[which(combined$month == "July")] <- "Jul"
combined$month[which(combined$month == "08")] <- "Aug"
combined$month[which(combined$month == "August")] <- "Aug"
combined$month[which(combined$month == "09")] <- "Sep"
combined$month[which(combined$month == "September")] <- "Sep"
combined$month[which(combined$month == "10")] <- "Oct"
combined$month[which(combined$month == "October")] <- "Oct"
combined$month[which(combined$month == "11")] <- "Nov"
combined$month[which(combined$month == "November")] <- "Nov"
combined$month[which(combined$month == "12")] <- "Dec"
combined$month[which(combined$month == "December")] <- "Dec"



combined$countViolentSF <- 0
combined$countViolentSF[which(combined$month == "01")] <- 59
combined$countViolentSF[which(combined$month == "Feb")] <- 54
combined$countViolentSF[which(combined$month == "Mar")] <- 58
combined$countViolentSF[which(combined$month == "Apr")] <- 55
combined$countViolentSF[which(combined$month == "May")] <- 65
combined$countViolentSF[which(combined$month == "Jun")] <- 65
combined$countViolentSF[which(combined$month == "Jul")] <- 64
combined$countViolentSF[which(combined$month == "Aug")] <- 68
combined$countViolentSF[which(combined$month == "Sep")] <- 60
combined$countViolentSF[which(combined$month == "Oct")] <- 67
combined$countViolentSF[which(combined$month == "Nov")] <- 
combined$countViolentSF[which(combined$month == "Feb")] <- 54
combined$countViolentSF[which(combined$month == "Feb")] <- 54


countYear <- combined%>%group_by(type, city, year)%>%tally()

ggplot(data = countYear, aes(x = year, y = n, color = city, shape = type)) +
  geom_line() + geom_point() + 
  scale_x_continuous(name = "Year", breaks = c(2018, 2019, 2020, 2021, 2022),
                     limits = c(2018, 2022)) +
  theme_dark() + scale_color_manual(values = c("#ffde59", "#cf4946")) +
  labs(title = "Amount of Violent and Property Crime for
       Dallas and San Francisco for each Year", x = "Year", y = "Amount")

ggplot(data = countMonth, aes(x = month, y = n, color = city, shape = type)) +
  geom_line() + geom_point()
countMonth%>%
  ggplot(aes(x = month, y = n, color = city, shape = type)) +
  geom_point()
#plots crime for each year for both cities
countMonthSF <- filter(countMonth, city == "Violent")
countMonthProp <- filter(countMonth, type == "Property")
countMonthViolentSF <- filter(countMonthViolent, city == "San Francisco")
countMonthViolentSF <- filter(countMonthViolent, city == "San Francisco")







write.csv(combined, "/Users/angelina/Documents/stat's project/combined.csv", row.names = FALSE)
