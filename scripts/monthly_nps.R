library(tidyverse)
library(lubridate)



# Autotask Client Listing - Needed to match data for NPS Upload to AT
# following all calculations

clients <- read.csv("rawdata/clients.csv", stringsAsFactors = FALSE) %>% 
  mutate(web = tolower(web))

clients$at_classification[is.na(clients$at_classification)] <- "Unknown"
clients$at_classification[clients$at_classification==""] <- "Unknown"



# 24-25 Client Survey
survey_2425 <- read.csv("rawdata/client_survey_2425.csv", stringsAsFactors = FALSE) %>% 
  select(Email.Address, Can.we.use.you.as.a.reference.) %>% 
  rename("Reference" = Can.we.use.you.as.a.reference.) %>% 
  mutate(Reference = ifelse(Reference=="Yes", "Yes", "No")) %>% 
  mutate(Domain = tolower(sub('.*@', '', Email.Address))) %>% 
  arrange(Domain, Reference) %>% 
  distinct(Domain, .keep_all = TRUE) %>% 
  mutate(Survey = "Yes")


clients <- left_join(clients, select(survey_2425, Domain, Survey, Reference), by=(c("web" = "Domain"))) 
rm(survey_2425)

clients$Survey <- replace_na(clients$Survey, "No")
clients$Reference <- replace_na(clients$Reference, "Unknown")



# 24-25 School Opening Survey
so_survey_2425 <- read.csv("rawdata/so_survey_2425.csv", stringsAsFactors = FALSE) %>% 
  select(Username, Overall..were.you.satisfied.with.school.opening.as.it.relates.to.technology.)%>% 
  rename("SO_Satisfaction" = Overall..were.you.satisfied.with.school.opening.as.it.relates.to.technology.) %>% 
  mutate(Domain = tolower(sub('.*@', '', Username))) %>% 
  arrange(Domain, SO_Satisfaction) %>% 
  distinct(Domain, .keep_all = TRUE) %>% 
  mutate(SO_Survey = "Yes")


clients <- left_join(clients, select(so_survey_2425, Domain, SO_Survey, SO_Satisfaction), by=(c("web" = "Domain"))) 
rm(so_survey_2425)

clients$SO_Survey <- replace_na(clients$SO_Survey, "No")
clients$SO_Satisfaction <- replace_na(clients$SO_Satisfaction, 0)



# CUrrent Contracts
sales <- read.csv("rawdata/sales.csv", stringsAsFactors = FALSE) %>% 
  mutate(close_date = mdy(close_date))

num_svcs <- arrange(sales, name, category) %>% 
  distinct(name, category, .keep_all= TRUE) %>% 
  group_by(name) %>%
  summarize(NumSvcs = n())

clients <- left_join(clients, num_svcs, by="name")
clients$NumSvcs <- replace_na(clients$NumSvcs, 0)

rm(num_svcs)

total_sales <- group_by(sales, name) %>%
  summarize(TotalSales = sum(amount))

clients <- left_join(clients, total_sales, by="name")
clients$TotalSales <- replace_na(clients$TotalSales, 0)

rm(total_sales)

# Last year Client Services

curr_sales <- filter(sales, close_date >= mdy("01/01/2024")) %>% 
  arrange(name, category, desc(close_date)) %>% 
  distinct(name, category, .keep_all= TRUE) 

write.csv(curr_sales, "cleandata/current_sales.csv", row.names = FALSE)

# Last 6 months surveys

surveys <- read.csv("rawdata/surveys.csv", stringsAsFactors = FALSE) %>% 
  filter(!is.na(Survey.Rating)) %>% 
  left_join(clients, by=c("Client.Name" = "name"))

surveys$name <- ifelse(is.na(surveys$web), surveys$Parent.Client.Name, surveys$Client.Name)

surveys <- group_by(surveys, name) %>% 
  summarize(AvgScore = round(mean(Survey.Rating),2),
            NumSurv = n())

clients <- left_join(clients, surveys, by="name")
rm(surveys)

clients$AvgScore <- replace_na(clients$AvgScore, 0)
clients$NumSurv <- replace_na(clients$NumSurv, 0)



## Import list of key meetings from AT Project tasks

meetings <- read.csv("rawdata/meetings.csv", stringsAsFactors = FALSE) %>% 
  mutate(Due.Date = mdy(Due.Date)) %>% 
  mutate(Complete.Date = mdy(Complete.Date))

meetings$overdue <- ifelse(meetings$Due.Date < today() & is.na(meetings$Complete.Date), 1, 0)

overdue_mtg <- group_by(meetings, Client.Name) %>% 
  summarize(OverdueMtg = sum(overdue))



clients <- left_join(clients, overdue_mtg, by=c("name" = "Client.Name"))

clients$OverdueMtg[is.na(clients$OverdueMtg)] <- 0

rm(meetings, overdue_mtg)



## Customer Success (from Vinson Protect)

success <- read.csv("rawdata/customer_success.csv", stringsAsFactors = FALSE) %>% 
  select(Name, Classification) %>% 
  rename("vp_classification" = Classification)

clients <- left_join(clients, success, by=c("name" = "Name"))

clients$vp_classification[is.na(clients$vp_classification)] <- "Unknown"



clients$Classification <- ifelse(clients$at_classification != clients$vp_classification & clients$vp_classification != "Unknown", 
                                 clients$vp_classification, clients$at_classification)

clients$Classification <- ifelse(clients$vp_classification == "Unknown", "Red Flag - Executive Involvement", clients$vp_classification)



clients$Classification[is.na(clients$Classification)] <- "Unknown"







## Calculate NPS Rating for each Client

clients$NPS_Rating_Score <- 0

clients$NPS_Rating_Score <- clients$NPS_Rating_Score +
  ifelse(clients$Reference=="Yes", 50, 
                                   ifelse(clients$Reference=="No", -100, 0))
                                   
                                   
clients$NPS_Rating_Score <- clients$NPS_Rating_Score +
  clients$NumSvcs*10


clients$NPS_Rating_Score <- clients$NPS_Rating_Score +
  ifelse(str_detect(clients$Classification, "Red"), -100, 0)

clients$NPS_Rating_Score <- clients$NPS_Rating_Score +
  ifelse(clients$SO_Satisfaction==1, -50,ifelse(clients$SO_Satisfaction==2, -25,ifelse(clients$SO_Satisfaction==3, 0,
                                                                                       ifelse(clients$SO_Satisfaction==4,25,0))))



clients$NPS_Rating <- ifelse(clients$NPS_Rating_Score >= 70, "Promoter", 
                             ifelse(clients$NPS_Rating_Score < 0, "Detractor", "Passive"))


## Calculate NPS Score for Vinson


nps <- 100*(as.numeric(sum(clients$NPS_Rating == "Promoter"))/nrow(clients) - 
  as.numeric(sum(clients$NPS_Rating == "Detractor"))/nrow(clients))
  

clients$Vinson_NPS <- nps



  
## Create load file for AT

loadfile <- select(clients, ID, name, phone, NPS_Rating_Score, NPS_Rating,
                   Survey, Reference, NumSvcs, TotalSales,
                   AvgScore, NumSurv, OverdueMtg, vp_classification, 
                   Classification, SO_Survey, SO_Satisfaction) %>% 
  rename("Client ID [updates only]" = ID,
         "[required] Name" = name,
         "[required] Phone" = phone,
         "Client UDF:29683097 NPS Rating" = NPS_Rating,
         "Client UDF:29683098 NPS Rating Score" = NPS_Rating_Score,
         "Client UDF:29683100 NPS Annual Survey" = Survey,
         "Client UDF:29683104 NPS Average Survey Score" = AvgScore,
         "Client UDF:29683102 NPS Number of Services" = NumSvcs,
         "Client UDF:29683105 NPS Number of Surveys - 180 Days" = NumSurv,
         "Client UDF:29683101 NPS Reference?" = Reference,
         "Client UDF:29683103 NPS Total Sales" = TotalSales,
         "Client UDF:29683106 NPS Overdue Meetings" = OverdueMtg,
         "Client UDF:29683108 NPS Customer Success" = vp_classification,
         "Client UDF:29683109 NPS SO Survey" = SO_Survey,
         "Client UDF:29683110 NPS SO Survey Score" = SO_Satisfaction)

write.csv(loadfile, "cleandata/nps_at_loadfile.csv", row.names = FALSE)



##