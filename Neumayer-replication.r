#The research question in this study explores whether rehabilitation and 
#reconstruction activities carried out in the aftermath of a natural disaster, can 
#cause recovery of GDP growth in the short and long term to pre-disaster growth 
#rates, or whether recovery of GDP growth to pre-disaster levels is not possible. 
#This question is significant particularly in context of the increased frequency of 
#natural disasters due to anthropogenic climate change, which also makes it 
#necessary to ascertain the predicted future frequency of natural disasters, and 
#the policy level interventions required to manage their fallout. For consistency of 
#analysis, this study focuses exclusively on the Southeast Asian state of Indonesia. 
#To answer the research question, data from the study “The impact of the Indian 
#Ocean tsunami on Aceh’s long-term Economic Growth” (Neumayer et al, 2019) is 
#used and the findings of the study are replicated.

rm(list=ls())
getwd()
library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("lfe")
library(lfe)
data <- read.csv("Article for JED (districts).csv")
data2 <- read.csv("Article for JED (districts for synthetic control).csv")
data3 <- read.csv("Article for JED (Kecamatans).csv")
data21 <- na.omit(data2)

unique(data$island)
unique(data$prov)
unique(data$tsunami_aceh)
unique(data$tsunami_sumatra)
unique(data$tsunami_aceh1)
unique(data$tsunami_aceh2)
unique(data$tsunami_aceh3)
unique(data$tsunami_sumatra1)
unique(data$tsunami_sumatra2)
unique(data$tsunami_sumatra3)
unique(data$aceh_treat3)

plot(x = data2$year, y = data2$gdp, frame = FALSE, col = "#2E9FDF")
abline(lm(data2$gdp ~ data2$year))


data2$did <- data2$tsunami_yrs * data2$aceh_treat1

didreg <- lm(gdp ~ tsunami_yrs + aceh_treat1 + did, data = data2)
summary(didreg)

didreg1 <- lm(gdp ~ tsunami_yrs + aceh_treat1 + tsunami_yrs:aceh_treat1, data = data2)
summary(didreg1)

reg2 <- lm(gdp ~ tsunami_yrs + aceh_treat1, data = data2)
summary(reg2)

interaction <- data2$tsunami_yrs * data2$aceh_treat1


print(interaction)

data21 <- data2[!(is.na(data2$aceh_treat1)),]
data211 <- data21[!(is.na(data21$tsunami_yrs)),]

unique(data$tsunami_aceh1)
data212 <- data211[data211$tsunami_aceh1 == "Tsunami stricken",]
data213 <- data211[data211$tsunami_aceh1 == "not Tsunami stricken",]

unique(data3$id)
unique(data$tsunami_aceh)

data$post <- ifelse(data$year >= 2004, 1, 0)
data$ever_treated <- ifelse(data$tsunami_aceh == "Tsunami stricken", 1, 0)
data3$treated <- ifelse(data3$pop_flooded_sum == 0, 0, 1)


causal1 <- lm(log_gr ~ post + ever_treated + post*ever_treated, data = data)
summary(causal1)


causal1 <- lm(log_gr ~ aceh_treat1 + tsunami_yrs, data = data)
summary(causal1)

data %>% 
  filter(tsunami_aceh == "Tsunami stricken") %>%
  group_by(year) %>%
  summarize(growth = mean(log_gr), .groups = "keep") %>%
  ggplot(aes(year, growth)) +
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = 2004), linetype = 2) +
  theme(legend.position = "bottom")


data$treated_yet[data$year >= 2004] <- 1
data$treated_yet[data$year < 2004] <- 0
data$treated_yet[data$tsunami_aceh != "Tsunami stricken"] <- 0

fixed_effects <- felm(log_gr ~ treated_yet | id | 0 | id, data = data)
summary(fixed_effects)

#***

df_es <- data %>%
  
  mutate(ever_treated_es = case_when(!tsunami_aceh == "Tsunami stricken" ~ 0,
                                     
                                     tsunami_aceh == "Tsunami stricken" ~ 1,
                                     
                                     TRUE ~ as.numeric(NA)),
         
         event_time = ifelse(!is.na(tsunami_aceh), year - 2004, 0),
         
         D = factor(ever_treated * event_time),
         
         D = relevel(D, ref = "-1")) # here is where we define the omitted category, with "relevel"



es <- felm(log_gr ~ D| id + year |0| id, data = df_es)

summary(es)



#```{r event_study_plot}



# create a graphable dataframe from your regression object

res <- as.data.frame(summary(es)$coefficients)

res$low <- res$Estimate - qnorm(1 - 0.05/2)*res$`Cluster s.e.`

res$high <- res$Estimate + qnorm(1 - 0.05/2)*res$`Cluster s.e.`

res$event_time_value <- c(-4:-2, 0:8)

res <- res %>% dplyr::select(Estimate, event_time_value, low, high)



# add an observation for event time -1

omitted <- data.frame("Estimate" = 0,
                      
                      "event_time_value" = -1,
                      
                      "low" = 0,
                      
                      "high" = 0)

res <- res %>% rbind(omitted)



res %>%
  
  ggplot(aes(x = event_time_value, y = Estimate)) +
  
  geom_point() +
  
  geom_line() +
  
  geom_ribbon(aes(ymin = low, ymax = high), fill = "blue", alpha = 0.5) +
  
  scale_x_continuous(breaks = res$event_time_value)

#***


copy <- data.frame(data)
unique(copy$tsunami_aceh1)
unique(copy$tsunami_aceh2)
unique(copy$tsunami_aceh3)
copy20 <- subset(data2, select = c("id", "gdp"))
merged <- merge(copy, copy20, by = "id", all = TRUE)


df_es <- data %>%
  
  mutate(ever_treated_es = case_when(!tsunami_aceh == "Tsunami stricken" ~ 0,
                                     
                                     tsunami_aceh == "Tsunami stricken" ~ 1,
                                     
                                     TRUE ~ as.numeric(NA)),
         
         event_time = ifelse(!is.na(tsunami_aceh), year - 2004, 0),
         
         D = factor(ever_treated * event_time),
         
         D = relevel(D, ref = "-1")) # here is where we define the omitted category, with "relevel"



es <- felm(log_gr ~ D| id + year |0| id, data = df_es)

summary(es)



#```{r event_study_plot}



# create a graphable dataframe from your regression object

res <- as.data.frame(summary(es)$coefficients)

res$low <- res$Estimate - qnorm(1 - 0.05/2)*res$`Cluster s.e.`

res$high <- res$Estimate + qnorm(1 - 0.05/2)*res$`Cluster s.e.`

res$event_time_value <- c(-4:-2, 0:8)

res <- res %>% dplyr::select(Estimate, event_time_value, low, high)



# add an observation for event time -1

omitted <- data.frame("Estimate" = 0,
                      
                      "event_time_value" = -1,
                      
                      "low" = 0,
                      
                      "high" = 0)

res <- res %>% rbind(omitted)



res %>%
  
  ggplot(aes(x = event_time_value, y = Estimate)) +
  
  geom_point() +
  
  geom_line() +
  
  geom_ribbon(aes(ymin = low, ymax = high), fill = "blue", alpha = 0.5) +
  
  scale_x_continuous(breaks = res$event_time_value)

#***

#data %>% 
  #filter(tsunami_aceh == "Tsunami stricken") %>%
  #group_by(year) %>%
  #summarize(growth = mean(log_gr), .groups = "keep") %>%
  #ggplot(aes(year, growth)) +
  #geom_point() +
  #geom_line() +
  #geom_vline(aes(xintercept = 2004), linetype = 2) +
  #theme(legend.position = "bottom")

data %>% 
  filter(tsunami_sumatra == "Rest of Sumatra") %>%
  group_by(year) %>%
  summarize(growth = mean(log_gr), .groups = "keep") %>%
  ggplot(aes(year, growth)) +
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = 2004), linetype = 2) +
  theme(legend.position = "bottom")



data$group1 <- ifelse(data$year %in% c(2003, 2004), 1, 0)
data$group2 <- ifelse(data$year == 2005, 1, 0)
data$group3 <- ifelse(data$year %in% c(2006, 2007, 2008), 1, 0)
data$group4 <- ifelse(data$year %in% c(2009, 2010, 2011, 2012), 1, 0)


###NIGHT LIGHTS

data3_es <- data3 %>%
  
  mutate(ever_treated_es = case_when(pop_flooded_sum == 0 ~ 0,
                                     
                                     pop_flooded_sum != 0 ~ 1,
                                     
                                     TRUE ~ as.numeric(NA)),
         
         event_time = ifelse(!is.na(pop_flooded_sum), year - 2004, 0),
         
         D = factor(treated * event_time),
         
         D = relevel(D, ref = "-1")) # here is where we define the omitted category, with "relevel"



es_night_lights <- felm(lights_mean ~ D| id + year |0| id, data = data3_es)

summary(es_night_lights)



#```{r event_study_plot}



# create a graphable dataframe from your regression object

res3 <- as.data.frame(summary(es_night_lights)$coefficients)

res3$low <- res3$Estimate - qnorm(1 - 0.05/2)*res3$`Cluster s.e.`

res3$high <- res3$Estimate + qnorm(1 - 0.05/2)*res3$`Cluster s.e.`

res3$event_time_value <- c(-12:-2, 0:8)

res3 <- res3 %>% dplyr::select(Estimate, event_time_value, low, high)



# add an observation for event time -1

omitted3 <- data.frame("Estimate" = 0,
                       
                       "event_time_value" = -1,
                       
                       "low" = 0,
                       
                       "high" = 0)

res3 <- res3 %>% rbind(omitted)



res3 %>%
  
  ggplot(aes(x = event_time_value, y = Estimate)) +
  
  geom_point() +
  
  geom_line() +
  
  geom_ribbon(aes(ymin = low, ymax = high), fill = "blue", alpha = 0.5) +
  
  scale_x_continuous(breaks = res3$event_time_value)

#***


#data3$group1 <- ifelse(data3$year %in% c(2003, 2004), 1, 0)
#data3$group2 <- ifelse(data3$year == 2005, 1, 0)
#data3$group3 <- ifelse(data3$year %in% c(2006, 2007, 2008), 1, 0)
#data3$group4 <- ifelse(data3$year %in% c(2009, 2010, 2011, 2012), 1, 0)

#merged %>% 
  #filter(tsunami_aceh == "Tsunami stricken") %>%
  #group_by(year) %>%
  #summarize(growth = mean(log_gr), .groups = "keep") %>%
  #ggplot(aes(year, growth)) +
  #geom_point() +
  #geom_line() +
  #geom_vline(aes(xintercept = 2004), linetype = 2) +
  #theme(legend.position = "bottom")