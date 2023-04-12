  # Packages installation and libraries loading 

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("skimr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("here")
install.packages("janitor")

library(tidyverse)
library(ggplot2)
library(skimr)
library(dplyr)
library(lubridate)
library(here)
library(janitor)
library(tidyr)


  # Data sets import

daily_activity <- read.csv ("daily_activity.csv" , header = TRUE, sep = ",")
daily_sleep <- read.csv ("daily_sleep.csv" , header = TRUE, sep = ",")


  # Data frames preview and cleaning

n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)

sum(duplicated(daily_activity))
sum(duplicated(daily_sleep))

clean_names(daily_activity)
daily_activity <- rename_with(daily_activity, tolower)

clean_names(daily_sleep)
daily_sleep <- rename_with(daily_sleep, tolower)

  
daily_activity <- daily_activity %>%
  rename(date = activitydate) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_sleep <- daily_sleep %>%
  rename(date = sleepday) %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))



str(daily_activity)
str(daily_sleep)


  # Summarizing averages
    # Classification of Activity levels
    # User Classification

daily_activity ["total_minutes_active"] <- daily_activity$veryactiveminutes + daily_activity$fairlyactiveminutes + daily_activity$lightlyactiveminutes

daily_activity ["device_usage_min"] <- daily_activity$total_minutes_active + daily_activity$sedentaryminutes

daily_activity ["device_usage_percent"] <- (daily_activity$device_usage_min / 1440) * 100 
  

daily_activity_avg <- daily_activity %>%
  group_by(id) %>%
  summarise (daily_steps_avg = mean(totalsteps),
             daily_calories_avg = mean(calories),
             daily_sedentaryminutes_avg = mean(sedentaryminutes),
             daily_activeminutes_avg = mean(total_minutes_active),
             device_usage_min_avg = mean(device_usage_min),
             device_usage_perc_avg = mean(device_usage_percent)) %>% 
  
  mutate(activity_level = case_when(
    daily_steps_avg < 5000 ~ "sedentary",
    daily_steps_avg >= 5000 & daily_steps_avg < 7500 ~ "low_active",
    daily_steps_avg >= 7500 & daily_steps_avg < 10000 ~ "somewhat_active",
    daily_steps_avg >= 10000 & daily_steps_avg < 12500 ~ "active",
    daily_steps_avg >= 12500 ~ "highly_active"
    )) %>% 
  
  mutate(user_classification = case_when(
    device_usage_perc_avg >= 95 ~ "top_user",
    device_usage_perc_avg >= 70 & device_usage_perc_avg < 95 ~ "regular_user",
    device_usage_perc_avg < 70 ~ "ocassional_user"
  ))

daily_activity <- merge(daily_activity, daily_activity_avg, by=("id"))


  # Remove unnecessary columns

daily_activity <- subset(
  daily_activity, select = -c(
    totaldistance, trackerdistance, loggedactivitiesdistance,veryactivedistance,
    moderatelyactivedistance, lightactivedistance, sedentaryactivedistance,
    daily_calories_avg, daily_activeminutes_avg, daily_steps_avg))


  # Average of daily sleep/user

daily_sleep_avg <- daily_sleep %>% 
  group_by(id) %>% 
  summarise (daily_sleep_avg_min = mean(totalminutesasleep))

daily_sleep_avg["daily_sleep_avg_hr"] <- daily_sleep_avg$daily_sleep_avg_min / 60

daily_sleep_avg$daily_sleep_avg_min <- format(round(daily_sleep_avg$daily_sleep_avg_min, 2), nsmall = 2)
daily_sleep_avg$daily_sleep_avg_hr <- format(round(daily_sleep_avg$daily_sleep_avg_hr, 2), nsmall = 2)


  # Analyzing User classification

user_class_totals <- daily_activity_avg %>% 
  group_by(user_classification) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_classification) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))


user_class_totals %>% 
  ggplot (aes (x = "", y = total_percent, fill = user_classification)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_minimal() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text (hjust = 0.5, size=14, face = "bold")) +
  scale_fill_brewer(palette="Blues") +
  geom_text (aes (label = labels),
            position = position_stack (vjust = 0.5)) +
  labs(title="User Classification")


# Analyzing User's activity levels

user_activity_level <- daily_activity_avg %>% 
  group_by(activity_level) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(activity_level) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

user_activity_level$activity_level <- factor(user_activity_level$activity_level,
                                             levels = c("highly_active", "active",
                                                        "somewhat_active", "low_active",
                                                        "sedentary"))

user_activity_level %>% 
  ggplot (aes (x = "", y = total_percent, fill = activity_level,)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_minimal() +
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text (hjust = 0.5, size=14, face = "bold")) +
  scale_fill_brewer(palette="YlGnBu", direction = -1) +
  geom_text (aes (label = labels),
             position = position_stack (vjust = 0.5)) +
  labs(title="User's Activity Level")


  # Relation between Daily Activity and Device usage 

ggplot (data = daily_activity_avg) +
  aes (x = daily_activeminutes_avg, y = device_usage_min_avg, color = user_classification) +
  geom_point(size = 5) +
  theme_minimal() +
  theme(panel.border = element_blank(),
        plot.title = element_text (hjust = 0.5, size=14, face = "bold")) +
  labs (title = "Daily Activity VS Device Usage",
        x = "Daily Activity (min)", y = "Device Usage (min)")

  # Relation between Daily Steps and Calories burnt

daily_activity_avg$activity_level <- factor(daily_activity_avg$activity_level,
                                             levels = c("highly_active", "active",
                                                        "somewhat_active", "low_active",
                                                        "sedentary"))

ggplot (data = daily_activity_avg) +
  aes (x = daily_steps_avg, y = daily_calories_avg, color = activity_level) +
  geom_point(size = 5) +
  scale_color_brewer(palette = "YlOrRd", direction = -1) +
  geom_jitter() +
  geom_smooth(color = "blue") +
  theme_minimal() +
  theme(panel.border = element_blank(),
        plot.title = element_text (hjust = 0.5, size=14, face = "bold")) +
  labs (title = "Daily Steps VS Calories Burnt",
        x = "Daily Daily Steps", y = "Calories Burnt")


  # Relation between Minutes Active and Calories burnt

ggplot (data = daily_activity_avg) +
  aes (x = daily_activeminutes_avg, y = daily_calories_avg, color = activity_level) +
  geom_point(size = 5) +
  scale_color_brewer(palette = "YlOrRd", direction = -1) +
  geom_jitter() +
  geom_smooth(color = "red") +
  theme_minimal() +
  theme(panel.border = element_blank(),
        plot.title = element_text (hjust = 0.5, size=14, face = "bold")) +
  labs (title = "Minutes Active VS Calories Burnt",
        x = "Minutes Active", y = "Calories Burnt")






#####----- I'll move on with the project, to continue with this analysis later.


  # Healthy habits over time

    ## DFs Creation

daily_activity <- subset(
  daily_activity, select = -c(veryactiveminutes, fairlyactiveminutes, lightlyactiveminutes,
                              sedentaryminutes, device_usage_min, device_usage_percent,
                              daily_sedentaryminutes_avg, device_usage_min_avg, device_usage_perc_avg))

daily_activity <- merge(daily_activity, daily_sleep, by=("id"))


healthy_habits_top <- filter(daily_activity, user_classification == "top_user")
healthy_habits_regular <- filter(daily_activity, user_classification == "regular_user")
healthy_habits_ocassional <- filter(daily_activity, user_classification == "ocassional_user")



    ## Daily Steps

ggplot(data = healthy_habits_top) +
  aes(x = date, y = totalsteps, group = id, colour = as.factor(id)) +
  geom_line() 
  
ggplot(data = healthy_habits_regular) +
  aes(x = date, y = totalsteps, group = id, colour = as.factor(id)) +
  geom_line() 
  
ggplot(data = healthy_habits_ocassional) +
  aes(x = date, y = totalsteps, group = id, colour = as.factor(id)) +
  geom_line() 





write.csv (daily_activity, file = "daily_activity_processed.csv")




