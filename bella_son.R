install.packages("skimr")
library(tidyverse)
library(skimr)
library(janitor)

activity <- read_csv("dailyActivity_merged.csv")
calories <- read_csv("dailyCalories_merged.csv")
heartrate <- read_csv("heartrate_seconds_merged.csv")
intensities <- read_csv("dailyIntensities_merged.csv")
sleep <- read_csv("sleepDay_merged.csv")
steps <- read_csv("dailySteps_merged.csv")
steps_hourly <- read_csv("hourlySteps_merged.csv")
weight <- read_csv("weightLogInfo_merged.csv")

head(activity)

head(sleep)

colSums(is.na(activity))

head(calories)

colSums(is.na(calories))

head(intensities)

colSums(is.na(intensities))

head(steps)
colSums(is.na(steps))


head(weight)

colSums(is.na(weight))

sum(duplicated(activity))
sum(duplicated(calories))
sum(duplicated(heartrate))
sum(duplicated(intensities))
sum(duplicated(sleep))
sum(duplicated(steps))
sum(duplicated(steps_hourly))
sum(duplicated(weight))

sleep <- sleep %>% 
  distinct()
sum(duplicated(sleep))

n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(heartrate$Id)
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(steps$Id)
n_distinct(steps_hourly$Id)
n_distinct(weight$Id)


head(activity)

head(intensities)

clean_names(activity)

activity <- rename_with(activity,tolower)

head(calories)

clean_names(calories)

calories <- rename_with(calories,tolower)

clean_names(intensities)
intensities <- rename_with(intensities, tolower)

clean_names(sleep)
sleep <- rename_with(sleep, tolower)

clean_names(steps)
steps <- rename_with(steps, tolower)

clean_names(steps_hourly)
steps_hourly <- rename_with(steps_hourly, tolower)

activity <- activity %>%
  rename(date = activitydate) %>% 
  mutate(id = as.character(id)) %>%
  mutate(date = mdy(date))


calories <- calories %>% 
  rename(date = activityday) %>% 
  mutate(id = as.character(id)) %>% 
  mutate(date = mdy(date))

intensities <- intensities %>% 
  rename(date = activityday) %>% 
  mutate(id = as.character(id)) %>% 
  mutate(date = mdy(date))

head(sleep)
head(steps)
head(steps_hourly)


sleep <- sleep %>% 
  rename(date = sleepday) %>% 
  mutate(id = as.character(id))

sleep <- sleep %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

steps <- steps %>% 
  rename(date = activityday) %>% 
  mutate(id = as.character(id)) %>% 
  mutate(date = mdy(date))

steps_hourly <- steps_hourly %>% 
  rename(date_time = activityhour) %>% 
  mutate(id = as.character(id)) %>% 
  mutate(date_time = mdy_hms(date_time))

steps_hourly <- steps_hourly %>% 
  separate(date_time, into = c("date", "time"), sep = " ") %>% 
  mutate(date = ymd(date))


head(activity)
head(sleep)

activity_sleep <- merge(activity, sleep, by = c("id", "date"))
head(activity_sleep)

calories_intensities <- merge(calories, intensities, by = c("id", "date"))

calories_intensities$totalminutes <- calories_intensities$lightlyactiveminutes + calories_intensities$fairlyactiveminutes 
+ calories_intensities$veryactiveminutes


calories_steps <- merge(calories, steps, by = c("id", "date"))

head(calories_steps)

head(calories_intensities)

ggplot(data = calories_intensities, mapping = aes(x = totalminutes, y = calories)) +
  geom_jitter() + geom_smooth(method = lm) + labs(title = "Active minutes vs Calories")

ggplot(data = calories_steps, mapping = aes(x = steptotal, y = calories)) +
  geom_jitter() + geom_smooth(method = lm) + labs(title = "Total steps vs Calories")

head(steps_hourly)

steps_hourly %>%
  group_by(time) %>%
  summarize(avg_steps = mean(steptotal)) %>%
  ggplot() +
  geom_bar(mapping = aes(x = time, y = avg_steps), stat = "identity") +
  labs(title = "Average Steps Hourly") +
  theme(axis.text.x = element_text(angle = 45))




