library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(infer)

# ENGAGEMENT: ----

## Data
survey <- read_csv("Data/survey_data.csv")

## Exploratory Data Analysis
# -- summary
survey %>% skimr::skim()
survey %>% glimpse()
survey %>% count(department)

## Visual: Average Engagement By Department
survey %>% 
  group_by(department) %>% 
  summarise(avg_engagement = mean(engagement)) %>% 
  ggplot(aes(department, avg_engagement)) +
  geom_col() + geom_hline(yintercept = 3.05, color = "red") + 
  ggtitle("Average Engagement ~ Department")

## Visual: Survey Summary By Department
# - feature engineering
survey_disengaged <- survey %>% 
  mutate(disengaged = ifelse(engagement <= 2, 1,0)) %>% 
  group_by(department) %>% 
  summarise(pct_disengaged = mean(disengaged),
            avg_salary = mean(salary),
            avg_vacation_days = mean(vacation_days_taken))
# - gather
survey_disengaged %>% 
  gather(key = "measure", value = "value",
         pct_disengaged, avg_salary, avg_vacation_days) %>% 
  # visual
  ggplot(aes(measure, value, fill = department)) +
  geom_col(position = "dodge") +
  facet_wrap(~measure, scales = "free") +
  ggtitle("Survey Summary ~ Departments")

## Visual: Sales vs Others ~ Engagement
# - Filter Sales
survey_SALES <- survey %>% 
  mutate(disengaged = factor(ifelse(engagement <= 2, 1,0)),
         in_sales = ifelse(department == "Sales","Sales","Other"))
# - visual
survey_SALES %>% 
  ggplot(aes(in_sales, fill = disengaged)) +
  geom_bar(position = "fill") +
  ggtitle("Is different engagement with Sales Significant ?")
# - stat test: Significant
survey_SALES %>% chisq_test(disengaged ~ in_sales)

# Visual: Sales vs Others ~ Vacation days taken
survey_SALES %>% 
  ggplot(aes(in_sales, vacation_days_taken)) +
  geom_boxplot() +
  ggtitle("Is Vacation Days between Sales Significant ?")
# - stat test: Significant
survey_SALES %>% t_test(vacation_days_taken ~ in_sales)

