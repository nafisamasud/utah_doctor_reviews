# utah_doctor_reviews
A visualization of the data collected by University of Utah healthcare physician reviews. Dataset in separate file, code below.

### Doctor data analysis

library(tidyverse)

# setwd("~/Desktop/ds/after-metis/frontanalytics/internship")
df <- read_csv('updated_doc_data.csv')  # Load the dataset, assumed in the same location as doctors.R

# The long way
df$`Likelihood of recommending care provider` <- as.numeric(df$`Likelihood of recommending care provider`)
df$`My confidence in care provider` <- as.numeric(df$`My confidence in care provider`)
df$`Care provider's concern for questions & worries` <- as.numeric(df$`Care provider's concern for questions & worries`)
df$`Care provider spoke using clear language` <- as.numeric(df$`Care provider spoke using clear language`)
df$`Time care provider spent with me` <- as.numeric(df$`Time care provider spent with me`)
df$`Care provider's friendliness and courtesy` <- as.numeric(df$`Care provider's friendliness and courtesy`)
df$`Wait time at clinic` <- as.numeric(df$`Wait time at clinic`)
df$`Care provider's effort to include me in decisions` <- as.numeric(df$`Care provider's effort to include me in decisions`)
df$`Care provider's explanation of condition/problem` <- as.numeric(df$`Care provider's explanation of condition/problem`)

high_lvl_specialties <- c('Family Medicine','Family Nurse Practitioner', 'General Pediatrics',
                          'Geriatrics','Internal Medicine, General','Internal Medicine/Pediatrics',
                          'OB/GYN Nurse Practitioner','OB/Gyn, General','Physician Assistant',
                          'Preventive Medicine',"Women and Children's Health",
                          "Women's Health Care Nurse Practitioner")

df %>% 
  group_by(location) %>% 
  summarise(avg_by_location = mean(`Wait time at clinic`, na.rm=T)) %>% 
  filter(avg_by_location > 0) %>% 
  ggplot(aes(x=reorder(location, avg_by_location), y=avg_by_location, fill=location,
             label=round(avg_by_location, digits = 1))) +
    geom_bar(stat='identity') + 
    geom_text(hjust = 1.1, color='white', size=3.5) +
    theme_minimal() + 
    ggtitle("Average Wait Time Rating by Facility") + 
    coord_flip(ylim = c(1, 5)) + xlab("") + ylab("Average Wait Time Rating") +
    theme(legend.position = 'none')
  
# Average Wait Time by Specialty Ratings
p <- df %>% 
  group_by(specialty) %>% 
  summarise(avg_by_specialty = mean(`Wait time at clinic`, na.rm = T),
            count_of_reviews = n()) %>% 
  filter(count_of_reviews > 3, avg_by_specialty > 1) %>% 
  filter(specialty %in% high_lvl_specialties) %>% 
  ggplot(aes(x=reorder(specialty, avg_by_specialty), y=avg_by_specialty,
             label=round(avg_by_specialty, digits = 1), fill=specialty)) +
    geom_bar(stat = 'identity') +
    geom_text(hjust = 1.1, color='white', size=3.5) + 
    theme_minimal() + 
    ggtitle("Average Wait Time by Specialty Ratings") + 
    coord_flip(ylim = c(0, 5)) + xlab("") + ylab("")
p + theme(legend.position = 'none')

# Languages by specialties
l <- df %>% 
  group_by(specialty) %>% 
  summarise(n_languages = n_distinct(language),
            n_doctors = n_distinct(name)) %>% 
  mutate(normed = n_languages / n_doctors) %>% 
  filter(specialty %in% high_lvl_specialties) %>% 
  select(specialty, n_languages, n_doctors, normed) %>% 
  arrange(desc(n_languages)) %>% 
  ggplot(aes(x=reorder(specialty, normed), y=normed, fill=specialty)) +
      geom_bar(stat="identity") +
      theme_minimal() +
      ggtitle("Number of Languages spoken by Specialty") + 
      #labs(xlab(""), ylab("")) +
      coord_flip(ylim = c(0, 2)) + xlab("") + ylab("# languages / # doctors")

l  + theme(legend.position = 'none')
  
# Trying the impossible
df %>% 
  group_by(gender, language) %>% 
  filter(gender != '?', language != 'English') %>%  # Filtering out the English b/c so many
  summarise(l_count = n()) %>% 
  select(gender, language, l_count) %>% 
  ggplot(aes(x=gender, y=l_count, fill=language, label=language)) + 
    geom_bar(stat='identity', color="black", size=0.2) +
    theme_minimal() +
    ggtitle("Language diversity by number of doctors, gender, excluding English") +
    theme(legend.position = 'top') +
    ylab("Number of doctors") + xlab("")
  
