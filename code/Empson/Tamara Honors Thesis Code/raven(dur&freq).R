library(readxl)
library(tidyverse)
install.packages("janitor")
library(janitor)

raven.test <- read.delim("~/Desktop/raven/selection tables/T247_April_11_2022_4.Table.1.selections.txt") %>% clean_names() %>% 
  separate(begin_file, sep = "_", into=c("bird_id", "month", "day", "year", "song_n"), remove=F) %>%
  mutate(date=paste(month,day,year, sep="-"), song_n = str_remove(song_n, pattern=".wav")) %>%
  relocate(bird_id,
           date, 
           song_n, 
           selection, 
           syllable_type);raven.test

getwd()
setwd("~/Desktop/raven/selection tables")
list.files(pattern = ".selections.txt")

raven <- tibble()

for(i in 1:length(list.files(pattern = ".selections.txt"))) {
  a.file <- list.files(pattern = ".selections.txt")[i]
 read.it <- read.delim(a.file) %>% clean_names() %>% 
  separate(begin_file, sep = "_", into=c("bird_id", "month", "day", "year", "song_n"), remove=F) %>%
  mutate(date=paste(month,day,year, sep="-"), song_n = str_remove(song_n, pattern=".wav")) %>%
  relocate(bird_id,
           date, 
           song_n, 
           selection)
 
 raven<- bind_rows(raven, read.it) }
raven


   treatment.info <- read.csv("~/Desktop/raven/selection tables/2025-11-25_count_summaries.csv") %>% 
     select(male:round, chamber:humidity_mean) %>% 
     separate(date, sep="-", into =c("year", "month", "day")) %>% 
     mutate (bird_id = male,
             date = paste(month,day,year, sep="-")) %>% 
     select(-year, -month, -day)
   
  full_join(raven, treatment.info)
 glimpse(treatment.info)
   

#messing around 
   dat <- full_join(raven, treatment.info,
                    by = c("bird_id", "date"))
glimpse(dat)

summary(dat$temp_mean)
summary(dat$bird_id)

dat <- full_join(raven, treatment.info) %>%
  filter(!is.na(bird_id), !is.na(date)) %>%   # remove bad rows 
  mutate(
    duration = end_time_s - begin_time_s)  
dat <- dat %>%
  mutate(
    temp_mean = as.factor(temp_mean),
    bird_id = as.factor(bird_id),
    song_n = as.factor(song_n))

library(lme4)
m1 <- lmer(duration ~ temp_target + (1 | bird_id), data = dat)
summary(m1)
