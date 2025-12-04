

install.packages("av")
library(av)
library(tidyverse)

setwd("/Volumes/ZEFI_Videos/clipped songs")
files <- list.files(pattern = ".wav")
new.data<- data.frame(NA,NA)
colnames(new.data) <- c("i.file","duration")
for (i in 1:length(files)) {
    tryCatch(
        expr = {
            i.file <- files %>% head(i) %>% tail(1)
            i.info <- av_media_info(i.file)
            duration<- i.info$duration
            new.data<- new.data %>% add_row(i.file,duration)
        },
        error = function(cond){
            duration<- NA
            new.data<- new.data %>% add_row(i.file,duration)
        }
    )
}

setwd("/Users/taraempson/Library/CloudStorage/OneDrive-UniversityofTennessee/Derryberry Lab Drive/Projects not within a Field Season/Captive Projects by Starting Year/Captive 2022/KIM (ZEFI Song Heat)/Tamara Scoring Spring 2024/clipped songs (2)")
files <- list.files(pattern = ".wav")
for (i in 1:length(files)) {
    tryCatch(
        expr = {
            i.file <- files %>% head(i) %>% tail(1)
            i.info <- av_media_info(i.file)
            duration<- i.info$duration
            new.data<- new.data %>% add_row(i.file,duration)
        },
        error = function(cond){
            duration<- NA
            new.data<- new.data %>% add_row(i.file,duration)
        }
    )
}

new.data <- new.data %>% mutate(clipped_file_name = str_remove_all(i.file, ".wav")) %>% filter(!is.na(duration)) %>% select(clipped_file_name,duration)




