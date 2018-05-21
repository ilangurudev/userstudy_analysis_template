library(magrittr)
library(tidyverse)
library(broom)

#path of data file
# data_file_name <- "./data/question_answers.csv"
data_file_name <- "question_answers.csv"

#importing
data_exp <- read_csv(data_file_name)

#threshold for making iv2 a binary value (specific to the variable)
threshold <-  2

#preparing the data with "high" and "low" confidence levels for a chisq test
#specific to the data
#if no need for such aggregation for the chisq test, comment the next 6 lines

subset_data <- function(filt_grade = c(0,1),modes = 1:5){
  chisq_data <- 
    data_exp %>%
    filter(grade == filt_grade) %>% 
    filter(mode %in% modes) %>% 
    select(mode,confidence_recoded) %>%
    mutate(confidence_recoded = ifelse(confidence_recoded >= threshold, "high", "low")) %>%
    table() %T>% print()
}


chisq.test(subset_data(filt_grade = 0, modes = 1:2))
fifer::chisq.post.hoc(subset_data(filt_grade = 0, modes = 1:2),
                      test = c("fisher.test"), control = "bonferroni")

chisq.test(subset_data(filt_grade = 1, modes = 1:2))
fifer::chisq.post.hoc(subset_data(filt_grade = 1, modes = 1:2),
                      test = c("fisher.test"), control = "bonferroni")

chisq.test(subset_data(filt_grade = 0, modes = 2:5))
fifer::chisq.post.hoc(subset_data(filt_grade = 0, modes = 2:5),
                      test = c("fisher.test"), control = "bonferroni")

chisq.test(subset_data(filt_grade = 1, modes = 2:5))
fifer::chisq.post.hoc(subset_data(filt_grade = 1, modes = 2:5),
                      test = c("fisher.test"), control = "bonferroni")

