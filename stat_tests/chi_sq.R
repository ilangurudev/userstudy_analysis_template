install.packages("dplyr")

library(tidyverse)
library(broom)

#path of data file
# data_file_name <- "./data/question_answers.csv"
data_file_name <- "question_answers.csv"

#importing
data_exp <- read_csv(data_file_name)

#name of variable 1 (unquoted)
iv1 <- quo(mode)

#name of variable 2 (unquoted)
iv2 <- quo(confidence_recoded)


#a chisq test that you can use for any variable. 
#(currently considers confidence as a 3 factor variable)

(chisq_data <- 
  data_exp %>%
    select(!!iv1,!!iv2) %>%
    table())


chisq_data  %>%
  chisq.test()


(chisq_results <- 
  chisq_data %>%
    chisq.test())

tidy(chisq_results)


#posthoc test
(chisq_results_posthoc <- 
  chisq_data %>%
    fifer::chisq.post.hoc(test = c("fisher.test"), control = "bonferroni") )


#############################################################################################################

#threshold for making iv2 a binary value (specific to the variable)
threshold <-  2

#preparing the data with "high" and "low" confidence levels for a chisq test
#specific to the data
#if no need for such aggregation for the chisq test, comment the next 6 lines
chisq_data <- 
  data_exp %>%
  select(!!iv1,!!iv2) %>%
  mutate(x1 = ifelse((!!iv2) >= threshold, "high", "low")) %>%
  select(-!!iv2) %>% 
  table()


chisq.test(chisq_data[c(1,2),])
chisq.test(chisq_data[2:5,])

(conf_all <- 
    data_exp %>%
    select(!!iv1,!!iv2) %>%
    group_by(!!iv1) %>% 
    summarise(high_percent = sum((!!iv2) >= threshold)/n(),
              low_percent = sum((!!iv2) < threshold)/n()))

conf_all %>%
  write_csv("percent_high_low.csv")
