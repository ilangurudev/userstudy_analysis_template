
#removes all previous data in the workspace
rm(list = ls())


if(!"fifer" %in% installed.packages()){
  pacman::p_install_gh("cran/fifer")
} else {
  pacman::p_load(tidyverse, broom, fifer)
}

#path of data file
# data_file_name <- "data/key_info.csv"
data_file_name <- "data/question_answers.csv"

# Enter name of variable 1 inside the paranthesis without quotes. 
# Eg: quo(mode)
iv1 <- quo(grade)

# Enter name of variable 2 inside the paranthesis without quotes. 
iv2 <- quo(conf_lh)

#importing
data_exp <- suppressMessages(read_csv(data_file_name))

data_exp <- data_exp %>% mutate(conf_lh = if_else(confidence_recoded == 1, "low", "high") %>% as.factor())

#a chisq test that you can use for any variable. 
#(currently considers confidence as a 3 factor variable)

chisq_data <-
  data_exp %>%
    dplyr::select((!!iv1),(!!iv2)) %>%
    table() %>% 
    print()

chisq_results <- 
  chisq_data %>%
    chisq.test()

message("Chi-squared results")
print(tidy(chisq_results) %>% rename(df = parameter))


#posthoc test
message("Chi-squared Posthoc results (fisher test and bonferroni control)")
(chisq_results_posthoc <- 
  chisq_data %>%
    fifer::chisq.post.hoc(test = c("fisher.test"), control = "bonferroni") %>% 
    print())

