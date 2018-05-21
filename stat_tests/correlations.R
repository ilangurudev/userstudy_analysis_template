# two-way factorial ANOVA for between-subjects design
# this script has some summary statistics calculations removed because this is scary after

#removes all previous data in the workspace
rm(list = ls())

pacman::p_load(tidyverse, broom)

datafilename <- "data/key_info.csv"	# csv file name

# Enter name of variable 1 inside the paranthesis without quotes. 
# Eg: quo(mode)
dv1 <- quo(scr30_percent)

# Enter name of variable 2 inside the paranthesis without quotes. 
dv2 <- quo(duration_main)

df = suppressMessages(read_csv(datafilename)) 

df <- 
  df %>% 
  filter(!is.na(!!dv1), !is.na(!!dv2))

dv_1 <- df %>% pull(!!dv1)
dv_2 <- df %>% pull(!!dv2)


#pearson
message("pearson correlation test")
cor.test(dv_1, dv_2, use="all.obs", method="pearson") %>% tidy() %>% print()

#spearman
message("spearman correlation test")
cor.test(dv_1, dv_2, use="all.obs", method="spearman") %>% tidy() %>% print()

# overview graphs
message("Plotting")
reg1 <- lm(dv_2 ~ dv_1)
plot(dv_1, dv_2 ,asp=1, xlab = as.character(dv1)[2], ylab = as.character(dv2)[2])
abline(reg1)
