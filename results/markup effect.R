# two-way factorial ANOVA for between-subjects design
# this script has some summary statistics calculations removed because this is scary after

#removes all previous data in the workspace
rm(list = ls())

pacman::p_load(tidyverse, lsr, broom, PMCMR)


datafilename <- "data/key_info.csv"	# csv file name

dv <- quo(scr30_percent)			# dependent variable
iv <- quo(mode)		# independent variable	 

norm_test_threhold <- 0.05

df <- suppressMessages(read_csv(datafilename))    #read the data into a data frame using the header row

df <- 
  df %>% 
  filter(mode %in% c(1,2))

df <- 
  df %>% 
  filter(!is.na(!!dv))

# iv = getColumnByName(df, independentVariable1)	 #create handles to the data rows we care about
iv_fac <- df %>% pull(!!iv) %>% factor()
outcome <- df %>% pull(!!dv) 


t.test(df$scr36[df$mode == 1], df$scr36[df$mode == 2])

