# two-way factorial ANOVA for between-subjects design
# this script has some summary statistics calculations removed because this is scary after

#removes all previous data in the workspace
rm(list = ls())

pacman::p_load(tidyverse, lsr, broom, PMCMR)


datafilename <- "data/key_info.csv"	# csv file name

dv <- quo(mean_confidence)			# dependent variable
iv <- quo(mode)		# independent variable	 

norm_test_threhold <- 0.05

df <- suppressMessages(read_csv(datafilename))    #read the data into a data frame using the header row

df <- 
  df %>% 
  filter(mode != 1)

df <- 
  df %>% 
  filter(!is.na(!!dv))

# iv = getColumnByName(df, independentVariable1)	 #create handles to the data rows we care about
iv_fac <- df %>% pull(!!iv) %>% factor()
outcome <- df %>% pull(!!dv) 

#check normality
cat("\n")
message("--------------------------------------------------------------------------------------")
message("----------------------------------Test for Normality----------------------------------")
par(mfrow=c(1,2))
qqnorm(outcome)			#normal QQ plot (should be straight diagonal for normal)
hist(outcome,breaks=length(outcome))	#histogram
sh <- shapiro.test(outcome) %>% print()
ks.test(outcome, "pnorm", mean=mean(outcome), sd=sd(outcome)) %>% print()

if(sh$p.value < 0.05){
  message("Normality satisfied according to Shapiro-Wilk")
} else {
  message("Normality NOT satisfied according to Shapiro-Wilk")
}


cat("\n")
message("--------------------------------------------------------------------------------------")
message("------------------------------------Means and SDs------------------------------------")
# mean overall
cat("\n")
message("mean overall")
mean(outcome, na.rm = FALSE) %>% print()

# calculate means for condition cells (combinations of conditions)
cat("\n")
message("means of outcome by levels of independent variable")
tapply(outcome, iv_fac, mean)  %>% print()

# standard deviation overall
cat("\n")
message("SD overall")
sd(outcome, na.rm = FALSE) %>% print()

# calculate SD for condition cells (combinations of conditions)
cat("\n")
message("SDs of outcome by levels of independent variable")
tapply(outcome, iv_fac, sd)  %>% print()


cat("\n")
message("--------------------------------------------------------------------------------------")
message("------------------------------------Non-paramteric tests------------------------------------")

message("Krushal Wallis test")
kruskal.test(outcome ~ iv_fac) %>% suppressMessages() %>% print()

