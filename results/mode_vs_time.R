# two-way factorial ANOVA for between-subjects design
# this script has some summary statistics calculations removed because this is scary after

#removes all previous data in the workspace
rm(list = ls())

pacman::p_load(tidyverse, lsr, broom, PMCMR)


datafilename <- "data/key_info.csv"	# csv file name

dv <- quo(duration_main)			# dependent variable
iv <- quo(mode)		# independent variable	 



df <- suppressMessages(read_csv(datafilename))    #read the data into a data frame using the header row

df <- df %>%
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

#Don't worry about ties warning\n"
#

# test for homogeneity of variance.
# NOTE 1: bartlett test is only reliable for normal data.
# NOTE: this stupid function will crash and break R if there are unequal numbers of results in conditions
#bartlett.test(outcome ~ independentVariable1 , data=df)

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



# ANOVA. note: missing values omitted by default for ANOVA		
cat("\n")
message("--------------------------------------------------------------------------------------")
message("-----------------------------------------ANOVA----------------------------------------")
aov.out <- aov(log(outcome) ~ iv_fac, data=df)    #do the analysis of variance
tidy(aov.out) %>% print()
# aov.out  		   #SHOW ANOVA MODEL
# summary(aov.out)   #SHOW ANOVA summary table
# don't need to worry about SS types for one-way model

if(tidy(aov.out)$p.value[1] < 0.05){
  cat("\n")
  message("-----------Effect detected by ANOVA-----------")
} else{
  cat("\n")
  message("-----------No evidence for effect detected by ANOVA-----------")
}

cat("\n")
message("Analysis on the ANOVA results")

# give both eta squared and partial eta squared (type 1, 2, or 3 ANOVAs are same for one-way)
message("ETA squared")
print(etaSquared(aov.out, type = 3, anova = TRUE))	# give both eta squared and partial eta squared

message("Model tables")
print(model.tables(aov.out,"means"), digits=4)    #report the means and the number of subjects/cell
