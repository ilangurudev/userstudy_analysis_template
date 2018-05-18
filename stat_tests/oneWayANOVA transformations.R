# two-way factorial ANOVA for between-subjects design
# this script has some summary statistics calculations removed because this is scary after

datafilename="./data/key_info.csv"	# csv file name

dv = "scr30_percent"			# dependent variable
independentVariable1 = "mode"		# independent variable	 


# new function for getting column by header
getColumnByName = function(dataframe, colName){return(dataframe[[which(colnames(dataframe) == colName)]])}

df = read.csv(datafilename,header=T)   #read the data into a data frame using the header row
df = df[complete.cases(df[, dv]),]	
iv1 = getColumnByName(df, independentVariable1)	 #create handles to the data rows we care about
iv1 = factor(iv1)
outcome = getColumnByName(df, dv)

#create field with reversed values to help with transformations for negatively skewed distributions
# maxOutcome = max(outcome)
# df = within(df, {reversed = maxOutcome - outcome})	
# reversedOutcome = getColumnByName(df, "reversed")

# try transformations:

#for positive skew (skewed right. tail on the right)
#outcome = log(outcome+1)		# the +1 is just because log(0) is not defined
#outcome = reversedOutcome^2
#outcome = sqrt(outcome)
#outcome = 1/(reversedOutcome +1)		# the +1 is to avoid division by 0
#outcome = 1/((reversedOutcome+1)^2)

#for negative skews (skewed left. tail on the left). just use reversed outcome
#Remember to interpret results as reversed for interpretation!
#outcome = outcome^2
#outcome = log(reversedOutcome + 1)		# the +1 is just because log(0) is not defined
#outcome = sqrt(reversedOutcome)
#outcome = 1/(outcome+1)	# the +1 is to avoid division by 0	
#outcome = 1/((outcome+1)^2)

#kurtosis transformations
#outcome = 1/(outcome^3)
#outcome = 1/sin(outcome)

#check normality
par(mfrow=c(1,2))
qqnorm(outcome)			#normal QQ plot (should be straight diagonal for normal)
hist(outcome,breaks=length(outcome))	#histogram
shapiro.test(outcome)
ks.test(outcome, "pnorm", mean=mean(outcome), sd=sd(outcome))
#Don't worry about ties warning\n"
#

# test for homogeneity of variance.
# NOTE 1: bartlett test is only reliable for normal data.
# NOTE: this stupid function will crash and break R if there are unequal numbers of results in conditions
#bartlett.test(outcome ~ independentVariable1 , data=df)

# calculate means for condition cells (combinations of conditions)
tapply(outcome, df[ , independentVariable1], mean) 

# calculate SD for condition cells (combinations of conditions)
tapply(outcome, df[ , independentVariable1], sd) 

# standard deviation overall
sd(outcome, na.rm = FALSE)

# mean overall
mean(outcome, na.rm = FALSE)

# ANOVA. note: missing values omitted by default for ANOVA																
aov.out = aov(outcome ~ iv1, data=df)    #do the analysis of variance
aov.out  		   #SHOW ANOVA MODEL
summary(aov.out)   #SHOW ANOVA summary table
# don't need to worry about SS types for one-way model

# give both eta squared and partial eta squared (type 1, 2, or 3 ANOVAs are same for one-way)
#install.packages("lsr")
library("lsr")
etaSquared(aov.out, type = 3, anova = TRUE)	# give both eta squared and partial eta squared

print(model.tables(aov.out,"means"), digits=4)    #report the means and the number of subjects/cell

# post hoc tests
# Bonferroni has more power for smaller number of comparisons. Tukey better for larger number of groups
# paired t test
pairwise.t.test(outcome, iv1, p.adj = "none", data=df) #non-corrected t test	
# Bonferroni t
pairwise.t.test(outcome, iv1, p.adj = "bonf", data=df) #Bonferroni corrected
# Tukey
TukeyHSD(aov.out) # Tukey HSD post hoc

# overview graphs
boxplot(outcome ~ iv1, data=df)


kruskal.test(outcome ~ iv1, data=df) 


library(PMCMR)

(pairwise.wilcox.test(x = outcome, g = iv1, p.adj = "bonf"))
posthoc.kruskal.dunn.test(outcome, iv1, p.adj = "bonf")
posthoc.kruskal.nemenyi.test(outcome, iv1, p.adj = "bonf")
