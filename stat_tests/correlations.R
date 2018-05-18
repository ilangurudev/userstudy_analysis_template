# two-way factorial ANOVA for between-subjects design
# this script has some summary statistics calculations removed because this is scary after

datafilename="./data/key_info.csv"	# csv file name

dependentVar1 = "scr30_percent"			# dependent variable 1
dependentVar2 = "duration_main"								# dependent variable 2

# new function for getting column by header
getColumnByName = function(dataframe, colName){return(dataframe[[which(colnames(dataframe) == colName)]])}

df = read.csv(datafilename,header=T)   #read the data into a data frame using the header row
df = df[complete.cases(df[, dependentVar1]),]	   # exclude rows where missing a value for the dependent variable
df = df[complete.cases(df[, dependentVar2]),]	   # exclude rows where missing a value for the dependent variable

dv1 = getColumnByName(df, dependentVar1)	 #create handles to the data rows we care about
dv2 = getColumnByName(df, dependentVar2)

#pearson
cor.test(dv1, dv2, use="all.obs", method="pearson")

#spearman
cor.test(dv1, dv2, use="all.obs", method="spearman")

# overview graphs
reg1 <- lm(dv1~dv2)
plot(dv1, dv2 ,asp=1, xlab= dependentVar1, ylab= dependentVar2)
abline(reg1)
