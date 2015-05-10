require("jsonlite")
library("RCurl")
require(dplyr)
require("ggplot2")
library(extrafont)
library("reshape2")

# Reformatting the Table Frame abstainerslifetime and 12 Months
setwd("~/DV_FinalProject/0 Doc/Join these tables together!")

#file_path <- "Abstainers-lifetime-Copy.csv"
file_path <- "Abstainers-past-12-months - Copy.csv"

df <- read.csv(file_path, stringsAsFactors = FALSE)
names(df) <- gsub("\\.+", "_", names(df))
str(df)

measures <- c("Year","Male", "Female","Bothsexes"  )

for(n in names(df)) {
  df[n] <- data.frame(lapply(df[n], gsub, pattern="[^ -~]",replacement= ""))
}

dimensions <- setdiff(names(df), measures)
for(d in dimensions) {
  # Get rid of " and ' in dimensions.
  df[d] <- data.frame(lapply(df[d], gsub, pattern="[\"']",replacement= ""))
  # Change & to and in dimensions.
  df[d] <- data.frame(lapply(df[d], gsub, pattern="&",replacement= " and "))
  # Change : to ; in dimensions.
  df[d] <- data.frame(lapply(df[d], gsub, pattern=":",replacement= ";"))
}

for(m in measures) {
  df[m] <- data.frame(lapply(df[m], gsub, pattern="[^--.0-9]",replacement= ""))
}

write.csv(df, paste(gsub(".csv", "", file_path), ".reformatted.csv", sep=""), row.names=FALSE, na = "")

tableName <- gsub(" +", "_", gsub("[^A-z, 0-9, ]", "", gsub(".csv", "", file_path)))
sql <- paste("CREATE TABLE", tableName, "(\n-- Change table_name to the table name you want.\n")
for(d in dimensions) {
  sql <- paste(sql, paste(d, "varchar2(4000),\n"))
}
for(m in measures) {
  if(m != tail(measures, n=1)) sql <- paste(sql, paste(m, "number(38,4),\n"))
  else sql <- paste(sql, paste(m, "number(38,4)\n"))
}
sql <- paste(sql, ");")
cat(sql)



# End Reframing the Data

abstainerslifetime <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from ABSTAINERSLIFETIME"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_wkm285', PASS='orcl_wkm285',MODE='native_mode',MODEL='model',returnDimensions = 'False',returnFor = 'JSON'),verbose = TRUE)))

abstainers12month <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from abstainerspast12months"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_wkm285', PASS='orcl_wkm285',MODE='native_mode',MODEL='model',returnDimensions = 'False',returnFor = 'JSON'),verbose = TRUE)))

# I am assuming x is lifetime abstainers and y is 12 month abstainers 
abstainers <- full_join(abstainerslifetime, abstainers12month, by = "COUNTRY")
tbl_df(abstainers)
#tbl_df(abstainers)
#colnames(abstainers)

mdf_abstainers <- melt(abstainers, id.vars = "COUNTRY", measure.vars = c("MALE.x", "FEMALE.x", "BOTHSEXES.x", "YEAR.y", "MALE.y", "FEMALE.y" ,"BOTHSEXES.y"))
tbl_df(mdf_abstainers)


ggplot(data = abstainers) + geom_histogram(aes(x = MALE.x), binwidth = 5) + xlab("Male Abstainers Life Time") + ylab("Count")
ggplot(data = abstainers) + geom_histogram(aes(x = MALE.y), binwidth = 5) + xlab("Male Abstainers 12 Months") + ylab("Count")
ggplot(data = abstainers) + geom_histogram(aes(x = FEMALE.x), binwidth = 5) + xlab("Female Abstainers Life Time") + ylab("Count")
ggplot(data = abstainers) + geom_histogram(aes(x = FEMALE.y), binwidth = 5) + xlab("Female Abstainers 12 Months") + ylab("Count")
ggplot(data = abstainers) + geom_histogram(aes(x = BOTH_SEXES.x, binwidth = 5)) + xlab("Both Genders Abstainers Life Time") + ylab("Count")
ggplot(data = abstainers) + geom_histogram(aes(x = BOTH_SEXES.y, binwidth = 5)) + xlab("Both Genders Abstainers 12 Months") + ylab("Count")





