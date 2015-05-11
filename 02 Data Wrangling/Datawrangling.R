require("jsonlite")
library("RCurl")
require(dplyr)
require(tidyr)
require("ggplot2")
library(extrafont)
library("reshape2")


setwd("C:/Users/willieman/Desktop/Skool/College Homework/_Senior Year/Spring 2015/CS 329e/DataVisualization/DV_FinalProject/0 Doc")


file_path <- "drinking_policies.csv"

df <- read.csv(file_path, stringsAsFactors = FALSE)
names(df) <- gsub("\\.+", "_", names(df))
str(df)


measures <- c("General_population",  "Young_novice_drivers",	"commercial_drivers")


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

# End Reformatting the Data

drinking_policies <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from drinking_policies"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_wkm285', PASS='orcl_wkm285',MODE='native_mode',MODEL='model',returnDimensions = 'False',returnFor = 'JSON'),verbose = TRUE)))


abstainerslifetime <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from ABSTAINERSLIFETIME"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_wkm285', PASS='orcl_wkm285',MODE='native_mode',MODEL='model',returnDimensions = 'False',returnFor = 'JSON'),verbose = TRUE)))


CONSUMPTION_OF_PURE_ALCOHOL <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from CONSUMPTION_OF_PURE_ALCOHOL"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_wkm285', PASS='orcl_wkm285',MODE='native_mode',MODEL='model',returnDimensions = 'False',returnFor = 'JSON'),verbose = TRUE)))


fulljoin <- full_join(drinking_policies, abstainerslifetime, by = "COUNTRY") 
fulljoin2 <- full_join(drinking_policies, CONSUMPTION_OF_PURE_ALCOHOL, by = "COUNTRY") 


tbl_df(fulljoin2)

fulljoin %>% select(COUNTRY, BOTHSEXES,PATTERNS_OF_DRINKING_SCORE,SOBRIETY_CHECKPOINTS) %>% filter(PATTERNS_OF_DRINKING_SCORE %in% c("2- somewhat risky","3- medium risky","4- very risky"), SOBRIETY_CHECKPOINTS == "Yes") %>% ggplot(aes(x = COUNTRY, y = BOTHSEXES,fill=PATTERNS_OF_DRINKING_SCORE))+ geom_bar(stat="identity") + theme(plot.title = element_text(size=20, face = "bold" , vjust=2)) + theme(axis.text.x=element_text(angle=70, size=10, vjust=0.5)) 


# Does Beer Consumption become affected to random Breath Testing?
fulljoin2 %>% select(COUNTRY, BEER,RANDOM_BREATH_TESTING_RBT_USE) %>% filter(RANDOM_BREATH_TESTING_RBT_USE %in% c("Yes")) %>% ggplot(aes(x = COUNTRY, y = BEER))+ geom_bar(stat="identity") + theme(plot.title = element_text(size=20, face = "bold" , vjust=2)) + theme(axis.text.x=element_text(angle=70, size=10, vjust=0.5)) 

fulljoin2 %>% select(COUNTRY, BEER,RANDOM_BREATH_TESTING_RBT_USE) %>% filter(RANDOM_BREATH_TESTING_RBT_USE %in% c("No")) %>% ggplot(aes(x = COUNTRY, y = BEER))+ geom_bar(stat="identity") + theme(plot.title = element_text(size=20, face = "bold" , vjust=2)) + theme(axis.text.x=element_text(angle=70, size=10, vjust=0.5)) 

# Looking for some correlation between Beer consumption and BPA limit, should i perform a t-test?
fulljoin2 %>% ggplot(aes(x = BEER, y = GENERAL_POPULATION)) + theme(plot.title = element_text(size=20, face = "bold" , vjust=2)) + theme(axis.text.x=element_text(angle=70, size=10, vjust=0.5)) + geom_point()

fulljoin2 %>% ggplot(aes(x = GENERAL_POPULATION, y = BEER)) + theme(plot.title = element_text(size=20, face = "bold" , vjust=2)) + theme(axis.text.x=element_text(angle=70, size=10, vjust=0.5)) + geom_point()

tbl_df(drinking_policies)
