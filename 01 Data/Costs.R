require("jsonlite")
library("RCurl")
require(dplyr)
require("ggplot2")
library(extrafont)

setwd("C:/Users/willieman/Desktop/Skool/College Homework/_Senior Year/Spring 2015/CS 329e/DataVisualization/DV_FinalProject/0 Doc/Revenue")

file_path <- "Cost2.csv"
#file_path <- "Alcoholic beverage tax revenue - Copy.csv"
#file_path <- "Annual revenues from alcohol excise tax - Copy.csv"



df <- read.csv(file_path, stringsAsFactors = FALSE)
names(df) <- gsub("\\.+", "_", names(df))
str(df)

measures <- c("Year","Total_costs", "Direct_law_costs","Direct_health_costs",  "Indirect_costs" )
#measures <- c("X2013","X2012", "X2011", "X2010", "X2009", "X2008" ,"X2007", "X2006", "X2005","X2004", "X2003"  ,"X2002", "X2001"  ,"X2000", "X1999",  "X1998",  "X1997",  "X1996", "X1995", "X1994", "X1993", "X1992", "X1991", "X1990")

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

# Ends Reformating Data Frames

Costs <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from cost"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_wkm285', PASS='orcl_wkm285',MODE='native_mode',MODEL='model',returnDimensions = 'False',returnFor = 'JSON'),verbose = TRUE)))
names(Costs)

#Bar Chart of Total Cost by Country
ggplot(Costs, aes(x = COUNTRY, y = TOTAL_COSTS)) + geom_bar(stat ="identity") + theme(plot.title = element_text(size=20, face = "bold" , vjust=2)) + theme(axis.text.x=element_text(angle=70, size=7, vjust=0.5)) 
names(Costs)


mdf_Costs <- melt(Costs, id.vars = "COUNTRY", measure.vars = c("DIRECT_LAW_COSTS","DIRECT_HEALTH_COSTS", "DIRECT_OTHER_COSTS", "INDIRECT_COSTS" ))

#Bar Chart MDF
ggplot(mdf_Costs, aes(x =COUNTRY, y = value,  fill=variable))+ geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5))

