require("jsonlite")
library("RCurl")
require(dplyr)
require("ggplot2")
library(extrafont)
library("reshape2")

#setwd("C:/Users/willieman/Desktop/Skool/College Homework/_Senior Year/Spring 2015/CS 329e/DataVisualization/DV_FinalProject/0 Doc/Drunk Driving-statistics")
setwd("C:/Users/willieman/Desktop/Skool/College Homework/_Senior Year/Spring 2015/CS 329e/DataVisualization/DV_FinalProject/0 Doc")

#file_path <- "figure_01_2.csv"
#file_path <- "figure_02_2.csv"
file_path <- "figure_03_2.csv"

df <- read.csv(file_path, stringsAsFactors = FALSE)
names(df) <- gsub("\\.+", "_", names(df))
str(df)


#measures <- c("Year","Total_fatalities","Alcohol_related_fatalities", "Non_alcohol_related","Percentage")
#measures <- c("Year","Occupants","Pedestrians", "Pedalcyclists","Others_unknown","Occupants_Percent","Pedestrians_Percent", "Pedalcyclists_Percent","Others_unknown_Percent")
measures <- c("Year", "Total_Fatalities", "Alcohol_Use_Fatalities", "Without_Alcohol_Use", "Percentage")


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

# Data Frames
figure_01_2 <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from figure_01_2"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_wkm285', PASS='orcl_wkm285',MODE='native_mode',MODEL='model',returnDimensions = 'False',returnFor = 'JSON'),verbose = TRUE)))

figure_02_2 <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from figure_02_2"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_wkm285', PASS='orcl_wkm285',MODE='native_mode',MODEL='model',returnDimensions = 'False',returnFor = 'JSON'),verbose = TRUE)))

figure_03_2 <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from figure_03_2"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_wkm285', PASS='orcl_wkm285',MODE='native_mode',MODEL='model',returnDimensions = 'False',returnFor = 'JSON'),verbose = TRUE)))

# Plots
ggplot(figure_01_2, aes(x=YEAR, y=TOTAL_FATALITIES)) + geom_line()
ggplot(figure_01_2, aes(x=YEAR, y=ALCOHOL_RELATED_FATALITIES)) + geom_line()
ggplot(figure_01_2, aes(x=YEAR, y=NON_ALCOHOL_RELATED)) + geom_line()
ggplot(figure_01_2, aes(x=YEAR, y=PERCENTAGE)) + geom_line() # Try to fix this scale... quite misleadings

#Figure 1
# Reshaping the data with melt from reshape2
mdf <- melt(figure_01_2, id.vars = "YEAR", measure.vars = c("TOTAL_FATALITIES", "ALCOHOL_RELATED_FATALITIES","NON_ALCOHOL_RELATED"))
ggplot(mdf, aes(x=YEAR, y=value, color=variable)) + geom_line()
ggplot(mdf, aes(x =YEAR, y = value,  fill=variable))+ geom_bar(stat="identity")

#Figure 2
# Reshaping the data with melt from reshape2
mdf <- melt(figure_02_2, id.vars = "YEAR", measure.vars = c("OCCUPANTS", "PEDESTRIANS","PEDALCYCLISTS", "OTHERS_UNKNOWN" ))
ggplot(mdf, aes(x=YEAR, y=value, color=variable)) + geom_line()

ggplot(mdf, aes(x=YEAR, y=value, color=variable)) + geom_line()

ggplot(mdf, aes(x =YEAR, y = value,  fill=variable))+ geom_bar(stat="identity")


#Figure 3
# Reshaping the data with melt from reshape2
mdf <- melt(figure_03_2, id.vars = "YEAR", measure.vars = c("TOTAL_FATALITIES","ALCOHOL_USE_FATALITIES", "WITHOUT_ALCOHOL_USE" ))
ggplot(mdf, aes(x=YEAR, y=value, color=variable)) + geom_line()

#Figure 3
ggplot(figure_03_2, aes(x=YEAR, y=ALCOHOL_USE_FATALITIES))+ geom_bar(stat="identity")

mdf <- melt(figure_03_2, id.vars = "YEAR", measure.vars = c("ALCOHOL_USE_FATALITIES", "WITHOUT_ALCOHOL_USE" ))
ggplot(mdf, aes(x =YEAR, y = value,  fill=variable))+ geom_bar(stat="identity")
