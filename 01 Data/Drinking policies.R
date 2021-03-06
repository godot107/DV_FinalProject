require("jsonlite")
library("RCurl")
require(dplyr)
require("ggplot2")
library(extrafont)

#setwd("C:/Users/willieman/Desktop/Skool/College Homework/_Senior Year/Spring 2015/CS 329e/DataVisualization/Final_Project/0 Doc/Drinking policies")
setwd("C:/Users/willieman/Desktop/Skool/College Homework/_Senior Year/Spring 2015/CS 329e/DataVisualization/DV_FinalProject/0 Doc")

#file_path <- "Legal-BAC-limits - Copy.csv"
#file_path <- "Penalties for drink driving - Copy.csv"
#file_path <- "Random breath testing - Copy.csv"
#file_path <- "Sobriety checkpoints - Copy.csv"

df <- read.csv(file_path, stringsAsFactors = FALSE)
names(df) <- gsub("\\.+", "_", names(df))

str(df)

#measures <- c("Year","General_population", "Young_novice_drivers","Professional_commercial_drivers" )

measures <- c("Year")

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

#------------------------------



