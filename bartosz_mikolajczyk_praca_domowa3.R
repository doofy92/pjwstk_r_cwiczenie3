library(DBI)
library(RSQLite)
library(RPostgres)
library(rstudioapi)
library(tidyverse)

connectMe <- function(type=Postgres(), dbname="zykynaap", host="queenie.db.elephantsql.com", user="zykynaap"){
  con <- dbConnect(type, dbname=dbname, host=host,user=user, password="osDIVwVVps48a-0KDRIQ4UR09VUtBxwF")
}

# function 1
accounts <- read.csv('konta.csv')
rankAccount1 <- function(dataFrame,colName,groupName,valueSort,num){
  columnNames <- names(dataFrame)
  
  filterDf = filter(dataFrame, dataFrame[, colName] == groupName)
  sortedDf = filterDf[order(filterDf[, valueSort], decreasing = TRUE),]
  head(sortedDf, num)
  
}

result1 <- rankAccount1(accounts, 'occupation', 'LISTONOSZ', 'saldo', 10);
print(result1)


# function 2
rankAccount2 <- function(filePath, datachunk, colName, groupName,valueSort,num){
  fileConnection <-file(description = filePath, open='r')
  data <-read.table(fileConnection, nrows=datachunk, header = TRUE, fill= TRUE, sep = ',')
  columnNames <- names(data)
  df = NULL
  
  repeat{
    if(nrow(data) == 0){
      break;
    }
    data <- na.omit(data)
    filterDf = filter(data, data[, colName] == groupName)
    df = rbind(df, filterDf)
    data <- read.table(fileConnection, nrows=datachunk, col.names = columnNames, fill= TRUE, sep = ',')
  }
  
  sortedDf = df[order(df[, valueSort], decreasing = TRUE),]
  head(sortedDf, num)
  
}

result2 <- rankAccount2("konta.csv", 1000, "occupation", "NAUCZYCIEL", "saldo",10)
print(result2)

# function 3
rankAccount3 <- function(tab,colName,groupName,valueSort,num){
  con <-connectMe()
  dbGetQuery(con, paste0("select * from ", tab, " where ", colName, "='", groupName, "' order by ", valueSort, " limit ", num))
} 

result3 <- rankAccount3(con, "suicide", "COUNTRY", "Chile", "population", 2)
result3

