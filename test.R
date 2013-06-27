# ************** 
# R initialisation - load libraries etc
# **************
options(error = recover);
library(RODBC)
library(ggplot2)
library(grid)
library(RColorBrewer)
dbConn <- odbcConnect("newSp")

underlyingId  <- 16;
q <- paste("select * from prices where underlyingid='",underlyingId,"' and date>='1997-07-22' and date<='2007-06-26'",sep="")
r <- sqlQuery(dbConn,q,errors=FALSE)
if(is.integer(r) || length(r[,1]) == 0) {print(paste("Either no data or SQLerror: with",q,odbcGetErrMsg(dbConn)));browser(); stop();}
numPrices1 <- length(r[,1])

q <- paste("select * from prices where underlyingid='",underlyingId,"' and date>='2003-07-22' ",sep="")
r1 <- sqlQuery(dbConn,q,errors=FALSE)
if(is.integer(r1) || length(r1[,1]) == 0) {print(paste("Either no data or SQLerror: with",q,odbcGetErrMsg(dbConn)));browser(); stop();}
numPrices2 <- length(r1[,1])
r1[numPrices2,]

