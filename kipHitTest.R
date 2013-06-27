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
numPrices

q <- paste("select * from prices where underlyingid='",underlyingId,"' and date>='2003-07-24' ",sep="")
r1 <- sqlQuery(dbConn,q,errors=FALSE)
if(is.integer(r1) || length(r1[,1]) == 0) {print(paste("Either no data or SQLerror: with",q,odbcGetErrMsg(dbConn)));browser(); stop();}
numPrices2 <- length(r1[,1])
r1[numPrices2,]
numPrices2
returns   <- r1$Price/r$Price
numHits   <- sum(returns<0.5)
theseHits <- r1[which(returns<0.5),]
theseHits[1,]
theseHits[numHits,]
numHits



q <- paste("select * from outlook where productid='",89,"' and kipHit='1' ",sep="")
outlookHits <- sqlQuery(dbConn,q,errors=FALSE)
if(is.integer(outlookHits) || length(outlookHits[,1]) == 0) {print(paste("Either no data or SQLerror: with",q,odbcGetErrMsg(dbConn)));browser(); stop();}
numHits1 <- length(outlookHits[,1])
numHits1
outlookHits[1,]
outlookHits[numHits1,]


q <- paste("select * from outlook where productid='",89,"'",sep="")
allOutlook <- sqlQuery(dbConn,q,errors=FALSE)
if(is.integer(allOutlook) || length(allOutlook[,1]) == 0) {print(paste("Either no data or SQLerror: with",q,odbcGetErrMsg(dbConn)));browser(); stop();}
numOutlookPts <- length(allOutlook[,1])
allOutlook[1,]
allOutlook[numOutlookPts,]
