# *** 
# examine product icebergRatios
# -  setwd("c:/sites/sp/analysis/meteor")
# -  source("icebergRatio.R")
# *** 

# ************** 
# R initialisation - load libraries etc
# **************
options(error = recover);
library(RODBC)
library(ggplot2)
library(grid)
library(RColorBrewer)
dbConn <- odbcConnect("newSp")

# ************** 
# constants
# **************
if(length(dev.list()) < 3)  dev.new();

# get product outlook  
productId  <- 89;
q <- paste("select Date,Payoff,WorstPerf from outlook where productid='",productId,"'",sep="")
r <- sqlQuery(dbConn,q,errors=FALSE)
if(is.integer(r) || length(r[,1]) == 0) {print(paste("Either no data or SQLerror: with",q,odbcGetErrMsg(dbConn)));browser(); stop();}
numPoints  <- length(r$Date)
firstDate  <- r[1,"Date"]
lastDate   <- r[numPoints,"Date"]
# get underlyings
q <- paste("select distinct u.underlyingId UnderlyingId,u.Name ulName from productbarrier p join barrierrelation using (productbarrierid) ",
        "join underlying u using (underlyingid) where p.ProductId='",productId,"' order by underlyingId");  		     
u <- sqlQuery(dbConn,q,errors=FALSE)
if(is.integer(u) || length(u[,1]) == 0) {print(paste("Either no data or SQLerror: with",q,odbcGetErrMsg(dbConn)));browser(); stop();}
# get underlyingPrices

numNames <- length(u[,1]);
sql      <- "select p1.Date";
for(i in 1:numNames) sql <- paste(sql,",p",i,".price price",i,sep="");
sql <- paste(sql," from prices p1 ");
if(numNames>1) {for(i in 2:numNames) sql <- paste(sql," join prices p",i," using (Date) ",sep="");}
sql <- paste(sql," where p1.underlyingId = '",u[1,"UnderlyingId"],"'");
if(numNames>1) {for(i in 2:numNames) sql <- paste(sql," and p",i,".underlyingId='",u[i,"UnderlyingId"],"'",sep="");}
sql <- paste(sql, " and Date>='",firstDate,"'");
#sql <- paste(sql, " and Date<='",lastDate,"'");
sql <- paste(sql, " order by Date");
p <- sqlQuery(dbConn,sql,errors=FALSE)
if(is.integer(p) || length(p[,1]) == 0) {print(paste("Either no data or SQLerror: with",sql,odbcGetErrMsg(dbConn)));browser(); stop();}
numPrices    <- length(p[,1]);
firstUlDate  <- p[1,"Date"]
lastUlDate   <- p[numPrices,"Date"]
tdpy         <- as.integer(365*numPrices/as.integer(lastUlDate-firstUlDate))   # trading days per year
# rescale to 1
for(i in 1:numNames) {
  colName    <- paste("price",i,sep="")
  lastPrice  <- p[numPrices,colName]
  for(j in 1:numPrices) {p[j,colName] <-  p[j,colName]/lastPrice }
}
# get max and min price
maxPrice <- 0;
minPrice <- 10000;
for(i in 1:numNames) {
  colName    <- paste("price",i,sep="")
  minPrice   <- min(min(p[,colName]),minPrice)
  maxPrice   <- max(max(p[,colName]),maxPrice)
}


# draw timeseries
lastPayoffYear     <- as.numeric(format(lastDate,"%Y"));
lastPayoffDay      <- (as.numeric(format(lastDate,"%m"))-1)*tdpy/12 + as.numeric(format(lastDate,"%d"))*5/7;
lastPayoffPoint    <- lastPayoffYear + lastPayoffDay/tdpy
startYear          <- as.numeric(format(firstDate,"%Y"));
startDay           <- (as.numeric(format(firstDate,"%m"))-1)*tdpy/12 + as.numeric(format(firstDate,"%d"))*5/7;
tsStart            <- c(startYear,startDay)
ulAxis             <- pretty(range(c(minPrice,maxPrice)),5)  
payoffAxis         <- pretty(range(c(min(r$Payoff,r$WorstPerf),max(r$Payoff,r$WorstPerf))),5)  
colours            <- rainbow(numNames+2)
par(xpd=FALSE,mar=c(3.1,4.1,4.1,8.1));
plot(ts(p[,"price1"],start=tsStart,frequency=256),type='l',
     main=paste("Outlook"),col=colours[1],ylim=c(min(ulAxis),max(ulAxis)),
     ylab=paste("Underlyings - scaled to 1 at last date"))
if(numNames>1) {
  for(i in 2:numNames) {
    par(new=TRUE);
  plot(ts(p[,paste("price",i,sep="")],start=tsStart,frequency=256),type='l',
       main="",col=colours[i],ylim=c(min(ulAxis),max(ulAxis)),
       ylab="")
}
}
par(new=TRUE);
pricePadding <- rep(NA,numPrices-numPoints)
plot(ts(c(r$Payoff,pricePadding),start=tsStart,frequency=256),type='l',xaxt="n",yaxt="n",col=colours[numNames+1],
     ylim=c(min(payoffAxis),max(payoffAxis)),ylab="")
abline(v=lastPayoffPoint, lty = "solid", lwd=2,col="grey");
par(new=TRUE);
plot(ts(c(r$WorstPerf,pricePadding),start=tsStart,frequency=256),type='l',yaxt="n",col=colours[numNames+2],
     ylim=c(min(payoffAxis),max(payoffAxis)),ylab="")
axis(4, at=payoffAxis, labels=format(payoffAxis))
for(i in 1:length(payoffAxis)) {
  if(payoffAxis[i] != 1) abline(h=payoffAxis[i], lty = "dotted", col="grey");
}
abline(h=1, lty = "dotted", col="red");
par(xpd=TRUE);
legendNames <- character();
for(i in 1:numNames) {
  legendNames <- c(legendNames,as.character(u[i,"ulName"]))
}
legend ("topright",c(legendNames,"Payoff(rhs)","WorstUl(rhs)"),
        col=colours,lty=c("solid","solid"), bty="n", inset = c(-0.2,-0.1));
mtext("Payoff or WorstUnderlying",4,line=2)

