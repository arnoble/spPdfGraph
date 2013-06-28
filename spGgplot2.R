# ************** 
# R initialisation - load libraries etc
# **************
options(error = recover);
library(RODBC)
library(ggplot2)
library(grid)
library(RColorBrewer)
dbConn <- odbcConnect("newSp")


productId  <- 89;
q <- paste("select name,ecGain,ecLoss,100*probGain probGain,100*probLoss probLoss from cashflows join product using (productId) where projectedreturn='1' and productid in (35,",productId,")",sep="")
r <- sqlQuery(dbConn,q,errors=FALSE)
if(is.integer(r) || length(r[,1]) == 0) {print(paste("Either no data or SQLerror: with",q,odbcGetErrMsg(dbConn)));browser(); stop();}

x3 <- c(r$probGain/2)
y3 <- r$ecGain*r$probGain/100
x4 <- c(-r$probLoss/2)
y4 <- -r$ecLoss*r$probLoss/100

productGainColour    <- "#9999AF"
productGainDotColour <- "#9999FF"
productLossColour    <- "#FF99AF"
productLossDotColour <- "#FF99FF"
bmGains       <- data.frame(x1=c(rep(0,2),rep(r$probGain[1],2)),y1=c(0,rep(r$ecGain[1],2),0))
productGains  <- data.frame(x1=c(rep(0,2),rep(r$probGain[2],2)),y1=c(0,rep(r$ecGain[2],2),0))
bmLosses      <- data.frame(x1=c(rep(0,2),rep(-r$probLoss[1],2)),y1=c(0,rep(-r$ecLoss[1],2),0))
productLosses <- data.frame(x1=c(rep(0,2),rep(-r$probLoss[2],2)),y1=c(0,rep(-r$ecLoss[2],2),0))
xLineArrow    <- data.frame(x1=c(-max(r$probLoss),max(r$probGain)),y1=c(0,0))
yLineArrow    <- data.frame(x1=c(0,0),y1=c(-max(r$ecLoss),max(r$ecGain)))

xLabPos = annotate("text",x=max(r$probGain), y = -0.1*max(r$ecLoss), label ="Probability of gain", vjust = 1, hjust = 1, size=7,colour = "black");
xLabNeg = annotate("text",x=-0.1*max(r$probLoss), y = 0.1*max(r$ecGain), label ="Probability of loss", vjust = 1, hjust = 1, size=7,colour = "black");
yLabPos = annotate("text",x=-0.1*max(r$probLoss), y = max(r$ecGain), label ="Expected gain", vjust = 1, hjust = 1, size=7,colour = "black");
yLabNeg = annotate("text",x=0.1*max(r$probGain), y = -max(r$ecLoss), label ="Expected loss", vjust = 1, hjust = 0, size=7,colour = "black");


p <- ggplot(productGains,aes(x=x1,y=y1)) + theme_grey(base_size=12)
p <- p + geom_polygon(colour=productGainColour,stat="identity",fill=productGainColour) 
p <- p + geom_path(data=bmGains,colour="blue",stat="identity") 
p <- p + geom_polygon(data=productLosses,colour=productLossColour,stat="identity",fill=productLossColour) 
p <- p + geom_path(data=bmLosses,colour="red",stat="identity") 
p <- p + labs(title = expression("Product comparision with UKequity held 6y"),size=10)
p <- p + xlab("Probability") + ylab("Expected Gain or Loss")
p <- p + geom_point(data=data.frame(x=x3,y=y3),aes(x=x3,y=y3),color=c("blue",productGainDotColour),size=c(5,10))
p <- p + geom_point(data=data.frame(x=x4,y=y4),aes(x=x4,y=y4),color=c("red",productLossDotColour),size=c(5,10))
p <- p + geom_line(data=xLineArrow,arrow = arrow(angle = 15, ends = "both", type = "closed"))
p <- p + geom_line(data=yLineArrow,arrow = arrow(angle = 15, ends = "both", type = "closed"))
p <- p + xLabPos + xLabNeg + yLabPos + yLabNeg
p

