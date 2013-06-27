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

x1 <- c(rep(0,2),rep(r$probGain[1],3),rep(100,2))
y1 <- c(0,rep(r$ecGain[1],2),0,rep(r$ecLoss[1],2),0)
x2 <- c(rep(0.1,2),rep(r$probGain[2],3),rep(99.9,2))
y2 <- c(0,rep(r$ecGain[2],2),0,rep(r$ecLoss[2],2),0)
x3 <- c(r$probGain/2)
y3 <- r$ecGain*r$probGain/100
x4 <- c(r$probGain + (r$probLoss)/2)
y4 <- r$ecLoss*r$probLoss/100

graphData1 <- data.frame(x1,y1)
graphData2 <- data.frame(x2,y2)
graphData3 <- data.frame(x3,y3)
graphData4 <- data.frame(x4,y4)
a1 = annotate("text", x = x1[1], y = max(y1), label = r$name[1], vjust = 2, hjust = -.1, size=7,colour = "red");
a2 = annotate("text", x = x1[1], y = max(y2), label = r$name[2], vjust = 2, hjust = -.1, size=10,colour = "blue");

p <- ggplot(graphData1,aes(x=x1,y=y1)) + theme_grey(base_size=12)
p <- p + geom_line(colour="red")
p <- p + geom_line(aes(x=x2,y=y2),colour="blue") + xlab("Probability") + ylab("Expected Conditional Annualised Return (gains and losses)")
p <- p + geom_point(data=graphData3,aes(x=x3,y=y3),color=c("red","blue"),size=10)
p <- p + geom_point(data=graphData4,aes(x=x4,y=y4),color=c("red","blue"),size=10)
p <- p + a1 + a2 + labs(title = expression("Product comparision with UKequity held 6y"),size=10)
p <- p + theme(axis.text.x=element_text(size=10)) + theme(axis.text.y=element_text(size=10))
p
