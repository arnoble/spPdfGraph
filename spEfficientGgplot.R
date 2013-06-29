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

# get product info
commonQ <- paste("select p.name,producttypeid,pt.name type,ecGain,ecLoss,100*probGain probGain,100*probLoss probLoss from cashflows ",
  "join product p using (productId) join producttype pt using (producttypeid) ",
  "where projectedreturn='1' and illustrative != '1' ");
q <- paste(commonQ,"and productid='",productId,"'")
productInfo <- sqlQuery(dbConn,q,errors=FALSE)
if(is.integer(productInfo) || length(productInfo[,1]) == 0) {print(paste("Either no data or SQLerror: with",q,odbcGetErrMsg(dbConn)));browser(); stop();}

# get peer product info
q <- paste(commonQ,"and productid>'33' and producttypeid='",productInfo$producttypeid[1],"' order by WinLose desc limit 5")
peerInfo <- sqlQuery(dbConn,q,errors=FALSE)
if(is.integer(peerInfo) || length(peerInfo[,1]) == 0) {print(paste("Either no data or SQLerror: with",q,odbcGetErrMsg(dbConn)));browser(); stop();}

allInfo       <- rbind(productInfo,peerInfo)
graphData     <- data.frame(x1=allInfo$ecLoss*allInfo$probLoss/100,y1=allInfo$ecGain*allInfo$probGain/100,Product=allInfo$name)
numProducts   <- length(allInfo[,1]) 
prodColours   <- c("#FF0000",rainbow(numProducts-1)
xLineArrow    <- data.frame(x1=c(0,max(graphData$x1)),y1=c(0,0))
yLineArrow    <- data.frame(x1=c(0,0),y1=c(0,max(graphData$y1)))
Product       <- graphData$Product
                     
xLabPos = annotate("text",x=1.1*max(graphData$x1), y = -0.05*min(graphData$y1), label ="Expected Loss", vjust = 1, hjust = 1, size=7,colour = "black");
yLabPos = annotate("text",x=0, y = 1.1*max(graphData$y1), label ="Expected Gain", vjust = 1, hjust = 0, size=7,colour = "black");
p <- ggplot(graphData)
p <- p + geom_point(data=graphData,aes(x=x1,y=y1,colour=Product),size=4)
p <- p + scale_colour_manual(values=prodColours)
p <- p + labs(title = expression("Product comparision with similar products"),size=10)
p <- p + xlab("Expected Loss") + ylab("Expected Gain")
p <- p + xLabPos + yLabPos 
p <- p + geom_line(data=xLineArrow,aes(x=x1,y=y1),arrow = arrow(angle = 15, ends = "last", type = "closed"))
p <- p + geom_line(data=yLineArrow,aes(x=x1,y=y1),arrow = arrow(angle = 15, ends = "last", type = "closed"))
p <- p + theme(legend.position="bottom") + guides(col = guide_legend(nrow = 3))
p

