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

# get all product subsubtitles	
q <- paste("select productid id,name,subsubtitle,",
            "round(ecGain,1) ecGain,round(ecStrictGain,1) ecStrictGain,round(ecLoss,1) ecLoss,round(100*probGain,1) probGain,",
            "round(100*probStrictGain,1) probStrictGain,round(100*probLoss,1) probLoss,round(Duration,1) Duration,",
            "round(PosDuration,1) PosDuration,round(StrictPosDuration,1) StrictPosDuration,round(NegDuration,1) NegDuration",
            " from product join cashflows using (productid) where subsubtitle is not null and subsubtitle !='' and projectedReturn='1'",
            " and productid in (35,36,78) order by productId ");
r <- sqlQuery(dbConn,q,errors=FALSE)
if(is.integer(r) || length(r[,1]) == 0) {print(paste("Either no data or SQLerror: with",q,odbcGetErrMsg(dbConn)));browser(); stop();}
numProducts  <- length(r$subsubtitle) 
ExpectedGain <- r$ecGain*r$probGain/100
ExpectedLoss <- r$ecLoss*r$probLoss/100
bothNames    <- character(); bothIret = numeric();bothIprob = numeric();bothEret = numeric();



for(i in 1:numProducts)
{     
bothNames   <- c(bothNames,paste(r$name[i],"-Gain"),paste(r$name[i],"-Loss"))
bothIret    <- c(bothIret, r$ecGain[i], r$ecLoss[i] )
bothIprob   <- c(bothIprob,r$probGain[i],r$probLoss[i])
bothEret    <- c(bothEret, ExpectedGain[i], ExpectedLoss[i] )
}
results <- data.frame(id=seq(1:numProducts),name=r$name)


#
# do Gains graph
#
df     <- data.frame(ProductName=results$name, width = r$probGain, height = r$ecGain)
# ProductName is a factor whose levels are ordered alphabetically;; this line puts its levels into original order
df     <- transform(df,ProductName = reorder(ProductName,results$id))  
df$w   <- cumsum(df$width)
df$wm  <- df$w - df$width
df$wt  <- with(df, wm + (w - wm)/2)
p      <- ggplot(df,aes(ymin=0))
p1     <- p + geom_rect(aes(xmin = wm, xmax = w, ymax = height, fill = ProductName))
p2     <- p1 + geom_point(aes(x = wt, y = ExpectedGain))
p2     <- p2 + geom_point(aes(x = wt, y = ExpectedLoss,colour=r$probLoss)) + scale_colour_gradient(high = "red")
pGain  <- p2 + theme(legend.position = "right",legend.key.size=unit(10,"points")) + labs(x = NULL, y = "Gains - Annualised return")

#
# do Loss graph
#
df     <- data.frame(ProductName=results$name, width = r$probLoss, height = r$ecLoss)
# ProductName is a factor whose levels are ordered alphabetically;; this line puts its levels into original order
df     <- transform(df,ProductName = reorder(ProductName,results$id))  
df$w   <- cumsum(df$width)
df$wm  <- df$w - df$width
df$wt  <- with(df, wm + (w - wm)/2)
p      <- ggplot(df,aes(ymin=0))
p1     <- p + geom_rect(aes(xmin = wm, xmax = w, ymax = height, fill = ProductName))
p2     <- p1 + theme_bw() + theme(legend.position = "right",legend.key.size=unit(10,"points")) + labs(x = NULL, y = "Losses - Annualised return")
pLoss <- p2 + geom_text(aes(x = wt, y = ExpectedLoss, label = "+"))

#
# do joint Gains graph
#
alternateIndx    <- seq(1,(numProducts*2),by=2)
AnnualisedReturn <- rep(c("Gain","Loss"),numProducts)
ReturnColours    <- rep(c("Blue","Red"),numProducts)
df     <- data.frame(ProductName=bothNames,width = bothIprob, height = bothIret)
df     <- transform(df,ProductName=reorder(ProductName,seq(1:(2*numProducts))))
df$w   <- cumsum(df$width)
df$wm  <- df$w - df$width
df$wt  <- with(df, wm + (w - wm)/2)
p      <- ggplot(df,aes(ymin=0))
#  p1     <- p + geom_rect(aes(xmin = wm, xmax = w, ymax = height, fill = ProductName))
p1     <- p + geom_rect(aes(xmin = wm, xmax = w, ymax = height, fill = AnnualisedReturn)) + scale_x_discrete(labels="") 
#  p1     <- p1 + scale_color_discrete()
p1     <- p1 + scale_fill_discrete(h=c(360,0),h.start=200,direction=-1)
p2     <- p1 + geom_point(aes(x = wt, y = bothEret))
p2     <- p2 + geom_text(aes(x = wt[alternateIndx], y = r$ecGain*1.1, label = r$name))
pBoth  <- p2 + theme_bw() +  theme(legend.position = "right",legend.key.size=unit(10,"points")) + labs(x=NULL,y="Annualised return")


# draw graphs
pGain;dev.set(dev.next());pLoss;dev.set(dev.next());pBoth;dev.set(dev.next());
