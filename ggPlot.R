library(ggplot2)
head(diamonds)
str(diamonds)

p <- ggplot(diamonds,aes(x=carat,y=price))
p + geom_point()

p <- ggplot(diamonds,aes(cut))
p + geom_histogram(aes(fill=clarity))
p + stat_bin(geom="area")
p + stat_bin(geom="point")
p + stat_bin(geom="line")


p <- ggplot(diamonds,aes(price))
p + geom_histogram(aes(y=..density..))
p + geom_histogram(aes(color=..count..))

p <- ggplot(diamonds,aes(price))
p + geom_smooth(aes(y=..density..))
