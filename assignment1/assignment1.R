# read data from google drive
library(googlesheets)
suppressMessages(library(dplyr))
(my_sheets <- gs_ls())
gap <- gs_title("HW 1 Fall 2017.xlsx")
ret <- gap %>% gs_read(ws = "Sheet1")
ret$Date <- as.Date(ret$Date, format = "%m/%d/%Y")
cols <- names(ret)

# 1
library(psych)
ret.descStats <- round(describe(ret[-1]), 3)
View(ret.descStats)
library(gridExtra)
jpeg(filename = "~/502/assignment1/figures/tbl_descriptive_statistics.png", height = 170, width = 240)
grid.table(subset(ret.descStats, select = c("mean", "sd", "skew", "kurtosis")))
dev.off()
# 2
library(reshape2)
library(ggplot2)
ret.cor <- round(cor(ret[-1]),3)
View(ret.cor)
jpeg(filename = "~/502/assignment1/figures/tbl_correlation.jpeg", height = 200, width = 350)
grid.table(ret.cor)
dev.off()
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
# Reorder matrix
reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
ret.cor.order <- reorder_cormat(ret.cor)
ret.corr.upper_tri <- get_upper_tri(ret.cor)
# Convert the matrix to column
ret.corr.melt <- melt(ret.corr.upper_tri, na.rm = TRUE)
View(ret.corr.melt)
# Create a ggheatmap
jpeg(filename = sprintf("~/502/assignment1/figures/correlation_heatmap.jpeg", c))
ggheatmap <- ggplot(ret.corr.melt, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Daily Percent Changes\nExchange Traded Funds") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(size = 12))+
  coord_fixed()
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.4, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))
dev.off()
# 3
for (c in cols[-1]) {
  jpeg(filename = sprintf("~/502/assignment1/figures/autocorrelation_%s.jpeg", c))
  acf(ret[c], lag.max = 10, type = "correlation", ci=c(0.95, 0.9), ci.col = c("red", "blue"))
  dev.off()
}
# 4
for(c in cols[-1]) {
  ret[sprintf("%s%s", c, "sq")] <- ret[c]^2
}
View(ret)
squared_cols <- names(ret[9:15])
for (c in squared_cols) {
  jpeg(filename = sprintf("~/502/assignment1/figures/squared_autocorrelation_%s.jpeg", c))
  acf(ret[c], lag.max = 10, type = "correlation", ci=c(0.95, 0.9), ci.col = c("red", "blue"))
  dev.off()
}
# 5
for (c in cols[-1]) {
  jpeg(filename = sprintf("~/502/assignment1/figures/qqplot_%s.jpeg", c))
  qqnorm(scale(ret[c]), xlim=c(-4,4), ylim = c(-4,4), xlab = "Theoretical", 
         ylab = "Standardized Daily Percent Changes", main = sprintf("%s vs the Normal Distribution", c))
  abline(0,1, col= "red")
  dev.off()
}
# 6 - Tuesday
h.spy <- rep(0, 1536)
h.spy[1]<-var(ret[1:60, "SPY"])
for(i in c(2:1536)) {
  h.spy[i] <-0.06*ret[i-1, "SPY"]^2 + .94*h.spy[i-1] 
}
plot(h.spy, type="l")
vol.spy <- sqrt(h.spy)
plot(x=ret[1], y=vol.spy, type="l")
# 6 - Wednesday
h <-array(0, 1536)
condition.vol <-ret
for(i in cols[-1]) {
  h[1] <- var(ret[,i])
  for(j in c(2:1536)) {
    h[j] <- 0.6*ret[j-1, i]^2+0.94*h[j-1]
  }
  condition.vol[,i]<-sqrt(h)
}
plot(ret[[1]], ret[[7]], type="l")
lines(ret[[1]], condition.vol[[7]], col="red", lwd=3)
# 6 - Me
lagN <- 60
rows <- length(ret$Date)
lag60Var <- rep(0, times=(rows - lagN))
h <- rep(0, times=(rows - lagN))

for(c in cols[-1]) {
  for(i in 1:(rows - lagN)) {
    lag60Var[i] <- var(ret[[c]][i:(i + lagN)])
  }
  h <- (0.06*lag(ret[[sprintf("%s%s", c, "sq")]][(lagN + 1):rows],1) + 
        0.94*lag(lag60Var,1))^0.5
  jpeg(filename = sprintf("~/502/assignment1/figures/ewa_var_%s.jpeg", c))
  plot(x = ret$Date[(lagN + 2):rows], y = h[-1], type = "line", 
       xlab = "Date", ylab = "Conditional Volatility",
       main = sprintf("%s Daily \n Exponentially Weighted Average Volatility \n 60 Day Lag", c),
       col="red")
  dev.off()
}
