data("mtcars")
d<-mtcars
d
fxdisp <- function(x){c(mean(x),sd(x),sd(x)/sqrt(length(x))*1.96)}
tmp<-aggregate(mpg~am+vs, d,fxdisp)
disper<-cbind(tmp[-ncol(tmp)], tmp[[ncol(tmp)]])
colnames(disper)<-c("am2","vs2", "mean", "sd", "ci")

xa <- c(1, 2)
decal<-0.1
attach(d)
attach(disper)

plot(x=am[vs == 1]+decal, y=mpg[vs == 1], pch = 21, col = "black",
       bg = "grey", cex = 1,
     xlim = c(-0.5,1.5),
     ylim = c(5,40),
     xlab = "am", ylab = "mpg",
     xaxt='n')
points(x=am[vs == 0]-decal, y=mpg[vs == 0], pch = 21, col = "black",
     bg = "grey", cex = 1)
points(am2[vs2 == 1]+decal, y = mean[vs2 == 1], bg = "turquoise", pch = 22, cex = 1)
points(am2[vs2 == 0]-decal, y = mean[vs2 == 0], bg = "red", pch = 22, cex = 1)
lines(am2[vs2 == 1]+decal, y = mean[vs2 == 1], col = "turquoise", lty="dotted")
lines(am2[vs2 == 0]-decal, y = mean[vs2 == 0], col = "red", lty="dotted")
axis(1, at=c(0,1), labels=c("placebo", "rien"))

