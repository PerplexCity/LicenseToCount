library("plotrix")
library(ggplot2)

lics <- c(7802, 2866, 1348, 7821, 3029, 
          4237, 4922, 4597, 5186, 7490)

samp <- 10
m <-max(lics[1:samp])
k <- length(lics[1:samp])
est <- round(m + (m-k)/k, 0)
lb <- max(lics[1:samp])
ub <- max(lics[1:samp])*20^(1/(length(lics[1:samp])))


for (samp in 1:10){
  samp <- 10
  m <-max(lics[1:samp])
  k <- length(lics[1:samp])
  est <- round(m + (m-k)/k, 0)
  lb <- max(lics[1:samp])
  ub <- round(max(lics[1:samp])*20^(1/(length(lics[1:samp]))), 0)
  texty = paste("licenses: ", k, 
                "\nupper bound: ", ub,
                "\nestimate: ", est,
                "\nlower bound: ", lb)
  x <- 0
  y <- 0
  pic <- plot(x,y, xlim=c(-0.5, 0.5), ylim=c(-0.5, 0.5), axes=F, xlab="", ylab="")
  text(-0.2, 0.3, pos=2, font = 2, cex=0.8,
     texty)
  draw.circle(0,0,ub/150000, col="dodgerblue4")
  draw.circle(0,0,est/150000, col="dodgerblue2")
  draw.circle(0,0, lb/150000, col="white")
  pic
}

lic <- read.csv("~/Desktop/licperm.csv")
bbi <- element_text(face="bold.italic", color="black")

ggplot(lic[1:1,], aes(x=rank)) + 
  geom_point(aes(y=a[1]), col="dodgerblue3", size=3, shape=18) +
  geom_point(aes(y=b[1]), col="skyblue1", size=3, shape=18) + 
  geom_point(aes(y=c[1]), col="skyblue", size=3, shape=18) +
  geom_point(aes(y=d[1]), col="dodgerblue4", size=3, shape=18) + 
  geom_point(aes(y=e[1]), col="skyblue2", size=3, shape=18) +
  geom_point(aes(y=f[1]), col="skyblue3", size=3, shape=18) + 
  geom_point(aes(y=g[1]), col="dodgerblue", size=3, shape=18) +
  geom_point(aes(y=h[1]), col="skyblue4", size=3, shape=18) + 
  geom_point(aes(y=i[1]), col="dodgerblue1", size=3, shape=18) +
  geom_point(aes(y=j[1]), col="dodgerblue2", size=3, shape=18) + 
  scale_x_continuous(breaks = 1:10, limits = c(1,10)) + 
  labs(y="estimate", x="sample size", title="Ten permutations of same sample")+
  theme(title=bbi)

#toggle W to number of license plates in sample. 
#Estimate converges at w=10
W <- 10
A <-0.7

ggplot(lic[1:W,], aes(x=rank)) + 
  geom_line(aes(y=a[1:W]), col="dodgerblue3", alpha=A, size=2) +
  geom_line(aes(y=b[1:W]), col="skyblue1", alpha=A, size=2) + 
  geom_line(aes(y=c[1:W]), col="skyblue", alpha=A, size=2) +
  geom_line(aes(y=d[1:W]), col="dodgerblue4", alpha=A, size=2) + 
  geom_line(aes(y=e[1:W]), col="skyblue2", alpha=A, size=2) +
  geom_line(aes(y=f[1:W]), col="skyblue3", alpha=A, size=2) + 
  geom_line(aes(y=g[1:W]), col="dodgerblue", alpha=A, size=2) +
  geom_line(aes(y=h[1:W]), col="skyblue4", alpha=A, size=2) + 
  geom_line(aes(y=i[1:W]), col="dodgerblue1", alpha=A, size=2) +
  geom_line(aes(y=j[1:W]), col="dodgerblue2", alpha=A, size=2) + 
  scale_x_continuous(breaks = 1:10, limits = c(1,10)) + 
  labs(y="estimate", x="sample size", title="Ten permutations of same sample")+
  theme(title=bbi)

