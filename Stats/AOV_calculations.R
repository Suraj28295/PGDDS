l1<- c(13,10,8,11,8)
l2 <- c(13,11,14,14)
l3 <- c(4,1,3,4,2,4)
grp <- rep(c('f-A','f-B','f-C'),each=5)
grp[10] <- 'f-C'
aov <- aov(c(l1,l2,l3)~grp)


summary(aov)


TukeyHSD(aov)
