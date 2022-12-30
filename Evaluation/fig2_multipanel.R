load("multipanelfig.Rdata")

lancet_red <- "#AD002A99"
lancet_blue <- "#0099B4FF"
lancet_gray <- "#ADB6B6FF"

trans_blue <- rgb(col2rgb(lancet_blue)[1],col2rgb(lancet_blue)[2],col2rgb(lancet_blue)[3],  maxColorValue = 255, alpha = 25)
trans_red <- rgb(col2rgb(lancet_red)[1],col2rgb(lancet_red)[2],col2rgb(lancet_red)[3], maxColorValue = 255, alpha = 125)

dev.new(width=3*3, height=3*2, pointsize=11, noRStudioGD = TRUE)
par(mfrow=c(2,3), mar=c(1,1,2.5,1)+.1, mgp=c(1.75,0.5,0), oma=c(2.5, 2.5, 1.5, 0), tcl=-0.4)

plot(c(-40,40), c(-20,0),type="n", yaxs="i", axes=F, ylab="", xlab="")

mtext('A. Development set', side=3, line=2.5, at=-53, adj = 0, cex=1)

polygon(c(0,par("usr")[1],par("usr")[1],0), c(0, -par("usr")[1]/qnorm(0.025),0,0), col=lancet_gray, border=NA)
polygon(c(0,par("usr")[2],par("usr")[2],0), c(0, -par("usr")[2]/qnorm(0.975),0,0), col=lancet_gray, border=NA)


abline(v=0, lwd=0.75, lty=2)
arrows(-1, par("usr")[4]+.5, -42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)
arrows(1, par("usr")[4]+.5, 42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)

mtext(text="recommends RRT initation", side=3, line=0.75, at=2, cex=0.6, adj=0)
mtext(text="recommends no RRT initation", side=3, line=0.75, at=-2, cex=0.6, adj=1)

mtext("Standard Error", side=2, line=2.25, cex=.83)
#mtext("Blip (first day decision)", side=1, line=2.25, cex=.83)

# A positionner
lgd <- legend(x = mean(c(par("usr")[1],par("usr")[2])), y =  mean(c(par("usr")[3],par("usr")[4])), cex= .9, 
              c("Initatied RRT", "Did not initiate RRT"), pch=16, col = c("blue", "red"), xpd=T, plot = F)

#polygon(c(par("usr")[1], -5, -5, par("usr")[1]), c(0, 0, 5*1/qnorm(.975), 5*1/qnorm(.975)), col="white", border=NA)

legend(x = par("usr")[1]+8, y =  par("usr")[3] + lgd$rect$h-.2, cex= .9,
       c("Initiated RRT", "Did not initiate RRT"),
       pch=16, col = c(lancet_red, lancet_blue), bty = "n", plot = T)

axis(1, at=seq(-100, 200,by=150), lwd=0.75, lwd.tick=0)
axis(1, at=seq(-40, 40,by=10), lwd=0, lwd.tick=0.75)
axis(2, las=1, at=seq(0, -20, by=-5), labels=seq(0, 20, by=5), lwd=0.75, lwd.tick=0.75)

points(ite_k1[actual_ttts$a1==0], -ite_se_k1[actual_ttts$a1==0], col=trans_blue, pch=16)
points(ite_k1[actual_ttts$a1==1], -ite_se_k1[actual_ttts$a1==1], col=trans_red, pch=16, add=TRUE)


plot(c(-40,40), c(-20,0),type="n", yaxs="i", axes=F, ylab="", xlab="")
polygon(c(0,par("usr")[1],par("usr")[1],0), c(0, -par("usr")[1]/qnorm(0.025),0,0), col=lancet_gray, border=NA)
polygon(c(0,par("usr")[2],par("usr")[2],0), c(0, -par("usr")[2]/qnorm(0.975),0,0), col=lancet_gray, border=NA)


abline(v=0, lwd=0.75, lty=2)
arrows(-1, par("usr")[4]+.5, -42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)
arrows(1, par("usr")[4]+.5, 42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)

mtext(text="recommends RRT initation", side=3, line=0.75, at=2, cex=0.6, adj=0)
mtext(text="recommends no RRT initation", side=3, line=0.75, at=-2, cex=0.6, adj=1)

axis(1, at=seq(-100, 200,by=150), lwd=0.75, lwd.tick=0)
axis(1, at=seq(-40, 40,by=10), lwd=0, lwd.tick=0.75)
axis(2, las=1, at=seq(0, -20, by=-5), labels=seq(0, 20, by=5), lwd=0.75, lwd.tick=0.75)

points(ite_k2[actual_ttts$a2==0], -ite_se_k2[actual_ttts$a2==0], col=trans_blue, pch=16)
points(ite_k2[actual_ttts$a2==1], -ite_se_k2[actual_ttts$a2==1], col=trans_red, pch=16, add=TRUE)

plot(c(-40,40), c(-20,0),type="n", yaxs="i", axes=F, ylab="", xlab="")
polygon(c(0,par("usr")[1],par("usr")[1],0), c(0, -par("usr")[1]/qnorm(0.025),0,0), col=lancet_gray, border=NA)
polygon(c(0,par("usr")[2],par("usr")[2],0), c(0, -par("usr")[2]/qnorm(0.975),0,0), col=lancet_gray, border=NA)


abline(v=0, lwd=0.75, lty=2)
arrows(-1, par("usr")[4]+.5, -42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)
arrows(1, par("usr")[4]+.5, 42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)

mtext(text="recommends RRT initation", side=3, line=0.75, at=2, cex=0.6, adj=0)
mtext(text="recommends no RRT initation", side=3, line=0.75, at=-2, cex=0.6, adj=1)

axis(1, at=seq(-100, 200,by=150), lwd=0.75, lwd.tick=0)
axis(1, at=seq(-40, 40,by=10), lwd=0, lwd.tick=0.75)
axis(2, las=1, at=seq(0, -20, by=-5), labels=seq(0, 20, by=5), lwd=0.75, lwd.tick=0.75)

points(ite_k3[actual_ttts$a3==0], -ite_se_k3[actual_ttts$a3==0], col=trans_blue, pch=16)
points(ite_k3[actual_ttts$a3==1], -ite_se_k3[actual_ttts$a3==1], col=trans_red, pch=16, add=TRUE)

#VALIDATION SET
plot(c(-40,40), c(-20,0),type="n", yaxs="i", axes=F, ylab="", xlab="")

mtext('B. Validation set', side=3, line=.5, at=-53, adj = 0, cex=1)

polygon(c(0,par("usr")[1],par("usr")[1],0), c(0, -par("usr")[1]/qnorm(0.025),0,0), col=lancet_gray, border=NA)
polygon(c(0,par("usr")[2],par("usr")[2],0), c(0, -par("usr")[2]/qnorm(0.975),0,0), col=lancet_gray, border=NA)


abline(v=0, lwd=0.75, lty=2)
#arrows(-1, par("usr")[4]+.5, -42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)
#arrows(1, par("usr")[4]+.5, 42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)

#mtext(text="recommends RRT initation", side=3, line=0.75, at=2, cex=0.6, adj=0)
#mtext(text="recommends no RRT initation", side=3, line=0.75, at=-2, cex=0.6, adj=1)

mtext("Standard Error", side=2, line=2.25, cex=.83)
#mtext("Blip (first day decision)", side=1, line=2.25, cex=.83)

# A positionner
#lgd <- legend(x = mean(c(par("usr")[1],par("usr")[2])), y =  mean(c(par("usr")[3],par("usr")[4])), cex= .9, 
#              c("Initatied RRT", "Did not initiate RRT"), pch=16, col = c("blue", "red"), xpd=T, plot = F)

#polygon(c(par("usr")[1], -5, -5, par("usr")[1]), c(0, 0, 5*1/qnorm(.975), 5*1/qnorm(.975)), col="white", border=NA)

#legend(x = par("usr")[1], y =  par("usr")[3] + lgd$rect$h-.2, cex= .9,
#       c("Initiated RRT", "Did not initiate RRT"),
#       pch=16, col = c(lancet_red, lancet_blue), bty = "n", plot = T)

axis(1, at=seq(-100, 200,by=150), lwd=0.75, lwd.tick=0)
axis(1, at=seq(-40, 40,by=10), lwd=0, lwd.tick=0.75)
axis(2, las=1, at=seq(0, -20, by=-5), labels=seq(0, 20, by=5), lwd=0.75, lwd.tick=0.75)

points(ite_k1_val[actual_ttts_val$a1==0], -ite_se_k1_val[actual_ttts_val$a1==0], col=trans_blue, pch=16)
points(ite_k1_val[actual_ttts_val$a1==1], -ite_se_k1_val[actual_ttts_val$a1==1], col=trans_red, pch=16, add=TRUE)
mtext("Blip (first day decision)", side=1, line=2.25, cex=.83)

plot(c(-40,40), c(-20,0),type="n", yaxs="i", axes=F, ylab="", xlab="")
polygon(c(0,par("usr")[1],par("usr")[1],0), c(0, -par("usr")[1]/qnorm(0.025),0,0), col=lancet_gray, border=NA)
polygon(c(0,par("usr")[2],par("usr")[2],0), c(0, -par("usr")[2]/qnorm(0.975),0,0), col=lancet_gray, border=NA)


abline(v=0, lwd=0.75, lty=2)
#arrows(-1, par("usr")[4]+.5, -42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)
#arrows(1, par("usr")[4]+.5, 42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)

#mtext(text="recommends RRT initation", side=3, line=0.75, at=2, cex=0.6, adj=0)
#mtext(text="recommends no RRT initation", side=3, line=0.75, at=-2, cex=0.6, adj=1)

axis(1, at=seq(-100, 200,by=150), lwd=0.75, lwd.tick=0)
axis(1, at=seq(-40, 40,by=10), lwd=0, lwd.tick=0.75)
axis(2, las=1, at=seq(0, -20, by=-5), labels=seq(0, 20, by=5), lwd=0.75, lwd.tick=0.75)

points(ite_k2_val[actual_ttts_val$a2==0], -ite_se_k2_val[actual_ttts_val$a2==0], col=trans_blue, pch=16)
points(ite_k2_val[actual_ttts_val$a2==1], -ite_se_k2_val[actual_ttts_val$a2==1], col=trans_red, pch=16, add=TRUE)
mtext("Blip (second day decision)", side=1, line=2.25, cex=.83)

plot(c(-40,40), c(-20,0),type="n", yaxs="i", axes=F, ylab="", xlab="")
polygon(c(0,par("usr")[1],par("usr")[1],0), c(0, -par("usr")[1]/qnorm(0.025),0,0), col=lancet_gray, border=NA)
polygon(c(0,par("usr")[2],par("usr")[2],0), c(0, -par("usr")[2]/qnorm(0.975),0,0), col=lancet_gray, border=NA)


abline(v=0, lwd=0.75, lty=2)
#arrows(-1, par("usr")[4]+.5, -42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)
#arrows(1, par("usr")[4]+.5, 42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)

#mtext(text="recommends RRT initation", side=3, line=0.75, at=2, cex=0.6, adj=0)
#mtext(text="recommends no RRT initation", side=3, line=0.75, at=-2, cex=0.6, adj=1)

axis(1, at=seq(-100, 200,by=150), lwd=0.75, lwd.tick=0)
axis(1, at=seq(-40, 40,by=10), lwd=0, lwd.tick=0.75)
axis(2, las=1, at=seq(0, -20, by=-5), labels=seq(0, 20, by=5), lwd=0.75, lwd.tick=0.75)

points(ite_k3_val[actual_ttts_val$a3==0], -ite_se_k3_val[actual_ttts_val$a3==0], col=trans_blue, pch=16)
points(ite_k3_val[actual_ttts_val$a3==1], -ite_se_k3_val[actual_ttts_val$a3==1], col=trans_red, pch=16, add=TRUE)
mtext("Blip (third day decision)", side=1, line=2.25, cex=.83)

