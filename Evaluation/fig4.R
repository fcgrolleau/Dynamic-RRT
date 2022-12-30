library(reticulate)
setwd("/Users/francois/Desktop/github repos/MIMIC-DTR/validation/python/Terminal state fix")
np <- import("numpy")
pe <- np$load("est_alldf.npy")
cis <- np$load("emp_alldf.npy")
strat <- c("Crude strategy", "Stringent strategy", "Treat all within 24 hours")


#
pe <- c(pe,0)
strat <- c(strat, "Standard-delayed (reference)")
risk_a <- .05

dev.new(width=12, height=3, pointsize=11, noRStudioGD = TRUE)
epsi <- .2
par(mar=c(4.1, 2.1, 2.1, 2.1), mgp=c(1.75,0.5,0), oma=c(0, 0, 0, 0), tcl=-0.5)
plot(pe[c(1,2,3,4)], .5+(4:1)*.8, xlim=c(-45,100), ylim=c(1-epsi, length(1:4)+2), pch=19, cex=1.4, axes=F, ylab="", xlab="")
segments(0,-1,0,4+2.5*epsi, lwd=0.75, lty=2)
text(-50, .5+(4:1)*.8, strat[c(1,2,3,4)], adj=0)
text(42.5, .5+(4:1)*.8, c(gsub("\\.", "·", format(round(pe,1), nsmall = 1)[c(1,2,3)]), "    0"), adj=0)
text(47.5, .45+(4:1)*.8, c(gsub("\\.", "·", paste0("(", gsub(" ", "", format(round(cis[,1], 1)[c(1,2,3)], nsmall = 1, fixed = TRUE)), " to ", format(round(cis[,2], 1)[c(1,2,3)], nsmall = 1, fixed = TRUE), ")" )), "           –")
     , adj=0)

segments(x0 = cis[c(1,2,3),1], x1 = cis[c(1,2,3),2], y0=.5+(4:2)*.8, y1=.5+(4:2)*.8)

text(-39, 4.95, substitute(paste(bold("Evaluated strategy"))))
text(52, 5.5, substitute(paste(bold("Mean Difference in"))))

text(75, 5.5, substitute(paste(bold("Patients initating RRT"))))

text(93, 5.5, substitute(paste(bold("Hospital"))))
text(93, 4.95, substitute(paste(bold("mortality"))))
text(90, .5+(4:1)*.8, c("41%", "38%", "43%", "45%"), adj=0)

ci_size <- as.character(100-risk_a*100);ST=9210.08
ri_leg <- bquote(bold("HFD60 (" *.(ci_size) * "% CI)"))
text(52, 4.95, ri_leg)

text(75, 4.95, substitute(paste(bold("within three days"))))

text(70, .5+(4:1)*.8, c("53%", "14%", "100%", "38%"), adj=0)

axis(1, at=seq(-20, 40, by=1), lwd=1, lwd.tick=0, labels = FALSE)
axis(1, at=seq(-20, 40, by=10), lwd=0, lwd.ticks=1)

arrows(-.5, par("usr")[3]-1.25, -20, par("usr")[3]-1.25, length = 0.07, lwd=1, xpd=TRUE)
arrows(.5, par("usr")[3]-1.25, 20, par("usr")[3]-1.25, length = 0.07, lwd=1, xpd=TRUE)
mtext(text="favors evaluated strategy", side=1, line=2.45, at=.5, cex=.7, adj=0)
mtext(text="favors current best practices", side=1, line=2.45, at=-1, cex=.7, adj=1)

segments(x0 =-55 , x1 = 99 , y0 = 4+2.5*epsi, y1 = 4+2.5*epsi)






