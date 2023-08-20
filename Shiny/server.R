#library(Cairo) # comment this when running locally
#options(shiny.usecairo=TRUE) # comment this when running locally

#load("multipanelfig.Rdata")
load("multipanelfig_bunfix.Rdata")
load("ite_preds_coef.RData")

function(input, output) {
        
  ite_k1_react <- reactive({as.numeric(est_k1 %*% c(1, input$age, input$creat_k1, input$pot_k1) )})
  ite_k1_se_react <- reactive({ sqrt(as.numeric(t(c(1, input$age, input$creat_k1, input$pot_k1)) %*% vcv_k1 %*% c(1, input$age, input$creat_k1, input$pot_k1))) })
  ite_k1_rec_react <- reactive({ite_k1_react() - qnorm(.975) * ite_k1_se_react() > 0})
  
  ite_k2_react <- reactive({as.numeric(est_k2 %*% c(1, input$sofa, input$bun_k2, abs(input$ph_k2 - input$ph_k1), input$uo_k2 + input$uo_k1 ) )})
  ite_k2_se_react <- reactive({ sqrt(as.numeric(t(c(1, input$sofa, input$bun_k2, abs(input$ph_k2 - input$ph_k1), input$uo_k2 + input$uo_k1 ) ) %*% vcv_k2 %*% c(1, input$sofa, input$bun_k2, abs(input$ph_k2 - input$ph_k1), input$uo_k2 + input$uo_k1 ) )) })
  ite_k2_rec_react <- reactive({ite_k2_react() - qnorm(.975) * ite_k2_se_react() > 0})
  
  ite_k3_react <- reactive({as.numeric(est_k3 %*% c(1, input$uo_k3,  input$bun_k3/input$bun_k1 ) )})
  ite_k3_se_react <- reactive({ sqrt(as.numeric(t(c(1, input$uo_k3,  input$bun_k3/input$bun_k1)) %*% vcv_k3 %*% c(1, input$uo_k3,  input$bun_k3/input$bun_k1) )) })
  ite_k3_rec_react <- reactive({ite_k3_react() - qnorm(.975) * ite_k3_se_react() > 0})
  
  output$pol_rec_0 <- renderText({ 
          if(!ite_k1_rec_react() & !ite_k2_rec_react() & !ite_k3_rec_react()) "No RRT initiation on the first three days following the occurence of stage 3 KDIGO-AKI."
  })
  
  output$pol_rec_1 <- renderText({ 
          if(ite_k1_rec_react()) "RRT initiation on the first day (i.e., within 24 hours after stage 3 KDIGO-AKI occurence)."
  })
  
  output$pol_rec_2 <- renderText({ 
          if(ite_k2_rec_react() & !ite_k1_rec_react()) "RRT initiation on the second day (i.e., between 24 and 48 hours after stage 3 KDIGO-AKI occurence)."
  })

  output$pol_rec_3 <- renderText({ 
          if(ite_k3_rec_react() & !ite_k1_rec_react() & !ite_k2_rec_react()) "RRT initiation on the third day (i.e., between 48 and 72 hours after stage 3 KDIGO-AKI occurence)."
  })
  
  output$plot <- renderPlot({
    
    lancet_red <- "#AD002A99"
    lancet_blue <- "#0099B4FF"
    lancet_gray <- "#ADB6B6FF"
    
    jama_red <- "red"
    jama_blue <- "blue"
            
    
    trans_blue <- rgb(col2rgb(lancet_blue)[1],col2rgb(lancet_blue)[2],col2rgb(lancet_blue)[3],  maxColorValue = 255, alpha = 25)
    trans_red <- rgb(col2rgb(lancet_red)[1],col2rgb(lancet_red)[2],col2rgb(lancet_red)[3], maxColorValue = 255, alpha = 125)
    
    #dev.new(width=3*3, height=3*2, pointsize=11)
    par(mfrow=c(1,3), mar=c(2.25,1,2.5,1)+.1, mgp=c(1.75,0.5,0), oma=c(2.5, 2.5, 1.5, 0), tcl=-0.4)
    
    plot(c(-40,40), c(-20,0),type="n", yaxs="i", axes=F, ylab="", xlab="")
    
    polygon(c(0,par("usr")[1],par("usr")[1],0), c(0, -par("usr")[1]/qnorm(0.025),0,0), col=lancet_gray, border=NA)
    polygon(c(0,par("usr")[2],par("usr")[2],0), c(0, -par("usr")[2]/qnorm(0.975),0,0), col=lancet_gray, border=NA)
    
    
    abline(v=0, lwd=0.75, lty=2)
    arrows(-1, par("usr")[4]+.5, -42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)
    arrows(1, par("usr")[4]+.5, 42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)
    
    mtext(text="recommends RRT initation", side=3, line=1.5, at=2, cex=0.85, adj=0)
    mtext(text="recommends no RRT initation", side=3, line=1.5, at=-2, cex=0.85, adj=1)
    
    mtext("Standard Error", side=2, line=2.25, cex=1.1)
   
    # A positionner
    lgd <- legend(x = mean(c(par("usr")[1],par("usr")[2])), y =  mean(c(par("usr")[3],par("usr")[4])), cex= .9, 
                  c("Your patient", "Initatied RRT", "Did not initiate RRT"), pch=c(19,16,16), col = c("black", "blue", "red"), xpd=T, plot = F)
    
    #polygon(c(par("usr")[1], -5, -5, par("usr")[1]), c(0, 0, 5*1/qnorm(.975), 5*1/qnorm(.975)), col="white", border=NA)
    
    legend(x = par("usr")[1]+9, y =  par("usr")[3] + lgd$rect$h+.25, cex= 1, pt.cex = c(1.75, 1.25, 1.25),
           c("Your patient", "Initiated RRT", "Did not initiate RRT"),
           pch=c(19,16,16), col = c("black", lancet_red, lancet_blue), bty = "n", plot = T)
    
    axis(1, at=seq(-100, 200,by=150), lwd=0.75, lwd.tick=0)
    axis(1, at=seq(-40, 40,by=10), lwd=0, lwd.tick=0.75)
    axis(2, las=1, at=seq(0, -20, by=-5), labels=seq(0, 20, by=5), lwd=0.75, lwd.tick=0.75)
    
    #### REACTIVE PART HERE
    
    if(input$mimic){
    points(ite_k1[actual_ttts$a1==0], -ite_se_k1[actual_ttts$a1==0], col=trans_blue, pch=16, cex=.9)
    points(ite_k1[actual_ttts$a1==1], -ite_se_k1[actual_ttts$a1==1], col=trans_red, pch=16, cex=.9, add=TRUE)
    }
    
    if(input$akiki){    
            points(ite_k1_val[actual_ttts_val$a1==0], -ite_se_k1_val[actual_ttts_val$a1==0], col=trans_blue, cex=.9, pch=16)
            points(ite_k1_val[actual_ttts_val$a1==1], -ite_se_k1_val[actual_ttts_val$a1==1], col=trans_red, pch=16, cex=.9, add=TRUE)
    }
    
    if(ite_k1_rec_react()){
    points(ite_k1_react(), -ite_k1_se_react(), cex=3, pch=19, col=jama_red)
    mtext(paste0(format(round(ite_k1_react(), 1), nsmall=1 ), " HFD (", 
                 paste0(format(round(ite_k1_react() - qnorm(.975) * ite_k1_se_react(),1), nsmall=1 ), " to ", format(round(ite_k1_react() + qnorm(.975) * ite_k1_se_react(),1), nsmall=1 ))
                 , ")"), side=1, line=3.75, cex=.83, col=jama_red)
    } else{
    points(ite_k1_react(), -ite_k1_se_react(), cex=3, pch=19, col=jama_blue)
    mtext(paste0(format(round(ite_k1_react(), 1), nsmall=1 ), " HFD (", 
                 paste0(format(round(ite_k1_react() - qnorm(.975) * ite_k1_se_react(),1), nsmall=1 ), " to ", format(round(ite_k1_react() + qnorm(.975) * ite_k1_se_react(),1), nsmall=1 ))
                 , ")"), side=1, line=3.75, cex=.83, col=jama_blue)
        }
            
    mtext("Blip (first day decision)", side=1, line=2.25, cex=1.1)

    plot(c(-40,40), c(-20,0),type="n", yaxs="i", axes=F, ylab="", xlab="")
    polygon(c(0,par("usr")[1],par("usr")[1],0), c(0, -par("usr")[1]/qnorm(0.025),0,0), col=lancet_gray, border=NA)
    polygon(c(0,par("usr")[2],par("usr")[2],0), c(0, -par("usr")[2]/qnorm(0.975),0,0), col=lancet_gray, border=NA)
    
    
    abline(v=0, lwd=0.75, lty=2)
    arrows(-1, par("usr")[4]+.5, -42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)
    arrows(1, par("usr")[4]+.5, 42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)
    
    mtext(text="recommends RRT initation", side=3, line=1.5, at=2, cex=0.85, adj=0)
    mtext(text="recommends no RRT initation", side=3, line=1.5, at=-2, cex=0.85, adj=1)
    
    axis(1, at=seq(-100, 200,by=150), lwd=0.75, lwd.tick=0)
    axis(1, at=seq(-40, 40,by=10), lwd=0, lwd.tick=0.75)
    axis(2, las=1, at=seq(0, -20, by=-5), labels=seq(0, 20, by=5), lwd=0.75, lwd.tick=0.75)
    
    #### REACTIVE PART HERE
    
    if(input$mimic){
    points(ite_k2[actual_ttts$a2==0], -ite_se_k2[actual_ttts$a2==0], col=trans_blue, pch=16, cex=.9)
    points(ite_k2[actual_ttts$a2==1], -ite_se_k2[actual_ttts$a2==1], col=trans_red, pch=16, cex=.9, add=TRUE)
    }
    
    if(input$akiki){    
            points(ite_k2_val[actual_ttts_val$a2==0], -ite_se_k2_val[actual_ttts_val$a2==0], col=trans_blue, cex=.9, pch=16)
            points(ite_k2_val[actual_ttts_val$a2==1], -ite_se_k2_val[actual_ttts_val$a2==1], col=trans_red, cex=.9, pch=16, add=TRUE)
    }
    
    if(ite_k2_rec_react()){
            points(ite_k2_react(), -ite_k2_se_react(), cex=3, pch=19, col=jama_red)
            mtext(paste0(format(round(ite_k2_react(), 1), nsmall=1 ), " HFD (", 
                         paste0(format(round(ite_k2_react() - qnorm(.975) * ite_k2_se_react(),1), nsmall=1 ), " to ", format(round(ite_k2_react() + qnorm(.975) * ite_k2_se_react(),1), nsmall=1 ))
                         , ")"), side=1, line=3.75, cex=.83, col=jama_red)
    } else{
            points(ite_k2_react(), -ite_k2_se_react(), cex=3, pch=19, col=jama_blue)
            mtext(paste0(format(round(ite_k2_react(), 1), nsmall=1 ), " HFD (", 
                         paste0(format(round(ite_k2_react() - qnorm(.975) * ite_k2_se_react(),1), nsmall=1 ), " to ", format(round(ite_k2_react() + qnorm(.975) * ite_k2_se_react(),1), nsmall=1 ))
                         , ")"), side=1, line=3.75, cex=.83, col=jama_blue)
    }
    
    mtext("Blip (second day decision)", side=1, line=2.25, cex=1.1)

    plot(c(-40,40), c(-20,0),type="n", yaxs="i", axes=F, ylab="", xlab="")
    polygon(c(0,par("usr")[1],par("usr")[1],0), c(0, -par("usr")[1]/qnorm(0.025),0,0), col=lancet_gray, border=NA)
    polygon(c(0,par("usr")[2],par("usr")[2],0), c(0, -par("usr")[2]/qnorm(0.975),0,0), col=lancet_gray, border=NA)
    
    
    abline(v=0, lwd=0.75, lty=2)
    arrows(-1, par("usr")[4]+.5, -42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)
    arrows(1, par("usr")[4]+.5, 42, par("usr")[4]+.5, length = 0.07, lwd=0.5, xpd=TRUE)
    
    mtext(text="recommends RRT initation", side=3, line=1.5, at=2, cex=0.85, adj=0)
    mtext(text="recommends no RRT initation", side=3, line=1.5, at=-2, cex=0.85, adj=1)
    
    axis(1, at=seq(-100, 200,by=150), lwd=0.75, lwd.tick=0)
    axis(1, at=seq(-40, 40,by=10), lwd=0, lwd.tick=0.75)
    axis(2, las=1, at=seq(0, -20, by=-5), labels=seq(0, 20, by=5), lwd=0.75, lwd.tick=0.75)
    
    #### REACTIVE PART HERE
    
    if(input$mimic){
    points(ite_k3[actual_ttts$a3==0], -ite_se_k3[actual_ttts$a3==0], col=trans_blue, cex=.9, pch=16)
    points(ite_k3[actual_ttts$a3==1], -ite_se_k3[actual_ttts$a3==1], col=trans_red, cex=.9, pch=16, add=TRUE)
    }
    
    if(input$akiki){    
            points(ite_k3_val[actual_ttts_val$a3==0], -ite_se_k3_val[actual_ttts_val$a3==0], col=trans_blue, cex=.9, pch=16)
            points(ite_k3_val[actual_ttts_val$a3==1], -ite_se_k3_val[actual_ttts_val$a3==1], col=trans_red, cex=.9, pch=16, add=TRUE)
    }
    
    if(ite_k3_rec_react()){
            points(ite_k3_react(), -ite_k3_se_react(), cex=3, pch=19, col=jama_red)
            mtext(paste0(format(round(ite_k3_react(), 1), nsmall=1 ), " HFD (", 
                         paste0(format(round(ite_k3_react() - qnorm(.975) * ite_k3_se_react(),1), nsmall=1 ), " to ", format(round(ite_k3_react() + qnorm(.975) * ite_k3_se_react(),1), nsmall=1 ))
                         , ")"), side=1, line=3.75, cex=.83, col=jama_red)
    } else{
            points(ite_k3_react(), -ite_k3_se_react(), cex=3, pch=19, col=jama_blue)
            mtext(paste0(format(round(ite_k3_react(), 1), nsmall=1 ), " HFD (", 
                         paste0(format(round(ite_k3_react() - qnorm(.975) * ite_k3_se_react(),1), nsmall=1 ), " to ", format(round(ite_k3_react() + qnorm(.975) * ite_k3_se_react(),1), nsmall=1 ))
                         , ")"), side=1, line=3.75, cex=.83, col=jama_blue)
    }
    
    mtext("Blip (third day decision)", side=1, line=2.25, cex=1.1)
    
  })
  
}