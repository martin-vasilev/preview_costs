
# Martin R. Vasilev, 2019

#################################################################################################################
#                                           Prediction graph                                                    #
#################################################################################################################

#------------------------#
#        Panel 1         #
#------------------------#

library(pBrackets)
colours<- c('darkred', 'darkblue', '#8a8a8a', 'burlywood1')

pdf('pred_plot.pdf', width = 10, height = 5, paper='special')
layout(mat = matrix(c(1,2),nrow = 1,ncol = 2,byrow = TRUE))#, widths = c(0.4,0.2, 0.4))
#par(mar=c(3,3,3,3))

plot(NA, type= 'l', ylab= "", xlab= "", lwd=2, cex.main=2,family="serif",
     xlim = c(1, 2), ylim = c(200, 265), yaxt='n', xaxt='n', main= 'a)', cex.main=2, cex.lab=2)

axis(1, at= c( 1.2, 1.8), labels = c('valid preview', 'letter mask'), cex.axis=1.6, family="serif")

mtext(text= 'Fixation duration', side = 2, line = 0.5, family="serif", cex = 1.6)


# Plot data points
deg0<- c(220, 250)
deg20<- c(240, 240)

# 0% degradation:
lines(y = deg0, x= c(1.2, 1.8), lwd=3, lty= 1, col= colours[1])
points(y = deg0, x= c(1.2, 1.8), pch= 15, cex=2, col= colours[1])

# 20% degradation:
lines(y = deg20, x= c(1.2, 1.8), lwd=3, lty= 2, col= colours[2])
points(y = deg20, x= c(1.2, 1.8), pch= 17, cex=2, col= colours[2])

# Cost & benefit labels:
# benefit:
brackets(x1 = 1.165, y1 = deg0[1], x2 = 1.165, y2 = deg20[1], h = 0.08, lwd=2, col = colours[3])
text(x = 1.05, y = 230, labels= 'preview benefit', srt= 90, family="serif", cex = 1.6, col = colours[3])

# cost:
brackets(x1 = 1.835, y1 = deg0[2], x2 = 1.835, y2 = deg20[2], h = 0.06, lwd=2, col = colours[3])
text(x = 1.925, y = mean(c(deg0[2], deg20[2])), labels= 'preview cost', srt= -90, family="serif", cex = 1.6, col = colours[3])

# legend:
op <- par(family = "serif")
legend(x = 1.65, y = 220, title = expression(bold('Degradation')), legend=c("0 %", "20 %"), col = colours[1:2], lwd= 2,
       lty = c(1,2), pch = c(15, 17), seg.len=2.7, cex = 1.3, box.col = "white")


#------------------------#
#         Panel 3        #
#------------------------#
plot(NA, type= 'l', ylab= "", xlab= "", lwd=2, cex.main=2,family="serif",
     xlim = c(1, 2), ylim = c(200, 265), yaxt='n', xaxt='n', main= 'b)', cex.main=2, cex.lab=2)

axis(1, at= c( 1.2, 1.8), labels = c('valid preview', 'letter mask'), cex.axis=1.6, family="serif")

mtext(text= 'Fixation duration', side = 2, line = 0.5, family="serif", cex = 1.6)

rect(xleft = 1.2, ybottom = deg20[1], xright = 1.8, ytop = deg20[1]+15, col = colours[4], border = NA)
arrows(x0 = 1.5, y0 = deg20[1], x1 = 1.5, y1 = deg20[1]+10, length = 0.1, angle = 30, col = colours[2], lty = 1, lwd=1.3)
lines(x = c(1.2, 1.8), y= c(deg20[1], deg20[1]), lty= 2, col= colours[2], lwd=1.3)


# 0% degradation:
lines(y = deg0, x= c(1.2, 1.8), lwd=3, lty= 1, col= colours[1])
points(y = deg0, x= c(1.2, 1.8), pch= 15, cex=2, col= colours[1])

# 20% degradation:
lines(y = deg20+15, x= c(1.2, 1.8), lwd=3, lty= 2, col= colours[2])
points(y = deg20+15, x= c(1.2, 1.8), pch= 17, cex=2, col= colours[2])



dev.off()



#------------------------#
#  Prediction - Panel 2  #
#------------------------#

pdf('pred_plot_P2.pdf', width = 9, height = 4, paper='special')

plot(NA, type= 'n', ylab= "", xlab= "", lwd=2, cex.main=2,family="serif",
     xlim = c(1, 2), ylim = c(200, 275), axes=FALSE,ann=FALSE, main= '+', cex.main=2, cex.lab=2)
#axis(1, at= c( 1.2, 1.8), labels = c('valid', 'invalid'), cex.axis=2, family="serif")

# Draw Uniform preview cost
rect(xleft = 1.2, ybottom = 210, xright = 1.8, ytop = 240, col = colours[4])

#rect(xleft = 210, ybottom = 1.2, xright = 260, ytop = 1.8, col = colours[4])
#text(x = 1.5, y = 245, labels= 'display change awareness cost', srt= 0, family="serif", cex = 1.85, col = colours[3])

#mtext(text= '+', side = 2, line = 0, family="serif", cex = 2, font = 2)
par(xpd = TRUE) 
text(x = 2, y = 225, labels= '=', srt= 0, family="serif", cex = 3.5, font=2)
text(x = 1, y = 225, labels= '+', srt= 0, family="serif", cex = 3.5, font=2)


#brackets(x1 = 1.17, y1 = 210, x2 = 1.17, y2 = 240, h = 0.06, lwd=2, col = colours[3])
brackets(x1 = 1.2, y1 = 240, x2 = 1.8, y2 = 240, h = 8, lwd=2, col = colours[3])

text(x = 1.5, y = 263, labels= 'display change', srt= 0, family="serif", cex = 1.85, col = colours[3])
text(x = 1.5, y = 255, labels= 'awareness cost', srt= 0, family="serif", cex = 1.85, col = colours[3])


dev.off()
