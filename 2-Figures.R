#### Pre-loads ####
library(latex2exp)
library(mvtnorm)
library(pracma)
FigGen <- T # Set to TRUE or FALSE to save the figures as PDFs or plot them in RStudio
length_axis <- 4
############ Figure 1: Changing the pulling level############
load("Fig1.RData")

par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
    mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))

if(FigGen) { # Setting for saving the PDF image
  pdf(paste0("GM_OSB_Changing_PullingLevel.pdf"), width = , height = )
  par(mar=c(3, 3, 1, 1), cex.axis = 1.8, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
}

color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")

y_min <- min(c(min(bnd), strike, pull_line1, pull_line2, pull_line3))
y_max <- max(c(max(bnd), strike, pull_line1, pull_line2, pull_line3))

matplot(time_line, t(bnd), type = "l", lty = 1, ylim = c(y_min, y_max),
        xlab = "", ylab = "", col = color, lwd = 2)
lines(time_line, pull_line1, lty = 2, col = color[1], lwd = 2)
lines(time_line, pull_line3, lty = 2, col = color[3], lwd = 2)
lines(time_line, pull_line2, lty = 2, col = color[2], lwd = 2)
lines(c(0, expiration), rep(strike, 2), lty = 3)
if(FigGen){
  legend(x = 0.48, y = 1.15, bty = "n", title = "",
         legend = c(TeX(paste("$\\alpha(t) = -1$")),
                    TeX(paste("$\\alpha(t) = \\Phi(25 - 50t) - 0.5$")),
                    TeX(paste("$\\alpha(t) = 1$"))),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2)
} else {
  legend(x = 0.53, y = 1.1, bty = "n", title = "",
         legend = c(TeX(paste("$\\alpha(t) = -1$")),
                    TeX(paste("$\\alpha(t) =$", "$\\Phi(25 - 50t) - 0.5$")),
                    TeX(paste("$\\alpha(t) = 1$"))),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}

err_max <- max(err[[1]], err[[2]], err[[3]])
err_min <- min(err[[1]], err[[2]], err[[3]])

plot(1:length(err[[1]]), err[[1]], pch = 8, col = color[1], 
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[1]]), l = length_axis)))
lines(1:length(err[[1]]), err[[1]], lty = 1, col = color[1])
plot(1:length(err[[2]]), err[[2]], pch = 8, col = color[2],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[2]]), l = length_axis)))
lines(1:length(err[[2]]), err[[2]], lty = 1, col = color[2])
plot(1:length(err[[3]]), err[[3]], pch = 8, col = color[3],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[3]]), l = length_axis)))
lines(1:length(err[[3]]), err[[3]], lty = 1, col = color[3])

if (FigGen) dev.off()
############ Figure 2: Changing the slope ############
load("Fig2.RData")

par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
    mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))

if(FigGen) { # Setting for saving the PDF image
  pdf(paste0("GM_OSB_Changing_Slope.pdf"), width = , height = )
  par(mar=c(3, 3, 1, 1), cex.axis = 1.8, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
}

color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")

y_min <- min(c(min(bnd), strike, min(pull_line)))
y_max <- max(c(max(bnd), strike, max(pull_line)))

matplot(time_line, t(bnd), type = "l", lty = 1, ylim = c(y_min, y_max),
        xlab = "", ylab = "", col = color, lwd = 2)
lines(time_line, pull_line, lty = 2, lwd = 2)
lines(c(0, expiration), rep(strike, 2), lty = 3)
if(FigGen){
  legend(x = -0.03, y = 0.762, bty = "n", title = "",
         legend = c(TeX(paste("$\\theta(t) = 10$")),
                    TeX(paste("$\\theta(t) = 6\\Phi(25 - 50t) + 2$")),
                    TeX(paste("$\\theta(t) = 1$"))),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2)
} else {
  legend(x = -0.02, y = 0.8, bty = "n", title = "",
         legend = c(TeX(paste("$\\theta(t) =$", 10)),
                    TeX(paste("$\\theta(t) =$", "$6\\Phi(25 - 50t) + 2$")),
                    TeX(paste("$\\theta(t) =$", 1))),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}

err_max <- max(err[[1]], err[[2]], err[[3]])
err_min <- min(err[[1]], err[[2]], err[[3]])

plot(1:length(err[[1]]), err[[1]], pch = 8, col = color[1], 
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[1]]), l = length_axis)))
lines(1:length(err[[1]]), err[[1]], lty = 1, col = color[1])
plot(1:length(err[[2]]), err[[2]], pch = 8, col = color[2],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[2]]), l = length_axis)))
lines(1:length(err[[2]]), err[[2]], lty = 1, col = color[2])
plot(1:length(err[[3]]), err[[3]], pch = 8, col = color[3],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[3]]), l = length_axis)))
lines(1:length(err[[3]]), err[[3]], lty = 1, col = color[3])

if (FigGen) dev.off()
############ Figure 3: Changing the volatility ############
load("Fig3.RData")

par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
    mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))

if(FigGen) { # Setting for saving the PDF image
  pdf(paste0("GM_OSB_Changing_Volatility.pdf"), width = , height = )
  par(mar=c(3, 3, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
}

color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")

y_min <- min(c(min(bnd), strike, min(pull_line)))
y_max <- max(c(max(bnd), strike, max(pull_line)))

matplot(time_line, t(bnd), type = "l", lty = 1, ylim = c(y_min, y_max + 0.9),
        xlab = "", ylab = "", col = color, lwd = 2)
lines(time_line, pull_line, lty = 2, lwd = 2)
lines(c(0, expiration), rep(strike, 2), lty = 3)

if(FigGen){
  legend(x = -0.032, y = 2.31, bty = "n", title = "",
         legend = c(TeX(paste("$\\sigma(t) = 10$")),
                    TeX(paste("$\\sigma(t) = 2(2\\pi)^{1/2}(\\varphi(100 - 25t) + \\varphi(100 - 75t)) + 2$")),
                    TeX(paste("$\\sigma(t) = 1$"))),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2)
} else {
  legend(x = 0.29, y = 2.9, bty = "n", title = "",
         legend = c(TeX(paste("$\\sigma(t) =$", "$10$")),
                    TeX(paste("$\\sigma(t) =$", "$2(2\\pi)^{1/2}(\\varphi(100 - 25t) + \\varphi(100 - 75t)) + 2$")),
                    TeX(paste("$\\sigma(t) =$", "$1$"))),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}

err_max <- max(err[[1]], err[[2]], err[[3]])
err_min <- min(err[[1]], err[[2]], err[[3]])

plot(1:length(err[[1]]), err[[1]], pch = 8, col = color[1], 
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[1]]), l = length_axis)))
lines(1:length(err[[1]]), err[[1]], lty = 1, col = color[1])
plot(1:length(err[[2]]), err[[2]], pch = 8, col = color[2],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[2]]), l = length_axis)))
lines(1:length(err[[2]]), err[[2]], lty = 1, col = color[2])
plot(1:length(err[[3]]), err[[3]], pch = 8, col = color[3],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[3]]), l = length_axis)))
lines(1:length(err[[3]]), err[[3]], lty = 1, col = color[3])

if (FigGen) dev.off()
############ Figure 4: Approximation of a BB ############
load("Fig4.RData")

par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
    mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))

if(FigGen) { # Setting for saving the PDF image
  pdf(paste0("GM_OSB_BB_Approximation.pdf"), width = , height = )
  par(mar=c(3, 3, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
}

color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")

y_min <- min(c(min(bnd), strike, min(pull_line)))
y_max <- max(c(max(bnd), strike, max(pull_line)))

matplot(time_line, t(bnd), type = "l", lty = 1, ylim = c(y_min, y_max),
        xlab = "", ylab = "", col = color, lwd = 2)
lines(time_line, pull_line, lty = 2, lwd = 1)
lines(c(0, expiration), rep(strike, 2), lty = 3)
lines(time_line, strike - 0.8399 * sqrt(expiration - time_line),
      lty = 4, lwd = 2)
if(FigGen){
  legend(x = -0.02, y = 0.035, bty = "n", title = "",
         legend = c(TeX(paste("$n = 0$")),
                    TeX(paste("$n =  10$")),
                    TeX(paste("$n = 100$"))),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2)
} else {
  legend(x = -0.03, y = 0.05, bty = "n", title = "",
         legend = c(TeX(paste("$n =$", 0)),
                    TeX(paste("$n = $", 10)),
                    TeX(paste("$n =$", 100))),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}

err_max <- max(err[[1]], err[[2]], err[[3]])
err_min <- min(err[[1]], err[[2]], err[[3]])

plot(1:length(err[[1]]), err[[1]], pch = 8, col = color[1], 
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[1]]), l = length_axis)))
lines(1:length(err[[1]]), err[[1]], lty = 1, col = color[1])
plot(1:length(err[[2]]), err[[2]], pch = 8, col = color[2],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[2]]), l = length_axis)))
lines(1:length(err[[2]]), err[[2]], lty = 1, col = color[2])
plot(1:length(err[[3]]), err[[3]], pch = 8, col = color[3],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[3]]), l = length_axis)))
lines(1:length(err[[3]]), err[[3]], lty = 1, col = color[3])

if (FigGen) dev.off()
############ Figure 5: Approximation of a OUB ############
load("Fig5.RData")

par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
    mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))

if(FigGen) { # Setting for saving the PDF image
  pdf(paste0("GM_OSB_OUB_Approximation.pdf"), width = , height = )
  par(mar=c(3, 3, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1))
}

color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")

y_min <- min(c(min(bnd), strike, min(pull_line)))
y_max <- max(c(max(bnd), strike, max(pull_line)))

matplot(time_line, t(bnd), type = "l", lty = 1, ylim = c(y_min, y_max),
        xlab = "", ylab = "", col = color, lwd = 2)
lines(time_line, pull_line, lty = 2)
lines(time_line, - OUB_bnd, lty = 4, lwd = 2)
lines(c(0, expiration), rep(strike, 2), lty = 3)
if(FigGen){
  legend(x = 0.742, y = 0.11, bty = "n", title = "",
         legend = c(TeX(paste("$\\epsilon =$", "$0.25$")),
                    TeX(paste("$\\epsilon =$", "$0.05$")),
                    TeX(paste("$\\epsilon =$", "$0.01$"))),
         col = color, lty = 1, cex = 1.9, xpd = TRUE, lwd = 2)
} else {
  legend(x = 0.76, y = 0.18, bty = "n", title = "",
         legend = c(TeX(paste("$\\epsilon = 0.25$")),
                    TeX(paste("$\\epsilon = 0.05$")),
                    TeX(paste("$\\epsilon = 0.01$"))),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}

err_max <- max(err[[1]], err[[2]], err[[3]])
err_min <- min(err[[1]], err[[2]], err[[3]])

plot(1:length(err[[1]]), err[[1]], pch = 8, col = color[1], 
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[1]]), l = length_axis)))
lines(1:length(err[[1]]), err[[1]], lty = 1, col = color[1])
plot(1:length(err[[2]]), err[[2]], pch = 8, col = color[2],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[2]]), l = length_axis)))
lines(1:length(err[[2]]), err[[2]], lty = 1, col = color[2])
plot(1:length(err[[3]]), err[[3]], pch = 8, col = color[3],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[3]]), l = length_axis)))
lines(1:length(err[[3]]), err[[3]], lty = 1, col = color[3])

if (FigGen) dev.off()
############ Figure 6: Changing the discounting rate for partition_length = 5 ############
load("Fig6.RData")

par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
    mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1.2))

if(FigGen) { # Setting for saving the PDF image
  pdf(paste0("GM_OSB_Changing_Discount_N5.pdf"), width = , height = )
  par(mar=c(3, 3, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1.2))
}

color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")

y_min <- min(c(min(bnd), strike, min(pull_line)))
y_max <- max(c(max(bnd), strike, max(pull_line)))

matplot(time_line, t(bnd), type = "l", lty = 1, ylim = c(y_min, y_max),
        xlab = "", ylab = "", col = color, lwd = 2)
lines(time_line, pull_line, lty = 2, lwd = 2)
lines(c(0, expiration), rep(strike, 2), lty = 3)
if(FigGen){
  legend(x = 0.77, y = -0.58, bty = "n", title = "",
         legend = c(TeX(paste("$\\lambda =$", 0)),
                    TeX(paste("$\\lambda =$", 1)),
                    TeX(paste("$\\lambda =$", 5))),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2)
} else {
  legend(x = 0.82, y = -0.42, bty = "n", title = "",
         legend = c(TeX(paste("$\\lambda =$", 0)),
                    TeX(paste("$\\lambda =$", 1)),
                    TeX(paste("$\\lambda =$", 5))),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}

err_max <- max(err[[1]], err[[2]], err[[3]])
err_min <- min(err[[1]], err[[2]], err[[3]])

plot(1:length(err[[1]]), err[[1]], pch = 8, col = color[1], 
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[1]]), l = length_axis)))
lines(1:length(err[[1]]), err[[1]], lty = 1, col = color[1])
plot(1:length(err[[2]]), err[[2]], pch = 8, col = color[2],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[2]]), l = length_axis)))
lines(1:length(err[[2]]), err[[2]], lty = 1, col = color[2])
plot(1:length(err[[3]]), err[[3]], pch = 8, col = color[3],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[3]]), l = length_axis)))
lines(1:length(err[[3]]), err[[3]], lty = 1, col = color[3])

if (FigGen) dev.off()
############ Figure 7: Changing the discounting rate for partition_length = 20 ############
load("Fig7.RData")

par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
    mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1.2))

if(FigGen) { # Setting for saving the PDF image
  pdf(paste0("GM_OSB_Changing_Discount_N20.pdf"), width = , height = )
  par(mar=c(3, 3, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1.2))
}

color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")

y_min <- min(c(min(bnd), strike, min(pull_line)))
y_max <- max(c(max(bnd), strike, max(pull_line)))

matplot(time_line, t(bnd), type = "l", lty = 1, ylim = c(y_min, y_max),
        xlab = "", ylab = "", col = color, lwd = 2)
lines(time_line, pull_line, lty = 2, lwd = 2)
lines(c(0, expiration), rep(strike, 2), lty = 3)
if(FigGen){
  legend(x = 0.77, y = -0.58, bty = "n", title = "",
         legend = c(TeX(paste("$\\lambda = 0$")),
                    TeX(paste("$\\lambda = 1$")),
                    TeX(paste("$\\lambda = 5$"))),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2)
} else {
  legend(x = 0.82, y = -0.42, bty = "n", title = "",
         legend = c(TeX(paste("$\\lambda = 0$")),
                    TeX(paste("$\\lambda = 1$")),
                    TeX(paste("$\\lambda = 5$"))),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}

err_max <- max(err[[1]], err[[2]], err[[3]])
err_min <- min(err[[1]], err[[2]], err[[3]])

plot(1:length(err[[1]]), err[[1]], pch = 8, col = color[1], 
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[1]]), l = length_axis)))
lines(1:length(err[[1]]), err[[1]], lty = 1, col = color[1])
plot(1:length(err[[2]]), err[[2]], pch = 8, col = color[2],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[2]]), l = length_axis)))
lines(1:length(err[[2]]), err[[2]], lty = 1, col = color[2])
plot(1:length(err[[3]]), err[[3]], pch = 8, col = color[3],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[3]]), l = length_axis)))
lines(1:length(err[[3]]), err[[3]], lty = 1, col = color[3])

if (FigGen) dev.off()
############ Figure 8: Changing the discounting rate for partition_length = 200 ############
load("Fig8.RData")

par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
    mgp = c(2, 0.6, 0), tcl = -0.4) # Setting for plotting in RStudio
layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1.2))

if(FigGen) { # Setting for saving the PDF image
  pdf(paste0("GM_OSB_Changing_Discount_N200.pdf"), width = , height = )
  par(mar=c(3, 3, 1, 1), cex.axis = 2, cex.lab = 2,
      mgp = c(2.5, 1.6, 0), fin = c(6, 6.5), tcl = -0.8) # Setting for printing in RStudio
  layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, byrow = TRUE), heights = c(2, 1.2))
}

color <- c("firebrick3", "deepskyblue3", "darkolivegreen3")

y_min <- min(c(min(bnd), strike, min(pull_line)))
y_max <- max(c(max(bnd), strike, max(pull_line)))

matplot(time_line, t(bnd), type = "l", lty = 1, ylim = c(y_min, y_max),
        xlab = "", ylab = "", col = color, lwd = 2)
lines(time_line, pull_line, lty = 2, lwd = 2)
lines(c(0, expiration), rep(strike, 2), lty = 3)
if(FigGen){
  legend(x = 0.77, y = -0.58, bty = "n", title = "",
         legend = c(TeX(paste("$\\lambda = 0$")),
                    TeX(paste("$\\lambda = 1$")),
                    TeX(paste("$\\lambda = 5$"))),
         col = color, lty = 1, cex = 2, xpd = TRUE, lwd = 2)
} else {
  legend(x = 0.82, y = -0.42, bty = "n", title = "",
         legend = c(TeX(paste("$\\lambda =$", 0)),
                    TeX(paste("$\\lambda =$", 1)),
                    TeX(paste("$\\lambda =$", 5))),
         col = color, lty = 1, cex = 1.5, xpd = TRUE, lwd = 2)
}

err_max <- max(err[[1]], err[[2]], err[[3]])
err_min <- min(err[[1]], err[[2]], err[[3]])

plot(1:length(err[[1]]), err[[1]], pch = 8, col = color[1], 
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[1]]), l = length_axis)))
lines(1:length(err[[1]]), err[[1]], lty = 1, col = color[1])
plot(1:length(err[[2]]), err[[2]], pch = 8, col = color[2],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[2]]), l = length_axis)))
lines(1:length(err[[2]]), err[[2]], lty = 1, col = color[2])
plot(1:length(err[[3]]), err[[3]], pch = 8, col = color[3],
     ylim = c(err_min, err_max), xlab = "", ylab = "", xaxt = "n")
axis(side = 1, at = floor(seq(1, length(err[[3]]), l = length_axis)))
lines(1:length(err[[3]]), err[[3]], lty = 1, col = color[3])

if (FigGen) dev.off()