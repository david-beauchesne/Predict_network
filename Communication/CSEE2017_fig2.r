rm(list=ls())
setwd("/Users/davidbeauchesne/Dropbox/PhD/PhD_obj2/Structure_Comm_EGSL/EGSL_species_distribution/")

# Libraries
    library(reshape2)
    library(tidyr)
    library(dplyr)
    library(magrittr)
    library(Rcpp)
    library(RcppArmadillo)
    library(coda)
    library(HMSC)
    library(beanplot)
    library(corrplot)
    library(circlize)

# Add fonts
    #install.packages('showtext', dependencies = TRUE)
    library(showtext)
    showtext.auto()
    # xx <- font.files()
    font.add(family = 'Triforce', regular = 'Triforce.ttf')
    font.add(family = 'regText', regular = 'Alegreya-Regular.otf')
    # font.add(family = 'Hylia', regular = 'HyliaSerifBeta-Regular.otf')
    # font.add(family = 'HylianSymbols', regular = 'HylianSymbols.ttf')

# Functions
source('/Users/davidbeauchesne/dropbox/phd/phd_rawdata/script/function/eplot.r')

# Load files
    R2 <- readRDS('./RData/modelR2.rds') # Load individual model thur's R2
    R2comm <- readRDS('./RData/modelR2comm.rds') # Load mean model thur's R2
    modelAUC <- readRDS('./RData/modelAUC.rds') # Load individual model thur's R2

# Prevalence
    prevSp <- colSums(northPluri_HMSC$Y)

paleGreen <- '#CFD05F'
darkGreen <- '#56872F'
maxPrev <- 800
meanAUC <- colMeans(modelAUC)
sdAUC <- apply(modelAUC, MARGIN = 2, FUN = sd)

jpeg('/users/davidbeauchesne/desktop/AUROC_R2.jpeg', width = 10, height = 10, res = 300, units = 'in')
    par(bg = 'transparent', family = 'zelda')
    eplot(xmin = 0, xmax = maxPrev, ymin = 0, ymax = 2.1)
    lines(x = c(0,maxPrev), y = rep(R2comm+1.1, 2), col = paleGreen, lwd = 2) # R2 mean
    lines(x = c(0,maxPrev), y = rep(0.5, 2), col = 'grey', lwd = 2) # AUROC random
    lines(x = c(0,maxPrev), y = rep(mean(meanAUC), 2), col = paleGreen, lwd = 2) # AUROC mean
    axis(side = 1, at = seq(0, maxPrev, by = 100), las = 1, pos = -0.02, family = 'regText', cex.lab = 1.5)
    axis(side = 2, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = -0.02, family = 'regText')
    axis(side = 2, at = seq(0, 1, by = 0.2)+1.1, labels = seq(0, 1, by = 0.2), las = 1, pos = -0.02, family = 'regText')
    axis(side = 3, at = seq(0, maxPrev, by = 100), pos = 2.12, family = 'regText')
    axis(side = 4, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = 800.02, family = 'regText')
    axis(side = 4, at = seq(0, 1, by = 0.2)+1.1, labels = seq(0, 1, by = 0.2), las = 1, pos = 800.02, family = 'regText')
    abline(h = 1.05, col = "grey", lty = 2)
    text(x = maxPrev-10, y = 2.05, labels = 'Explanatory power of the model', font = 1, cex = 1.5, adj = 1)
    text(x = maxPrev-10, y = .05, labels = 'Monte Carlo cross-validation', font = 1, cex = 1.5, adj = 1)
    mtext(text = expression(R^2), side = 2, line = 2, at = 1.6, font = 1, cex = 1.5)
    mtext(text = 'AUC', side = 2, line = 2, at = 0.5, font = 1, cex = 1.5)
    # R2
        points(prevSp, (R2+1.1), cex = 1, pch="*", las=1, col = darkGreen)
    # AUROC
        arrows(x0 = prevSp, x1 = prevSp, y0 = (meanAUC - sdAUC), y1 = (meanAUC + sdAUC), code = 3, angle = 90, length = 0.05, col = darkGreen)
        points(prevSp, meanAUC, cex = 1, pch="*", las=1, col = darkGreen)
dev.off()


# Example of labeling points DO WITH GADUS MORHUA
attach(mtcars)
plot(wt, mpg, main="Milage vs. Car Weight",
  	xlab="Weight", ylab="Mileage", pch=18, col="blue")
text(wt, mpg, row.names(mtcars), cex=0.6, pos=4, col="red")
