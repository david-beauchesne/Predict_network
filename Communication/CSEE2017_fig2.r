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
    xx <- font.files()
    font.add(family = 'Triforce', regular = 'Triforce.ttf')
    font.add(family = 'regText', regular = 'Alegreya-Regular.otf')
    # font.add(family = 'Hylia', regular = 'HyliaSerifBeta-Regular.otf')
    font.add(family = 'HylianSymbols', regular = 'HylianSymbols.ttf')

# Functions
source('/Users/davidbeauchesne/dropbox/phd/phd_rawdata/script/function/eplot.r')

# Load files
    northPluri <- readRDS('./RData/northPluriCor.rds') # northPluri
    northPluri_HMSC <- readRDS('./RData/northPluri_HMSC.rds') # northPluri
    R2 <- readRDS('./RData/modelR2.rds') # Load individual model thur's R2
    R2comm <- readRDS('./RData/modelR2comm.rds') # Load mean model thur's R2
    modelAUC <- readRDS('./RData/modelAUC.rds') # Load individual model thur's R2

# Prevalence
    prevSp <- colSums(northPluri_HMSC$Y)

# Species list
    sp <- northPluri[, c('EspGen','N_EspSci')] %>%
            .[!duplicated(.), ] %>%
            .[order(.[, 'EspGen']), ]

# Identify species for special focus
    spFocus <- c('Hippoglossus hippoglossus','Gadus morhua','Pandalus borealis','Chionoecetes opilio')
    spFocusName <- c('Atlantic halibut', 'Atlantic cod', 'Northern prawn', 'Snow crab')
    spFocusPoints <-numeric(length(spFocus))
    for(i in 1:length(spFocus)) spFocusPoints[i] <- which(names(prevSp) == sp[which(sp[, 'N_EspSci'] == spFocus[i]), 'EspGen'])


# Graph parameters
paleGreen <- '#CFD05F'
darkGreen <- '#56872F'
paleEarth <- '#e0d9a0'
textCol <- '#21120A'
maxPrev <- 800
meanAUC <- colMeans(modelAUC)
sdAUC <- apply(modelAUC, MARGIN = 2, FUN = sd)
png('/users/davidbeauchesne/dropbox/phd/phd_obj2/Structure_Comm_EGSL/Predict_network/communication/AUROC_R2.png', width = 13, height = 13, res = 600, units = 'in', pointsize = 25)
    par(bg = 'transparent', family = 'Triforce', col.lab = textCol, col.axis = textCol, col = textCol)
    eplot(xmin = 0, xmax = maxPrev, ymin = 0, ymax = 2.1)
    abline(h = 1.05, col = textCol, lty = 2, lwd = 2)
    # polygon(x = c(0,0,maxPrev,maxPrev), y = c(-.02,2.12,2.12,-.02), col = paleEarth, border = paleEarth)
    # polygon(x = c(0,0,maxPrev,maxPrev), y = c(-.02,2.12,2.12,-.02), col = darkGreen, border = darkGreen)
    # R2
        points(prevSp[-spFocusPoints], (R2[-spFocusPoints]+1.1), cex = 1, pch="*", las=1, col = darkGreen)
    # AUROC
        arrows(x0 = prevSp[-spFocusPoints], x1 = prevSp[-spFocusPoints], y0 = (meanAUC[-spFocusPoints] - sdAUC[-spFocusPoints]), y1 = (meanAUC[-spFocusPoints] + sdAUC[-spFocusPoints]), code = 3, angle = 90, length = 0.05, col = darkGreen)
        points(prevSp[-spFocusPoints], meanAUC[-spFocusPoints], cex = 1, pch="*", las=1, col = darkGreen)
    # Species focus
        points(prevSp[spFocusPoints], (R2[spFocusPoints]+1.1), cex = 2, pch="k", las=1, col = 'red', family = 'HylianSymbols')
        arrows(x0 = prevSp[spFocusPoints], x1 = prevSp[spFocusPoints], y0 = (meanAUC[spFocusPoints] - sdAUC[spFocusPoints]), y1 = (meanAUC[spFocusPoints] + sdAUC[spFocusPoints]), code = 3, angle = 90, length = 0.05, col = 'red', lwd = 1.5)
        points(prevSp[spFocusPoints], meanAUC[spFocusPoints], cex = 2, pch="k", las=1, col = 'red', family = 'HylianSymbols')
        labelOrder <- c(4,2,3,4)
        for(i in 1:length(spFocus)) text(prevSp[spFocusPoints[i]], (R2[spFocusPoints[i]]+1.1), spFocusName[i], cex=0.8, pos=labelOrder[i], col="red", offset = 0.75)
        labelOrder <- c(1,3,3,4)
        for(i in 1:length(spFocus)) text(prevSp[spFocusPoints[i]], meanAUC[spFocusPoints[i]], spFocusName[i], cex=0.8, pos=labelOrder[i], col="red", offset = 0.75)
    lines(x = c(0,maxPrev), y = rep(R2comm+1.1, 2), col = textCol, lwd = 3) # R2 mean
    lines(x = c(0,maxPrev), y = rep(0.5, 2), col = 'grey', lwd = 3) # AUROC random
    lines(x = c(0,maxPrev), y = rep(mean(meanAUC), 2), col = textCol, lwd = 3) # AUROC mean
    #Axes
    axis(side = 1, at = seq(0, maxPrev, by = 100), las = 1, pos = -0.02, family = 'regText', cex.axis = 1.25, lwd = 3)
    axis(side = 2, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = -0.02, family = 'regText', lwd = 3, cex.axis = 1.25)
    axis(side = 2, at = seq(0, 1, by = 0.2)+1.1, labels = seq(0, 1, by = 0.2), las = 1, pos = -0.02, family = 'regText', lwd = 3, cex.axis = 1.25)
    # axis(side = 3, at = seq(0, maxPrev, by = 100), pos = 2.12, family = 'regText', lwd = 3, cex.axis = 1.25)
    # axis(side = 4, at = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2), las = 1, pos = 800.02, family = 'regText', lwd = 3, cex.axis = 1.25)
    # axis(side = 4, at = seq(0, 1, by = 0.2)+1.1, labels = seq(0, 1, by = 0.2), las = 1, pos = 800.02, family = 'regText', lwd = 3, cex.axis = 1.25)
    text(x = maxPrev-10, y = 2.05, labels = 'Explanatory power of the model', font = 1, cex = 1, adj = 1)
    text(x = maxPrev-10, y = .05, labels = 'Monte Carlo cross-validation', font = 1, cex = 1, adj = 1)
    mtext(text = expression(R^2), side = 2, line = 2, at = 1.6, font = 1, cex = 1.5)
    mtext(text = 'AUC', side = 2, line = 2, at = 0.5, font = 1, cex = 1.5)
    mtext(text = 'Prevalence', side = 1, line = 2, at = maxPrev/2, font = 1, cex = 1.5)
dev.off()
