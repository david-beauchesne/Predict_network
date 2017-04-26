rm(list=ls())
setwd("/Users/davidbeauchesne/Dropbox/PhD/PhD_obj2/Structure_Comm_EGSL/Predict_network/")

# Libraries
    library(reshape2)
    library(tidyr)
    library(dplyr)
    library(sp)
    library(rgdal)

# Add fonts
    #install.packages('showtext', dependencies = TRUE)
    library(showtext)
    showtext.auto()
    xx <- font.files()
    font.add(family = 'Triforce', regular = 'Triforce.ttf')
    font.add(family = 'regText', regular = 'Alegreya-Regular.otf')
    # font.add(family = 'Hylia', regular = 'HyliaSerifBeta-Regular.otf')
    # font.add(family = 'HylianSymbols', regular = 'HylianSymbols.ttf')


# Functions
    source('../../../PhD_RawData/script/function/colorBar.r')
    source('../../../PhD_RawData/script/function/plotEGSL.r')

# Load files
    # Species occurrence probabilities for the St. Lawrence
        predEGSL <- readRDS('../EGSL_species_distribution/RData/predEGSL.rds')

    # EGSL species list from JSDM analysis
        sp <- readRDS('./RData/spEGSL.rds')
        nSp <- nrow(sp) # number of taxa

    # Complete list of EGSL species
        load("../Interaction_catalog/RData/sp_egsl.RData")

    # Network predictions with interactions per cell
        networkPredict <- readRDS('./RData/networkPredict.rds')
        networkFoodWeb <- readRDS('./RData/networkFoodWeb.rds')

    # Load grid
        egsl_grid <- readOGR(dsn = "../../../PhD_obj0/Study_Area/RData/", layer = "egsl_grid") # Load grid

    # Measure link density
        linkDensity <- numeric(length(networkFoodWeb))
        for(i in 1:length(networkFoodWeb)){
            if(is.null(networkFoodWeb[[i]])) {
                next
            } else {
                linkDensity[i] <- sum(networkFoodWeb[[i]]) / nrow(networkFoodWeb[[i]])
            }
        }

# EGSL contour
egsl <- readOGR(dsn = "/Users/davidbeauchesne/Dropbox/PhD/PhD_RawData/data/EGSL", layer = "egsl")
# egsl <- rgeos::gSimplify(egsl, 100, topologyPreserve=TRUE)
# clip <- rgeos::gIntersection(egsl_grid, egsl, byid = TRUE, drop_lower_td = TRUE)

    # Plot
    # Color palette
        rbPal <- colorRampPalette(c('#B8CEED','#0E1B32')) # color palette
        cols <- rbPal(50)[as.numeric(cut(linkDensity, breaks = 50))]
        textCol <- '#21120A'

        # pdf('/users/davidbeauchesne/dropbox/phd/phd_obj2/Structure_Comm_EGSL/Predict_network/communication/linkDensity.pdf', width = 13, height = 13, pointsize = 25) # , units = 'in', res = 300
        png('/users/davidbeauchesne/dropbox/phd/phd_obj2/Structure_Comm_EGSL/Predict_network/communication/linkDensity_CHONeII.png', width = 13, height = 13, pointsize = 25, units = 'in', res = 300)
            layout(matrix(c(1,1,1,0,2,0),ncol=2), width = c(9,1), height = c(3,3,3))
            # layout(matrix(c(1,1,0,1,1,2,1,1,0),ncol=3), width = c(3,3,3), height = c(9,1))
            par(mar = c(0,0,0,0), bg = 'transparent', family = 'regText', col.lab = textCol, col.axis = textCol, col = textCol)
            plot(egsl_grid, col = cols, border = cols)
            # text(x = 527500, y = 950000, labels = 'Link density (l/s)', cex = 1.5, family = 'Triforce', adj = 0.5)
            # text(x = 527500, y = 925000, labels = 'Link density (l/s)', cex = 2, family = 'Triforce', adj = 1)
            plot(egsl, border = 'black', col = 'transparent', add = TRUE) # plot EGSL contour
            # colorBar(rbPal(50), min = round(min(linkDensity, na.rm = T),1), max = round(max(linkDensity, na.rm = T),1), align = 'horizontal')
            colorBar(rbPal(50), min = round(min(linkDensity, na.rm = T),1), max = round(max(linkDensity, na.rm = T),1), align = 'vertical')
        dev.off()
