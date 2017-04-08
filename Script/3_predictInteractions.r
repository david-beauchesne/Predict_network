rm(list=ls())
setwd("/Users/davidbeauchesne/Dropbox/PhD/PhD_obj2/Structure_Comm_EGSL/Predict_network/")

# Functions
    source('./Script/iEat.r')
    source('./Script/iEat_to_foodWeb.r')

# Load files
    # Species occurrence probabilities for the St. Lawrence
        predEGSL <- readRDS('../EGSL_species_distribution/RData/predEGSL.rds')

    # EGSL species list from JSDM analysis
        sp <- readRDS('./RData/spEGSL.rds')
        nSp <- nrow(sp) # number of taxa

    # Interactions catalog
        # TODO : eventually redo the whole catalog in order to be more inclusive in species considered to build it
        # TODO : I will undoubtedly have to do it anyways, because I have no primary productivity or detritivore activity incorporated in the catalog so far. There should therefore be a significant number of taxa without resources in the results using this version of the catalog
        S0 <- readRDS("./RData/S0.rds")
        S0 <- S0[, c('taxon','resource','consumer')]
        colnames(S0) <- c('taxon','target','source')

    # Similarity matrices
        simConsumers <- readRDS("./RData/Similarity_consumers.rds")
        simResources <- readRDS("./RData/Similarity_resources.rds")

    # Evaluate species co-occurrence based on threshold
        threshold <- 0.5
        predBin <- matrix(ncol = ncol(predEGSL), nrow = nrow(predEGSL), data = as.numeric(predEGSL >= threshold), dimnames = dimnames(predEGSL))

    # Predict interactions
        # Create list per grid cell, we can then go from there
        nCell <- nrow(predBin)
        networkPredict <- vector('list', nCell)
        names(networkPredict) <- rownames(predBin)

        pb <- txtProgressBar(min = 0, max = nCell, style = 3)
        for(i in 1:nCell) {
            taxa <- colnames(predBin)[predBin[i, ] == 1]
            taxaNames <- sp[sp[, 'EspGen'] %in% taxa,'N_EspSci']
            networkPredict[[i]] <- iEat(S0 = S0,
                                        S1 = taxaNames,
                                        S2 = taxaNames,
                                        sourceSim = simConsumers,
                                        targetSim = simResources,
                                        K = 5,
                                        minSim = 0.3,
                                        minWt = 1,
                                        predict = 'full algorithm')
            setTxtProgressBar(pb, i)
        }#i
        close(pb)

        saveRDS(networkPredict, file = './RData/networkPredict.rds')

    # Predictions as food web
        networkFoodWeb <- vector('list', nCell)
        for(i in 1:nCell) {
            if(nrow(networkPredict[[i]]) == 0) {
                next
            } else {
                networkFoodWeb[[i]]<- iEat_to_foodWeb(networkPredict[[i]])
            }
        }

        saveRDS(networkFoodWeb, file = './RData/networkFoodWeb.rds')
