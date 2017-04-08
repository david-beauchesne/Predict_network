# Similarity measurements between all taxa from catalog and St. Lawrence taxa
rm(list=ls())
setwd("/Users/davidbeauchesne/Dropbox/PhD/PhD_obj2/Structure_Comm_EGSL/Predict_network/")

# Functions
    source('../Predict_interactions/script/similarity_taxon.r')
    source('../Predict_interactions/script/tanimoto_traits.r')
    source('../Predict_interactions/script/tanimoto.r')



# Load species list
    sp <- readRDS('./RData/spEGSL.rds')

# Load interactions catalog
    load("../Predict_interactions/RData/S0_catalog.RData")

# Insert missing St. Lawrence taxa in catalog
    missTaxa <- which(!sp[, 'N_EspSci'] %in% S0_catalog[, 'taxon'])
    S0_add <- S0_catalog[1:length(missTaxa), ]
    S0_add <- apply(S0_add, MARGIN = c(1,2), FUN = is.na)
    rownames(S0_add) <- S0_add[, 'taxon'] <- sp[missTaxa, 'N_EspSci']
    S0_add[, 'taxonomy'] <- as.character(sp[missTaxa, 'taxonomy'])
    S0_add[, 3:6] <- ''
    S0 <- rbind(S0_catalog, S0_add)

# Export catalog
    saveRDS(S0, file = './RData/S0.rds')

# Weight values for 2-way similarity measurements
    wt <- 0.5

# 1st is for similarity measured from set of resources and taxonomy, for consumers
    similarity.consumers <- similarity_taxon(S0 = S0, wt = wt, taxa = 'consumer')
    saveRDS(similarity.consumers, file = "./RData/Similarity_consumers.rds")

# 2nd is for similarity measured from set of consumers and taxonomy, for resources
    similarity.resources <- similarity_taxon(S0 = S0, wt = wt, taxa = 'resource')
    saveRDS(similarity.resources, file = "./RData/Similarity_resources.rds")
