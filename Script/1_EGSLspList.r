rm(list=ls())
setwd("/Users/davidbeauchesne/Dropbox/PhD/PhD_obj2/Structure_Comm_EGSL/Predict_network/")

library(reshape2)
library(tidyr)
library(dplyr)
library(sp)
library(rgdal)
library(stringr)

source('../Interaction_catalog/Script/taxo_resolve.r')
source('../Interaction_catalog/Script/taxon_resolve_for_classification.r')
source('../Interaction_catalog/Script/taxo_valid.r')

# Load files
    # EGSL species list from JSDM analysis
        northPluri <- readRDS('../EGSL_species_distribution/RData/northPluriCor.rds')
        sp <- northPluri[, c('EspGen','N_EspSci')] %>%
                .[!duplicated(.), ] %>%
                .[order(.[, 'EspGen']), ]
        sp[, 'N_EspSci'] <- gsub(' sp.', '', sp[, 'N_EspSci'])

        nSp <- nrow(sp) # number of taxa

# CaRMS St.Lawrence  species list
    egslCaRMS <- read.table(file = '../Interaction_catalog/RawData/EGSL/CaRMS_checklist_20160516_mod.txt', sep = '\t', header = TRUE)

    # Remove duplicates
        egslCaRMS <- egslCaRMS[, c('ScientificName','Kingdom','Phylum','Class','Order','Family','Genus','Species')]
        egslCaRMS <- egslCaRMS[!duplicated(egslCaRMS), ]

        taxoCaRMS <- paste(egslCaRMS[, 'Kingdom'], egslCaRMS[, 'Phylum'], egslCaRMS[, 'Class'], egslCaRMS[, 'Order'], egslCaRMS[, 'Family'], egslCaRMS[, 'Genus'], egslCaRMS[, 'ScientificName'], sep = ' | ')

    # Interactions catalog
        load("../Predict_interactions/RData/S0_catalog.RData")

# Characterize taxonomy of taxa in EGSL taxa list
    # Identify which taxa are not already included in the catalog
        missCatalog <- which(!sp[, 'N_EspSci'] %in% S0_catalog[, 'taxon'])

    # Extract taxonomy of taxa that are already described in the catalog
        taxonomy <- character(nrow(sp))
        spTaxo <- S0_catalog[which(S0_catalog[, 'taxon'] %in% sp[, 'N_EspSci']), c('taxon','taxonomy')]

        for(i in 1:nrow(spTaxo)) {
            taxon <- which(sp[, 'N_EspSci'] == spTaxo[i, 'taxon'])
            taxonomy[taxon] <- spTaxo[i, 'taxonomy']
        }

# Fill in available taxonomy from CaRMS species list
    spCaRMS <- egslCaRMS[which(egslCaRMS[, 'ScientificName'] %in% sp[missCatalog, 'N_EspSci']), ]
        for(i in 1:nrow(spCaRMS)) {
            taxon <- which(sp[, 'N_EspSci'] == spCaRMS[i, 'ScientificName'])
            taxonomy[taxon] <- paste(spCaRMS[i, 'Kingdom'], spCaRMS[i, 'Phylum'], spCaRMS[i, 'Class'], spCaRMS[i, 'Order'], spCaRMS[i, 'Family'], spCaRMS[i, 'Genus'], spCaRMS[i, 'ScientificName'], sep = ' | ')
        }



# Manual changes from taxoResolve...
    missTaxo <- sp[which(taxonomy == ''), 'N_EspSci']
    # S0_catalog[grep('Spirontocaris', S0_catalog[, 'taxonomy']), 'taxonomy']

    taxonomy[which(sp[, 'N_EspSci'] == 'Porifera')] <- 'Animalia | Porifera | NA | NA | NA | NA | NA'
    taxonomy[which(sp[, 'N_EspSci'] == 'Bryozoa')] <- 'Animalia | Bryozoa | NA | NA | NA | NA | NA'
    taxonomy[which(sp[, 'N_EspSci'] == 'Naticidae')] <- 'Animalia | Mollusca | Gastropoda | Littorinimorpha | Naticoidea | Naticidae | NA'
    taxonomy[which(sp[, 'N_EspSci'] == 'Eusergestes arcticus')] <- 'Animalia | Arthropoda | Malacostraca | Decapoda | Sergestidae | Eusergestes | Eusergestes arcticus'
    taxonomy[which(sp[, 'N_EspSci'] == 'Ascidiacea')] <- "Animalia | Chordata | Ascidiacea | NA | NA | NA | NA"
    taxonomy[which(sp[, 'N_EspSci'] == 'Duva florida')] <- 'Animalia | Cnidaria | Anthozoa | Alcyonacea | Nephtheidae | Duva | Duva florida'
    taxonomy[which(sp[, 'N_EspSci'] == 'Stephanauge')] <- 'Animalia | Cnidaria | Anthozoa | Actiniaria | Hormathiidae | Stephanauge | NA'
    taxonomy[which(sp[, 'N_EspSci'] == 'Margarites')] <- 'Animalia | Molusca | Gastropoda | NA | Margaritidae | Margarites | NA'
    taxonomy[which(sp[, 'N_EspSci'] == 'Actinauge')] <- "Animalia | Cnidaria | Anthozoa | Actiniaria | Hormathiidae | Actinauge | NA"
    taxonomy[which(sp[, 'N_EspSci'] == 'Bolocera')] <- 'Animalia | Cnidaria | Anthozoa | Actiniaria | Actiniidae | Bolocera | NA'
    taxonomy[which(sp[, 'N_EspSci'] == 'Actinostola')] <- 'Animalia | Cnidaria | Anthozoa | Actiniaria | Actinostolidae | Actinostola | NA'
    taxonomy[which(sp[, 'N_EspSci'] == 'Pteraster pulvillus')] <- 'Animalia | Echinodermata | Asteroidea | Velatida | Pterasteridae | Pteraster | Pteraster pulvillus'
    taxonomy[which(sp[, 'N_EspSci'] == 'Cuspidaria')] <- 'Animalia | Molusca | Bivalvia | NA | Cuspidariidae | Cuspidaria | NA'
    taxonomy[which(sp[, 'N_EspSci'] == 'Epizoanthus')] <- 'Animalia | Cnidaria | Anthozoa | Zoanthidea | Epizoanthidae | Epizoanthus | NA'
    taxonomy[which(sp[, 'N_EspSci'] == 'Spirontocarisnus')] <- 'Animalia | Arthropoda | Malacostraca | Decapoda | Hippolytidae | Spirontocaris | NA'
    taxonomy[which(sp[, 'N_EspSci'] == 'Eumicrotremusnosus')] <- 'Animalia | Arthropoda | Malacostraca | Decapoda | Hippolytidae | Spirontocaris | Spirontocaris spinus'


# Final list with taxonomy
    sp <- cbind(sp,taxonomy)

# Save file
    saveRDS(sp, file = './RData/spEGSL.rds')
