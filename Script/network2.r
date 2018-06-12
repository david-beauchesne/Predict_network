########----- Network Gadus morhua ------########
####
library(magrittr)
library(webshot)
library(htmlwidgets)
library(DiagrammeR)
library(igraph)

library(showtext)
showtext.auto()
xx <- font.files()
font.add(family = 'Triforce', regular = 'Triforce.ttf')
font.add(family = 'regText', regular = 'Alegreya-Regular.otf')
# font.add(family = 'Hylia', regular = 'HyliaSerifBeta-Regular.otf')
font.add(family = 'HylianSymbols', regular = 'HylianSymbols.ttf')

plot0 <- function(x = 1, y = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1) {
  plot(x = x,
       y = y,
       bty = "n",
       ann = FALSE,
       xaxt = "n",
       yaxt = "n",
       type = "n",
       bg = "grey",
       ylim = c(ymin,ymax),
       xlim = c(xmin,xmax))
}

####
get_connec <- function(x) sum(x)/prod(dim(x))
spFocus <- c('Hippoglossus hippoglossus', 'Gadus morhua', 'Pandalus borealis', 'Chionoecetes opilio')
spFocusName <- c('Atlantic halibut', 'Atlantic cod', 'Northern prawn', 'Snow crab')
metaweb <- readRDS("./RData/metaWeb.rds")
connec <- get_connec(metaweb) ## Curiosity
## Taxon
taxon <- readRDS("./RData/spEGSL.rds")
taxon_grp <- taxon[,3] %>%
  as.character %>%
  strsplit(split="[|]") %>%
  lapply(FUN = function(x) paste(x[2:3], collapse="-")) %>%
  unlist
##
taxon$grp <- gsub(taxon_grp, pat=" ", rep="")
taxon[which(taxon$grp=="Chordata-Actinopteri"),1:2]
taxon$grp[which(taxon$grp=="Chordata-Actinopteri")] <- "Chordata-Actinopterygii"


#### Crabe crevette Flétan
spsc <- c(1,3,4)
sp_nms <- spFocus[spsc]
id <- which(colnames(metaweb) %in% sp_nms)
idpro <- which(metaweb[id[1],] > 0)
prpr <- c()
for (i in id){
  prpr <- c(prpr, which(metaweb[i,] > 0), which(metaweb[, i] > 0))
}
subnet <- which(1:ncol(metaweb)%in%unique(prpr)) # Au lieu d'utiliser unique histoire que ce soit dans le bon ordre
grptax <- taxon$grp[subnet]
subweb <- metaweb[subnet,subnet]
id <- which(colnames(subweb) %in% sp_nms)
# Eliminer les autres liens
for (i in 1:nrow(subweb)) {
  for (j in 1:ncol(subweb)) {
    if (!(i%in%id | j%in%id)) subweb[i,j] = 0
  }
}
nsp <- ncol(subweb)
vec_names <- colnames(subweb)
vec_names[-id] <- 1:(nsp-3)
vec_names[id] <- c("PB", "CO", "HH")
vec_size <- rep(9, nsp)
vec_size[id] <- 18
# vec_colfont <- rep("white",nsp)
# vec_colfont[-id] <- "black"
# sp_nm <- "cracreflé"
netw <-  graph_from_adjacency_matrix(t(subweb))
##
pal <- c("#cc154b", "#f79809", "#11bdec",
  "#0c3179", "#680c88", "#f526f4", "#75da10", "#0b6b43",
  "#888a88", "#6b4308")
vec_col <-  pal[as.numeric(as.factor(grptax))]
##

lay <- layout_nicely(netw)
png(paste0("./fig/figure1.png"), width=7, height=9, units="in", res=300)
  layout(matrix(c(1,2), 2, 1), height=c(.8,.2))
  par(bg="transparent", mar=rep(0.4,4), fg="black", family = 'regText')
  plot(netw,
    vertex.color = vec_col,
    vertex.frame.color = "transparent",
    vertex.label.color = "black",
    vertex.label = vec_names,
    vertex.size = vec_size,
    # edge.width = 1+3*log(links$weight/min(links$weight)),
    layout = lay,
    edge.arrow.size = .5
    # edge.color = factor(links$from, levels=levels(as.factor(nodes$id)))
    )
  plot0()
  legend("center", bty="n", legend=sort(unique(grptax)), cex=1.2, pch=21, ncol=2,
  pt.bg=pal, col=pal, pt.cex=2.4)
    # layout = coords)
dev.off()

## Legend
nms <- colnames(subweb)[-id]
leg_txt <- ""
for (i in 1:length(nms)){
  leg_txt %<>% paste0(i, "-", nms[i], "; ")
}
nms <- colnames(subweb)[id]
for (i in 1:length(nms)){
  leg_txt %<>% paste0(c("PB", "CO", "HH")[i], "-", nms[i], "; ")
}


########## Figure 2

plotMean <- readRDS("../EGSL_species_distribution/RData/plotMean.rds")
subweb2 <- plotMean[subnet,subnet]
# # Eliminer associations < 0.4
# for (i in 1:nrow(subweb)) {
#   for (j in 1:ncol(subweb)) {
#     if (subweb2[i,j] > -.4 & subweb2[i,j] < .4) subweb2[i,j] = 0
#   }
# }
subweb2 <- subweb2[which(subweb > 0)] # sélectionner uniquement les liens relevés pour la figure 1
arrowWidth <- ((abs(subweb2)/min(abs(subweb2)))/50)+1
cols <- floor(subweb2)
cols[cols == 0 | cols == 1] <- '#3CADA2' # associations positives
cols[cols == -1] <- '#B62F34' # associations négatives


##

png(paste0("./fig/figure2.png"), width=7, height=9, units="in", res=300)
  layout(matrix(c(1,2), 2, 1), height=c(.8,.2))
  par(bg="transparent", mar=rep(0.4,4), fg="black", family = 'regText')
  plot(netw,
    vertex.color = vec_col,
    vertex.frame.color = "transparent",
    vertex.label.color = "black",
    vertex.label = vec_names,
    vertex.size = vec_size,
    edge.arrow.size = 0,
    edge.width = arrowWidth,
    edge.color = cols,
    layout = lay
    )
  plot0()
  legend("center", bty="n", legend=sort(unique(grptax)), cex=1.2, pch=21, ncol=2,
  pt.bg=pal, col=pal, pt.cex=2.4)
    # layout = coords)
dev.off()
