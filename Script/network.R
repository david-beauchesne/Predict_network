########----- Network Gadus morhua ------########
####
library(magrittr)
library(webshot)
library(htmlwidgets)
library(DiagrammeR)
library(igraph)
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
  lapply(FUN = function(x) paste(x[1:3], collapse="")) %>%
  unlist
taxon$grp <- taxon_grp


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
png(paste0("./fig/cracreflé.png"), width=7, height=7, units="in", res=300)
par(bg="black", mar=rep(0.4,4))
plot(netw,
  vertex.color = as.factor(grptax),
  vertex.frame.color = "transparent",
  vertex.label.color = "black",
  vertex.label = vec_names,
  vertex.size = vec_size,
  # edge.width = 1+3*log(links$weight/min(links$weight)),
  edge.arrow.size = .8
  # edge.color = factor(links$from, levels=levels(as.factor(nodes$id)))
  )
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

#### Focus Species
for (i in 1:length(spFocus)) {
  sp_nm <- spFocus[i]
  ####
  id <- which(colnames(metaweb) == sp_nm)
  ## row = preys
  idpro <- which(metaweb[id, ] > 0)
  ## col = predators
  idpre <- which(metaweb[, id] > 0)
  ##
  subnet <- which(1:ncol(metaweb)%in%c(id, idpro, idpre))
  grptax <- taxon$grp[subnet]
  subweb <- metaweb[subnet,subnet]
  id <- which(colnames(subweb) == sp_nm)
  for (i in 1:nrow(subweb)) {
    for (j in 1:ncol(subweb)) {
      if (i!=id & j!=id) subweb[i,j] = 0
    }
  }
  #### Designing thed network using igraph
  netw <-  graph_from_adjacency_matrix(t(subweb))
  png(paste0("./fig/", sp_nm, ".png"), width=7, height=7, units="in", res=300)

  par(bg="black", mar=rep(0.4,4))
  plot(netw,
    vertex.color = as.factor(grptax),
    vertex.frame.color= "transparent",
    vertex.label.color="white",
    # edge.width = 1+3*log(links$weight/min(links$weight)),
    # edge.arrow.size = 1+10*log(links$weight/min(links$weight)),
    # edge.color = factor(links$from, levels=levels(as.factor(nodes$id)))
    )
    # layout = coords)
  dev.off()
  # netw <- from_adj_matrix(t(subweb), mode="directed") # t pour que les flèches aillent au bonen endroit
  # vec_col <- c(rep("#ba9218",40), rep("arksalmon",35))
  ##
  # morunet$nodes_df$color <- vec_col
  # morunet$nodes_df$fillcolor <- vec_col
  # netw$nodes_df$style <- "filled"
  # netw$nodes_df$shape <- "triangle"
  # id <- which(netw$nodes_df$label==sp_nm)
  # netw$nodes_df$fillcolor[id] <- "transparent"
  # netw$nodes_df$label <- NA_character_
  # ##
  # #### producing the graph
  # netviz <- netw %>% render_graph
  # morunetviz
  #### Saving the network
  # saveWidget(netviz, "~/Desktop/tmp.html", selfcontained = F)
  # # and in png
  # webshot("/Users/KevCaz/Desktop/tmp.html", paste0(sp_nm, "webshot.png"), delay = 5, vwidth = 480, vheight=480)
  # knitr::kable(morunet$nodes_df) %>% cat(file="~/Dropbox/LetiR/tbmorue.md")
}
