########----- Network Gadus morhua ------########
####
library(magrittr)
library(webshot)
library(htmlwidgets)
library(DiagrammeR)
####
get_connec <- function(x) sum(x)/prod(dim(x))

####
metaweb <- readRDS("./RData/metaWeb.rds")
connec <- get_connec(metaweb)
id <- which(colnames(metaweb) == "Gadus morhua")
## row = preys ?? 2Bchecked
idpro <- which(metaweb[id, ] > 0)
## col = predators ?? 2Bchecked
idpre <- which(metaweb[, id] > 0)
##
subnet <- which(1:ncol(metaweb)%in%c(id, idpro, idpre))
subweb <- metaweb[subnet,subnet]
id <- which(colnames(subweb) == "Gadus morhua")
for (i in 1:nrow(subweb)){
  for (j in 1:ncol(subweb)){
    if (i!=id & j!=id) subweb[i,j] = 0
  }
}
#### Designing thed network using
morunet <- from_adj_matrix(subweb, mode="directed")
vec_col <- c(rep("#ba9218",40), rep("arksalmon",35))
##
morunet$nodes_df$color <- vec_col
morunet$nodes_df$fillcolor <- vec_col
morunet$nodes_df$style <- "filled"
morunet$nodes_df$shape <- "triangle"
id <- which(morunet$nodes_df$label=="Gadus morhua")
morunet$nodes_df$fillcolor[id] <- "transparent"
morunet$nodes_df$label <- NA_character_
##
#### producing the graph
morunetviz <- morunet %>% render_graph
morunetviz
#### Saving the network
saveWidget(morunetviz, "~/Desktop/tmp.html", selfcontained = F)
# and in png
webshot("/Users/KevCaz/Desktop/tmp.html", "webshot.png", delay =5, vwidth = 480, vheight=480)
# knitr::kable(morunet$nodes_df) %>% cat(file="~/Dropbox/LetiR/tbmorue.md")
