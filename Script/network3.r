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









subweb2 <- plotMean[subnet,subnet]
# # Eliminer associations < 0.4
# for (i in 1:nrow(subweb)) {
#   for (j in 1:ncol(subweb)) {
#     if (subweb2[i,j] > -.4 & subweb2[i,j] < .4) subweb2[i,j] = 0
#   }
# }
subweb2 <- subweb2[which(subweb > 0)] # sélectionner uniquement les liens relevés pour la figure 1

# vec_colfont <- rep("white",nsp)
# vec_colfont[-id] <- "black"
# sp_nm <- "cracreflé"
netw2 <-  graph_from_adjacency_matrix(t(subweb2))

arrowWidth <- (abs(subweb2)/min(abs(subweb2)))/50

# Colour <- colorRampPalette(c("blue", "white", "red"))(200)
rbPal <- colorRampPalette(c('red','white', 'blue')) # color palette
cols <- rbPal(50)[as.numeric(cut(subweb2, breaks = 50))]

##

# png(paste0("./fig/cracreflé2.png"), width=7, height=9, units="in", res=300)
  layout(matrix(c(1,2), 2, 1), height=c(.8,.2))
  par(bg="transparent", mar=rep(0.4,4), fg="black", family = 'regText')
  plot(netw,
    vertex.color = vec_col,
    vertex.frame.color = "transparent",
    vertex.label.color = "black",
    vertex.label = vec_names,
    vertex.size = vec_size,
    # edge.width = 1+3*log(links$weight/min(links$weight)),
    edge.arrow.size = 0,
    edge.width = arrowWidth,
    edge.color = cols
    # edge.color = factor(links$from, levels=levels(as.factor(nodes$id)))
    )
  plot0()
  legend("center", bty="n", legend=sort(unique(grptax)), cex=1.2, pch=21, ncol=2,
  pt.bg=pal, col=pal, pt.cex=2.4)
    # layout = coords)
# dev.off()

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
