############################################################################  
## FUNCTIONS FOR PLOTS
############################################################################

plot_tree <- function(ds, plotColors) {
  counts <- assay(ds) # as.dist(1 - cor(.  , method = "pearson"))%>% hclust() %>% as.dendrogram()
  distance = as.dist(1 - cor(counts, method = "pearson"))
  hc <- hclust(distance)
  otter.dendro <- as.dendrogram(hc)
  # Create dendro
  dendro.plot <- ggdendro::ggdendrogram(data = otter.dendro, theme_dendro = FALSE) +
    labs(x = "", y = "Distance") +
    ggtitle("Sample clustering by pearson method") 
    theme <- theme(panel.border = element_rect(colour = "black", fill = NA, size = .5), 
          axis.text.x = element_text(colour = plotColors[[2]], size = 10, face = "bold"), 
          axis.text.y = element_text(colour = plotColors[[2]], size = 10, face = "bold"),
          axis.title.y = element_text(colour = plotColors[[2]], face = "bold"),
          legend.key = element_rect(fill = plotColors[[2]], colour = "white"),
          legend.position = "bottom", legend.direction = "horizontal", 
          panel.grid.major = element_line(colour = "#d3d3d3"), 
          plot.title = element_text(size = 14, family = "Tahoma", face = "bold", colour = plotColors[[2]]), 
          plot.background = element_rect(fill = plotColors[[1]], color = plotColors[[1]]),
          text = element_text(family = "Tahoma"))
    dendro.plot <- dendro.plot + theme
    print(dendro.plot)
}

plot_heatmap <- function(mat, plotColors) {
  #p<-aheatmap( mat, Colv = TRUE, hclustfun = "average", scale = "row", fontsize=20 )
  p <- pheatmap(mat, Colv = TRUE, hclustfun = "average", scale = "row")
  grid.draw(rectGrob(gp = gpar(fill = plotColors[[1]], lwd = 0)))
  grid.draw(p)
  grid.gedit("layout", gp = gpar(col = plotColors[[2]], text = ""))
}


plot_barplot <- function(dtf, plotColors) {
  ##built barplot
  p <- ggplot(data = dtf, aes(x = Run, y = counts, fill = gene)) +
    geom_bar(stat = "identity") + facet_wrap(~gene, scales = "free_y")
  theme <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
    theme(legend.position = "none") +
    theme(
      #panel.background = element_rect(fill = "black"), # bg of the panel
      plot.background = element_rect(fill = plotColors[[1]], color = plotColors[[1]]),
      axis.text.x = element_text(colour = plotColors[[2]], size = 10, face ="bold"), 
      axis.text.y = element_text(colour = plotColors[[2]], size = 10, face = "bold"),
      axis.title.y = element_text(colour = plotColors[[2]], face = "bold")
      #panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "white"),
      #panel.grid.minor = element_blank(), # get rid of minor grid
      #legend.background = element_rect(fill = "white"), # get rid of legend bg
      #legend.box.background = element_rect(fill = "white") # get rid of legend panel bg
    ) +
    theme(
      strip.text.x = element_text(margin = margin(2, 0, 2, 0))
    )
    p <- p + theme
    print(p)
}

plot_timeSeries <- function(dtf, list_rank, plotColors) {
  p <- ggplot(dtf, aes(x = factor(dev_stage, level = list_rank), y = counts, col = Run)) +
    geom_point()+ 
    stat_summary(fun = "median", geom="line", aes(group = 1), col = "black")+
    facet_wrap(~gene) + 
    labs(x = "Development stage")
  theme <- theme(axis.text.x = element_text(angle = 90, vjust =0.5, hjust = 1))+ 
    theme(plot.background = element_rect(fill = plotColors[[1]], color = plotColors[[1]]),
    axis.text.x = element_text(colour = plotColors[[2]], size = 10, face = "bold"), 
    axis.text.y = element_text(colour = plotColors[[2]], size = 10, face = "bold"),
    axis.title.y = element_text(colour = plotColors[[2]], face = "bold"),
    axis.title.x = element_blank())
    #axis.title.x = element_text(colour = plotColors[[2]], face = "bold"))
  p <- p + theme
  print(p)
}

plot_acp <- function(ds, condition, plotColors) {
  dta <- vst(ds, blind = FALSE) 
  P <- plotPCA(dta, intgroup = condition) + geom_point(aes(text = paste("Run:", colnames(dta))))
  theme <- theme(plot.background = element_rect(fill = plotColors[[1]], color = plotColors[[1]]),
                 axis.text.x = element_text(colour = plotColors[[2]], size = 10, face = "bold"), 
                 axis.text.y = element_text(colour = plotColors[[2]], size = 10, face = "bold"),
                 axis.title.y = element_text(colour = plotColors[[2]], face ="bold"),
                 axis.title.x = element_text(colour = plotColors[[2]], face ="bold"))
  P <- P + theme 
  ggplotly(P, tooltip = c("text"))
  #grid.draw(rectGrob(gp=gpar(fill="black", lwd=0)))
  #grid.draw(p)
  #grid.gedit("layout", gp = gpar(col = "white", text = ""))
}