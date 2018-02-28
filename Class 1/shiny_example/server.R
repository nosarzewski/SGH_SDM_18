#Wczytanie bibliotek
if (!require(fmsb)) {
  install.packages('fmsb')
} else {
  library(fmsb)
}

if (!require(plotrix)) {
  install.packages('plotrix')
} else {
  library(plotrix)
}
#library(condformat)

#Wczytanie danych
data <- read.csv2("https://raw.githubusercontent.com/nosarzewski/SGH_SDM_18/master/Class%201/shiny_example/academic%20dishonesty.csv")

# Skrócone dane do analizy skupień
attach(data)
data_cluster<-cbind(Freq_4,moral_obligation,intention_cheat,social_norms,attitude,behavioural_ctrl)

# Wstępna analiza skupień metodą hierarchiczną w celu określenia liczby grup
d <- dist(data_cluster, method = "euclidean")
fit <- hclust(d, method="ward")

shinyServer(

  function(input, output) {
    
  output$plotDendrogram <- renderPlot({
    plot(fit,labels=FALSE, main="Dendogram dla klastrów", ylab="Wysokość drzewa", xlab="", sub="")
    rect.hclust(fit, k=input$clusters, border="red")
  })
  
  output$plotRadar <- renderPlot({
    clu <- kmeans(data_cluster, input$clusters)
    cluster_stats<- aggregate(data_cluster,by=list(clu$cluster),FUN=mean)
    attach(cluster_stats)
    
    # Przeskalowanie wyników
    cluster_stats_resc<-data.frame(rescale(Freq_4,c(1,2)),rescale(moral_obligation,c(1,2)),rescale(intention_cheat,c(1,2)),
                                   rescale(social_norms, c(1,2)), rescale(attitude,c(1,2)), rescale(behavioural_ctrl,c(1,2)))
    colnames(cluster_stats_resc) <- c("Frequency", "Moral obligation", "Intention", "Social norms", "Attitude", "Behavioural control")
    row_names<- c()
    for (i in 1:input$clusters){
      row_names <- c(row_names, paste("Grupa ",i))
    }
    
    # Wykres radarowy dla otrzymanych grup
    cluster_stats_resc <- rbind(rep(2.5,input$clusters) , rep(0,input$clusters) , cluster_stats_resc)
    radarchart(cluster_stats_resc, cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,2.5,6), cglwd=0.8,
               pcol=rainbow(input$clusters, s=0.5), pfcol=rainbow(input$clusters,s=0.2, alpha=0.2), plwd=2 , plty=1, 
               vlcex=0.8, title="Wykres radarowy dla grup")
    legend("bottomright", legend = row_names, bty = "n", pch=20 , col=rainbow(input$clusters, s=0.5), pt.cex=2, cex=0.8)
  })
  
  }
)