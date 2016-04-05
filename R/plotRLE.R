plot.function<-function(fishnet.data, ecosystem.data, EOO.spatialpolygon){
  plot(fishnet.data, col = grey.colors(12), legend =F)
  plot(EOO.spatialpolygon, add = TRUE,  lwd = 2, col = "grey")
  plot(ecosystem.data, col = "darkslateblue", add = T)
}