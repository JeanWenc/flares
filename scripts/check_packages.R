check.packages<-function(){
  required.packages<-c("ade4","adegraphics","ca","dendextend","data.table","DT","leaflet","ggdendro","ggplot2","RColorBrewer","readr","reshape2","shiny","vegan")
  
  for(i in 1:length(required.packages)){
    if(!required.packages[i]%in%installed.packages()[,1]){
      install.packages(required.packages[i])
    }
  }
}