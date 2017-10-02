#######################################################################################################################
###################FLARES - Free-List Analysis under R Environment using Shiny########################################
#######################################################################################################################
#Copyright (C) 2017 Jean Wencélius

#License notice:
#This file is part of FLARES.

#FLARES is a free software: you can redistribute it and/or modify
#it under the terms of the GNU Affero General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU Affero General Public License for more details.

#You should have received a copy of the GNU Affero General Public License
#along with FLARES.  If not, see <http://www.gnu.org/licenses/>.

########################################################################################################################

plot.fl.analysis.chart<-function(data,sal.index,sort.order,min.freq,max.freq,text.size){
  #debug
  #data=res.FL
  #sal.index=salience.ind.checkbox2
  #sort.order=sort2
  #min.freq=10
  #max.freq=100
  
  min.freq1=min.freq/100
  max.freq1=max.freq/100

  data=data[which(data[,"freq.cit.rel"]>=min.freq1),]
  data=data[which(data[,"freq.cit.rel"]<=max.freq1),]
  
  dec.or.inc<-ifelse(as.numeric(sort.order)==1,FALSE,TRUE)
  data=data[order(data[,as.numeric(sort.order)],decreasing=dec.or.inc),]
  data$ord<-seq(1:dim(data)[1])
  labels<-data$Cited_Items
  
  data=data[,which(names(data)%in%c(sal.index,"ord"))]
  
  data2<-melt(data,id="ord")
  
  sal.name<-c("Frequency of mention","Smith index","Sutrop index")
  names(sal.name)<-c("freq.cit.rel","Smith.index","Sutrop.index")
  levels(data2$variable)<-as.character(sal.name[levels(data2$variable)])
  
  ggplot(data=data2,aes(x=ord,y=value,colour=variable))+
    geom_line()+
    geom_point()+
    guides(color=guide_legend(title="Saliency index"))+
    scale_x_discrete(labels=labels,breaks=seq(1:length(labels)),limits=seq(1:length(labels)),name="")+
    scale_y_continuous(name="")+
    theme(axis.text.x = element_text(family="sans",face="plain",angle = 90,hjust=1,vjust=0.5,size=text.size))+
    labs(title="Cultural saliency of cited items",subtitle=paste("The frequency of mention of the plotted items ranges from ",min.freq," to ",max.freq,"%.",sep=""))
}


