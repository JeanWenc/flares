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

plot.fl.analysis.chart.resp.var<-function(data,sal.index,freq,sort,item.freq,resp.var.name,text.size){
  
  #data=res.FL.resp.var
  #sal.index=select.sal.index2
  #freq=freq
  #sort=resp.var.name.mod
  #item.freq=res.FL
  #resp.var.name<-resp.var.name1
  #text.size=8
  
  min.freq<-freq[1]/100
  max.freq<-freq[2]/100
  
  items.to.show<-item.freq[which(item.freq[,"freq.cit.rel"]>min.freq),]
  items.to.show<-items.to.show[which(items.to.show[,"freq.cit.rel"]<max.freq),]
  items.to.show<-items.to.show$Cited_Items
  
  data<-data[which(row.names(data)%in%items.to.show),grep(sal.index,names(data))]
  names(data)<-sub(paste(" ",sal.index,sep=""),"",names(data))
  
  data<-data[order(data[,sort],decreasing=TRUE),]
  
  data$ord<-seq(1:dim(data)[1])
  
  labels<-row.names(data)
  
  data2<-melt(data,id="ord")
  
  ls.sal.index<-c("Frequency of mention", "Smith index","Sutrop index")
  names(ls.sal.index)<-c("Frequency","Smith","Sutrop")
  tit<-paste("Cultural saliency by ",ls.sal.index[sal.index],sep="")
  subtit<-paste("The frequency of mention of the plotted items ranges from ",freq[1]," to ",freq[2],"%.",sep="")
  
  ggplot(data=data2,aes(x=ord,y=value,colour=variable))+
    geom_line()+
    geom_point()+
    guides(color=guide_legend(title=resp.var.name))+
    scale_x_discrete(labels=labels,breaks=seq(1:length(labels)),limits=seq(1:length(labels)),name="")+
    theme(axis.text.x = element_text(family="sans",face="plain",angle = 90,hjust=1,vjust=0.5,size=text.size))+
    labs(title=tit,subtitle=subtit,y="")
}


