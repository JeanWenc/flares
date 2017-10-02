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

plot.data.saturation<-function(data){
  #debug
  #data=data.saturation
  
  nb.item=as.numeric(max(as.numeric(data[,2])))
  nb.resp=dim(data)[1]-1
  
  logEstimate=lm(as.numeric(data$new.cit.items[-1])~log(seq(1:(nb.resp))))
  data[,"log-fit"]<-c(0,as.numeric(predict(logEstimate)))
  sum.log<-round(summary(logEstimate)$adj.r.squared,3)
  
  data$ord<-seq(1:dim(data)[1])
  
  data<-data[,-1]
  
  data2<-reshape2::melt(data,id="ord")
  data2$value<-round(as.numeric(data2$value),2)
  levels(data2$variable)<-c("Newly cited items",paste("log-fit"," (R2 = ",sum.log,")",sep=""))
  
  ggplot(data2,aes(x=ord,y=value,colour=variable))+
    geom_line()+
    labs(title="Cumulative sum of newly cited items across respondents",subtitle=paste(nb.resp," respondents have cited all of the ",nb.item," items.",sep=""),x="Nb. of respondents",y="Nb. of new items")+
    guides(color=guide_legend(title=""))
}



