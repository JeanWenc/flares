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

plot.res.comp<-function(res.resp,resp.var.name){
  
  #debug
  #resp.var.name<-resp.var.name1
  
  if(resp.var.name!="No Variable"){
    data<-res.resp[,c("resp.freq.sum","resp.list.len",resp.var.name)]
    names(data)[3]<-"Variable"
    resp.var.name2<-resp.var.name
  }else{
    data<-res.resp[,c("resp.freq.sum","resp.list.len")]
    data$Variable<-"Respondents"
    resp.var.name2<-""
  }
  
  ls.na<-which(is.na(data$Variable))
  if(length(ls.na)!=0) data<-data[-ls.na,]
  
  tit<-"Respondent competence"
  if(length(ls.na)==0){
    subt<-paste(dim(data)[1]," respondents", sep="")
  }else{
    subt<-paste(dim(data)[1]," respondents.", " The ",resp.var.name, " variable is not documented for ",length(ls.na)," respondent(s).",sep="")
  }
  
  
  ggplot(data,aes(x=resp.freq.sum,y=resp.list.len,colour=Variable))+
    geom_point()+
    labs(title=tit,subtitle=subt,x="Summed frequency",y="List length")+
    guides(color=guide_legend(title=resp.var.name2))
  
}

      
 
  

