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

FL.analysis.resp.var<-function(resp.var.data,mat.cit.abs.pres,mat.cit.rk,resp.var.name){
  #debug
    #mat.cit.abs.pres<-res1$mat.cit.abs.pres
    #mat.cit.rk<-res1$mat.cit.rk
    #resp.var.name<-resp.var.name1
  
  for(i in 2:dim(resp.var.data)[2]){
    resp.var.data[,i]<-factor(resp.var.data[,i])
  }
  
  resp.var.mod=levels(resp.var.data[,resp.var.name])
  res.tot.temp=as.data.frame(row.names(mat.cit.abs.pres))
  
  for(i in 1:length(resp.var.mod)){
    ls.resp.temp=as.character(resp.var.data[which(resp.var.data[,resp.var.name]==resp.var.mod[i]),1])
    nb.resp.temp=length(ls.resp.temp)
    if(nb.resp.temp>0){
      mat.cit.abs.pres.temp=as.data.frame(mat.cit.abs.pres[,which(colnames(mat.cit.abs.pres)%in%ls.resp.temp)])
      mat.cit.rk.temp=as.data.frame(mat.cit.rk[,which(colnames(mat.cit.rk)%in%ls.resp.temp)]  )
      if(nb.resp.temp==1){
        dimnames(mat.cit.abs.pres.temp)<-list(rownames(mat.cit.abs.pres),ls.resp.temp)
        dimnames(mat.cit.rk.temp)<-list(rownames(mat.cit.rk),ls.resp.temp)
        }
      
      
      #Frequence de citation absolue
      FL.res.temp=as.data.frame(apply(as.data.frame(mat.cit.abs.pres.temp),1,sum))
      names(FL.res.temp)="freq.cit"
      #Frequence de citation relative
      FL.res.temp$freq.cit.rel=round(FL.res.temp$freq.cit/nb.resp.temp,2)
      #somme des rangs
      FL.res.temp$sum.rank=apply(as.data.frame(mat.cit.rk.temp),1,sum)
      #Rang moyen
      ##fonction pour faire la moyenne sans compter les 0
      nzmean=function(x){mean(x[x!=0])}
      FL.res.temp$mean.rank=round(apply(as.data.frame(mat.cit.rk.temp),1,nzmean),3)
      #Smith Index
      #longueur de chaque liste
      resp.list.len.temp=apply(as.data.frame(mat.cit.rk.temp),2,max)
      #fonction calcul smith index
      sindex=function(x){
        (resp.list.len.temp-x+1)/resp.list.len.temp 
      }
      #calcul smith index sans prendre en compte les  0
      FL.res.temp$Smith.index=round(apply(apply(as.data.frame(mat.cit.rk.temp),1,sindex)*t(as.data.frame(mat.cit.abs.pres.temp)),2,sum)/nb.resp.temp,3)
      #Sutrop Index
      FL.res.temp$Sutrop.index=round(FL.res.temp$freq.cit/(nb.resp.temp*FL.res.temp$mean.rank),3)
      
      #Replacing NAs by 0
      FL.res.temp[which(is.na(FL.res.temp[,"Sutrop.index"])),c("mean.rank","Sutrop.index")]=0
      
      #Limiting number of columns
      FL.res.temp=FL.res.temp[,c("freq.cit.rel","Smith.index","Sutrop.index")]
      names(FL.res.temp)=c("Frequency","Smith","Sutrop")
      
      #renaming columns
      names(FL.res.temp)=paste(resp.var.mod[i],names(FL.res.temp),sep=" ")
      
      res.tot.temp=merge(res.tot.temp,FL.res.temp,by.x=1,by.y=0,all=TRUE)  
    }
    
    
  }
  
  row.names(res.tot.temp)=res.tot.temp[,1]
  res.tot.temp=res.tot.temp[,-1]
  
  return(res.tot.temp)
}