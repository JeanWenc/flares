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

FL.analysis<-function(mat.cit.abs.pres,mat.cit.rk,origListLength){
  #debug
  #mat.cit.abs.pres<-res1$mat.cit.abs.pres
  #mat.cit.rk<-res1$mat.cit.rk
  #origListLength<-res1$origListLength
  
  nb.resp=dim(mat.cit.abs.pres)[2]
  
  #Frequence de citation absolue
  FL.res=as.data.frame(apply(as.data.frame(mat.cit.abs.pres),1,sum))
  names(FL.res)="freq.cit"
  #Frequence de citation relative
  FL.res$freq.cit.rel=round(FL.res$freq.cit/nb.resp,3)
  #somme des rangs
  FL.res$sum.rank=apply(as.data.frame(mat.cit.rk),1,sum)
  #Rang moyen
  ##fonction pour faire la moyenne sans compter les 0
  nzmean=function(x){mean(x[x!=0])}
  FL.res$mean.rank=round(apply(as.data.frame(mat.cit.rk),1,nzmean),3)
  #Smith Index
  #longueur de chaque liste
  #resp.list.len=apply(mat.cit.rk,2,max)
  resp.list.len=origListLength$List_length_orig
  
  #fonction calcul smith index
  sindex=function(x){
    (resp.list.len-x+1)/resp.list.len 
  }
  #calcul smith index sans prendre en compte les  0
  FL.res$Smith.index=round(apply(apply(as.data.frame(mat.cit.rk),1,sindex)*t(as.data.frame(mat.cit.abs.pres)),2,sum)/nb.resp,4)
  #Sutrop Index
  FL.res$Sutrop.index=round(FL.res$freq.cit/(nb.resp*FL.res$mean.rank),4)
  #B Score
  max.B.mat<-matrix(apply(as.data.frame(mat.cit.rk),2,max),nrow=dim(mat.cit.rk)[1],ncol=dim(mat.cit.rk)[2],byrow=T)
  B.mat<-(max.B.mat-mat.cit.rk)/(max.B.mat-1)*mat.cit.abs.pres
  B.score<-(apply(as.data.frame(B.mat),1,sum)+FL.res$freq.cit-1)/((2*nb.resp)-1)
  
  FL.res$B.score<-round(B.score,4)
  
  FL.res$Cited_Items=rownames(FL.res)
  
  #Liste par ordre alphabÃ©tique
  FL.res=FL.res[order(FL.res$Cited_Items,decreasing = FALSE),c(8,1,2,4,5,6,7)]  

  rownames(FL.res)=seq(1:dim(FL.res)[1])
  
  return(FL.res)
}
