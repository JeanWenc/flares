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

item.prox.mat<-function(mat.cit.abs.pres,mat.cit.rk,res.FL,debug){
  
  #debug
  #mat.cit.abs.pres<-res1$mat.cit.abs.pres
  #mat.cit.rk<-res1$mat.cit.rk
  #debug=TRUE
  
  mat.dist.item.hen=matrix(0,nrow=dim(mat.cit.rk)[1],ncol=dim(mat.cit.rk)[1],dimnames=list(row.names(mat.cit.rk),row.names(mat.cit.rk)))
  mat.dist.item.succ=matrix(0,nrow=dim(mat.cit.rk)[1],ncol=dim(mat.cit.rk)[1],dimnames=list(row.names(mat.cit.rk),row.names(mat.cit.rk)))
  
  res.FL=res.FL[order(res.FL[,1]),]
  
  diag(mat.dist.item.succ)=res.FL$freq.cit
  
  nb.resp=dim(mat.cit.abs.pres)[2]
  #Can eventually use res1$origListLength
  resp.list.len=as.data.frame(apply(mat.cit.rk,2,max))
  
  max.i=dim(mat.cit.rk)[1]-1
  max.prog=max.i*(max.i+1)/2
  curr.i=0
  
  for(i in 1:(dim(mat.cit.rk)[1]-1)){
    x=i+1
    for(i2 in x:dim(mat.cit.rk)[1]){
      hen.temp=0
      nb.pair=0
      succ.count=0
      ####
      ls.col.hen=names(mat.cit.rk)[which(mat.cit.rk[i,]!=0)]
      ls.col.hen=ls.col.hen[which(mat.cit.rk[i2,ls.col.hen]!=0)]
      
      if(length(ls.col.hen)!=0){
        substraction=abs(apply(as.data.frame(mat.cit.rk[c(i,i2),ls.col.hen]),2,diff))
        resp.list.len.row=which(row.names(resp.list.len)%in%ls.col.hen)
        hen.temp=sum(substraction*100/resp.list.len[resp.list.len.row,1])
        nb.pair=length(ls.col.hen)
        
        ls.col.succ=ls.col.hen[which(substraction==1)]
        succ.count=length(ls.col.succ)
      }
  
      ####
      #hen.temp=0
      #nb.pair=0
      #succ.count=0
      #for(j in 1:nb.resp){
        #if(mat.cit.rk[i,j]!=0 & mat.cit.rk[i2,j]!=0){
          #hen.temp=hen.temp+(abs(mat.cit.rk[i,j]-mat.cit.rk[i2,j])*100/resp.list.len[j,1])
          #nb.pair=nb.pair+1
          #if(abs(mat.cit.rk[i,j]-mat.cit.rk[i2,j])==1) succ.count=succ.count+1
        #}
      #}
      
      if(nb.pair==0) hen.val=100 else hen.val=hen.temp/nb.pair
      mat.dist.item.hen[i,i2]=hen.val
      mat.dist.item.hen[i2,i]=hen.val
      mat.dist.item.succ[i,i2]=succ.count
      mat.dist.item.succ[i2,i]=succ.count
    }
    curr.i=curr.i+(max.i+1-i)
   
    if(debug==FALSE) incProgress(((max.i+1)-i)/max.prog,detail=paste(round(curr.i/max.prog*100),"%",sep=" "))
  }
  
  res=list(mat.dist.item.hen,mat.dist.item.succ)
  names(res)=c("Henley index","Successive count")
  return(res)
  
}

