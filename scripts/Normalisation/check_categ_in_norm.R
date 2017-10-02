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

check.categ.in.norm<-function(ls.categ,norm.var,norm.item.data){
#debug
  #ls.categ<-norm.category.names2
  #norm.var<-norm.type2
  
  start=FALSE
  categ.pb="empty"
  
  for(i in 1:length(ls.categ)){
    ls.categ.temp=ls.categ[i]
    
    if(i==1 | start==TRUE){
      pb.tab=as.data.frame.matrix(table(norm.item.data[,norm.var],norm.item.data[,ls.categ.temp]))
      names(pb.tab)=paste(ls.categ[i],names(pb.tab),sep="_")
      
      n.mod=apply(pb.tab,1,function(x) length(which(x>0)))
      pb=which(n.mod>1)
      
      pb.tab[norm.var]=rownames(pb.tab)
      rownames(pb.tab)=seq(1:dim(pb.tab)[1])
      pb.tab=pb.tab[,c(dim(pb.tab)[2],seq(1:(dim(pb.tab)[2]-1)))]
      
      if(length(pb)!=0) categ.pb=ls.categ.temp
      if(length(pb)==0) start=TRUE 
    }
    
    if(i!=1 & start!=TRUE){
      temp=as.data.frame.matrix(table(norm.item.data[,norm.var],norm.item.data[,ls.categ.temp]))
      names(temp)=paste(ls.categ[i],names(temp),sep="_")
      
      n.mod=apply(temp,1,function(x) length(which(x>0)))
      pb=which(n.mod>1)
      
      temp[norm.var]=rownames(temp)
      rownames(temp)=seq(1:dim(temp)[1])
      temp=temp[,c(dim(temp)[2],seq(1:(dim(temp)[2]-1)))]
      
      if(length(pb)!=0){
        categ.pb=c(categ.pb,ls.categ.temp)
        pb.tab=merge(pb.tab,temp,by=norm.var,all=T)
      } 
    }
  }
  
  if(categ.pb[1]!="empty"){
    pb.tab$n.mod=apply(pb.tab,1,function(x) length(which(x[-1]>0))) 
    pb.tab=pb.tab[which(pb.tab$n.mod>length(categ.pb)),]
    pb.tab=pb.tab[,-dim(pb.tab)[2]]  
  }else{
    pb.tab=data.frame(OK="")
  }
  
  res=list(pb.tab,categ.pb)
  names(res)=c("item.cat.pb.tab","ls.pb.categ")
  return(res)  
}