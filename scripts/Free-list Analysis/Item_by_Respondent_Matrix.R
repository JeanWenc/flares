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

item.resp.mat<-function(up.data.orig,norm.item.data,go.norm,norm.type.col,apac){
  #DEBUG
  #up.data.orig=up.data
  #norm.type.col=norm.type
  
  if(apac=="FLAME"){
    up.data=up.data.orig$FLAME_format
    #Number of lists
    nb.resp=dim(up.data)[2]
    #list of respondents
    ls.resp=names(up.data)  
  
    
    #the following line creates a data.frame with one line= one item cited by one informant.
    #Columns are: ID_RESP(id code of repondent), Cited_Items (the cited item), FL.ord (the order of the cited item for informant i)
    for(i in 1:dim(up.data)[2]){
      if(i==1){
        FL=as.character(up.data[,i])
        FL.na=which(is.na(up.data[,i]))
        
        if(length(FL.na)!=0) FL=FL[-FL.na]
        
        FL.ord=seq(1:length(FL))
        FL.resp=rep(names(up.data)[i],length(FL))
      }
      if(i!=1){
        FL.temp=as.character(up.data[,i])
        FL.na=which(is.na(up.data[,i]))
        
        if(length(FL.na)!=0) FL.temp=FL.temp[-FL.na]
        
        FL=c(FL,FL.temp)
        FL.ord=c(FL.ord,seq(1:length(FL.temp)))
        FL.resp=c(FL.resp,rep(names(up.data)[i],length(FL.temp)))
        
      }
    }
    FL.list=data.frame(ID_RESP=FL.resp,Cited_Items=FL,Order=FL.ord)
    #creating a column with order across informants
    FL.list$tot_order=seq(1:dim(FL.list)[1])
  }else{#if uploaded data was in anthropac or atools format, the list format already exists (and maybe categorical data too)
    FL.list=up.data.orig$list_format
    ls.resp=unique(FL.list$ID_RESP)
    nb.resp=length(ls.resp)
  }
  
  #keeping track of the original list length of each respondents (before removing duplicates)
  origListLength<-as.data.frame(table(FL.list[,"ID_RESP"]))
  names(origListLength)<-c("ID_RESP","List_length_orig")
  
  #if norm data was uploaded then merge the normalisation information to the created data.frame
  if(names(norm.item.data)[1]!="empty") {
    FL.list=merge(FL.list,norm.item.data,by.x="Cited_Items",by.y=1,all.x=TRUE)
    #reoder data.frame in original order
    FL.list=FL.list[order(FL.list$tot_order),]
  }
  
  #if norm data was uploaded indicate which column user wishes to use for normalisation
  sel.col<-ifelse(go.norm==TRUE,norm.type.col,"Cited_Items")
  
  #looking for duplicate items
  duplicates=as.data.frame(table(FL.list$ID_RESP,FL.list[,sel.col]))
  duplicates=duplicates[which(duplicates$Freq>1),]
  
  warn.doub=0
  list.resp.doub=""
  #if duplicates were found create list of informants for which duplicates were found, and remove second (and third etc..) occurences
  if(dim(duplicates)[1]>0){
    warn.doub=dim(duplicates)[1]
    list.resp.doub=as.character(unique(duplicates$Var1))
    
    for(i in 1:length(list.resp.doub)){
      ls.dup=as.character(duplicates[duplicates$Var1%in%list.resp.doub[i],"Var2"])
      for(j in 1:length(ls.dup)){
        dup.lines=FL.list[which(FL.list$ID_RESP==list.resp.doub[i] & FL.list[,sel.col]==ls.dup[j]),"tot_order"]
        if(length(dup.lines)>1) FL.list=FL.list[-which(FL.list$tot_order%in%dup.lines[-1]),]
      }
    }
  }
  
  #total number of different cited items
  nb.item=length(unique(FL.list[,sel.col]))
  
  #creating two item/respondent matrices: one with rank of citation of items and one with absence/presence of items
  #replacing NA's by 0
  mat.cit.rk=acast(FL.list,as.formula(paste(sel.col,"~","ID_RESP",sep="")),value.var="Order")
  mat.cit.rk[is.na(mat.cit.rk)]<-0
  mat.cit.abs.pres=mat.cit.rk
  mat.cit.abs.pres[mat.cit.abs.pres>1]<-1
  
  mat.cit.rk=as.data.frame(mat.cit.rk)
  mat.cit.abs.pres=as.data.frame(mat.cit.abs.pres)
  
  
  res=list(mat.cit.rk,mat.cit.abs.pres,nb.resp,warn.doub,list.resp.doub,nb.item,FL.list,origListLength)
  names(res)=c("mat.cit.rk","mat.cit.abs.pres","nb.resp","warn.doub","list.resp.doub","nb.item","FL.list","origListLength")
  return(res)
}