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

anth_form<-function(up.data,apac){
  
  if(apac=="APAC"){
    resp.id=grep("#",up.data[,1])
    if(length(resp.id)!=0) up.data[,1]=(sub("#","",up.data[,1]))
    
    if(length(resp.id)==0) {
      resp.id=grep(".X",up.data[,1])
      up.data[,1]=(sub(".X","",up.data[,1]))
    } 
    names(up.data)[1]="Cited_Items"
  }else{
    up.data[,c(1,2)]=up.data[,c(2,1)]
    names(up.data)[1:2]=c("Cited_Items","ID_RESP")
    list.resp=unique(up.data[,"ID_RESP"])
    resp.id=numeric()
    for(i in 1:length(list.resp)){
      resp.id[i]=min(which(up.data[,"ID_RESP"]==list.resp[i]))
    }
  }
  
  list.len=numeric()
  for(i in 1:(length(resp.id)-1)){
    i.len=(resp.id[i+1]-resp.id[i])
    list.seq=seq(from=resp.id[i],to=(resp.id[i+1]-1),by=1)
    
    if(apac=="APAC"){
      i.len=i.len-1
      up.data[list.seq,"ID_RESP"]=as.character(up.data[resp.id[i],1])
      up.data[list.seq[-1],"Order"]=seq(1:i.len)
    }else{
      up.data[list.seq,"Order"]=seq(1:i.len)
    }
    list.len[i]=i.len
  }
  
  i.len=dim(up.data)[1]-resp.id[length(resp.id)]+1
  list.seq=seq(from=resp.id[length(resp.id)],to=(dim(up.data)[1]),by=1)
  if(apac=="APAC"){
    i.len=i.len-1
    up.data[list.seq,"ID_RESP"]=as.character(up.data[resp.id[length(resp.id)],1])
    up.data[list.seq[-1],"Order"]=seq(1:i.len)
  }else{
    up.data[list.seq,"Order"]=seq(1:i.len)
  }

  list.len=c(list.len,i.len)
  
  max=max(list.len)+1
  
  data=data.frame(matrix(nrow = max,ncol=length(resp.id),dimnames=list(seq(1:max),up.data[resp.id,"ID_RESP"])))
  
  for(i in 1:(length(resp.id)-1)){
    list.i=as.character(up.data[resp.id[i]:(resp.id[i+1]-1),"Cited_Items"])
    if(apac=="ATOOLS") list.i=c(as.character(up.data[resp.id[i],"ID_RESP"]),list.i)
    data[1:length(list.i),i]=list.i
  }
  
  list.i=as.character(up.data[resp.id[length(resp.id)]:dim(up.data)[1],"Cited_Items"])
  if(apac=="ATOOLS") list.i=c(as.character(up.data[resp.id[length(resp.id)],"ID_RESP"]),list.i)
  data[1:length(list.i),length(resp.id)]=list.i
  
  colnames(data)=data[1,]
  data=data[-1,]
  
  if(apac=="APAC") up.data=up.data[-resp.id,]
  up.data$tot_order=seq(1:dim(up.data)[1])
  
  res=list(data,up.data)
  names(res)=c("FLAME_format","list_format")
  
  return(res)
}
