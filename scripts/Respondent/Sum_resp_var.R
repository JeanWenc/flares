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

sum.resp.var<-function(data){
  
  res=data.frame(list("Var","mod",0))
  names(res)=c("Variable","Modality","Count")
  res[,1]=as.character(res[,1])
  res[,2]=as.character(res[,2])
  
  l=2
  for(i in 2:dim(data)[2]){
    
    var.mod=as.character(unique(data[,i]))
    count.j=0
    for(j in 1:length(var.mod)){
      if(is.na(var.mod[j])){
        count.j=c(count.j,length(which(is.na(data[,i]))))
      }
      if(!is.na(var.mod[j])){
        count.j=c(count.j,length(which(data[,i]==var.mod[j])))  
      }
    }
    count.j=count.j[-1]
    var=rep(names(data)[i],length(var.mod))
    res[l:(l+length(var.mod)-1),]=list(var,var.mod,count.j)
    l=l+length(var.mod)
  }
  res=res[-1,]
  
  return(res)
  
}