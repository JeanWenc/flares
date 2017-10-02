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

check.apac.categories<-function(FL.list,norm.item.data,norm.type){
  apac.categ.temp=names(FL.list)
  if(names(norm.item.data)[1]%in%c("empty","WARNING")) norm.and.cat.col=""
  if(!names(norm.item.data)[1]%in%c("empty","WARNING")) norm.and.cat.col=names(norm.item.data[-1])
  
  apac.categ=apac.categ.temp[!apac.categ.temp%in%c(norm.and.cat.col,"Cited_Items","ID_RESP","Order","tot_order")]
  
  if(length(apac.categ)==0) 
    return(NULL)
  
  norm.col=ifelse(norm.type=="None",1,which(names(FL.list)==norm.type))
  
  for(i in 1:length(apac.categ)){
    apac.categ.temp=apac.categ[i]
    if(i==1){
      res.tab=as.data.frame.matrix(table(FL.list[,norm.col],FL.list[,apac.categ.temp]))
      res.tab=res.tab[,which(apply(res.tab,2,sum)!=0)]
      
      new.func<-function(x){
        ls.x.col=names(x)[which(x==max(x))]
        x.categ=ifelse(length(ls.x.col)==length(x),"all",paste(ls.x.col,collapse="_"))
        return(x.categ)
      }
      
      new.tab=data.frame(Items=rownames(res.tab),max=apply(res.tab,1,new.func))
      row.names(new.tab)=seq(1:dim(new.tab)[1])
      names(new.tab)[2]=apac.categ.temp
      
      names(res.tab)=paste(apac.categ.temp,names(res.tab),sep="_")
    }
    
    if(i!=1){
      res.tab.temp=as.data.frame.matrix(table(FL.list[,norm.col],FL.list[,apac.categ.temp]))
      res.tab.temp=res.tab.temp[,which(apply(res.tab.temp,2,sum)!=0)]
      
      new.tab.temp=data.frame(Items=rownames(res.tab),apac.categ.temp=apply(res.tab.temp,1,new.func))
      row.names(new.tab.temp)=seq(1:dim(new.tab.temp)[1])
      names(new.tab.temp)[2]=apac.categ.temp
      
      new.tab=merge(new.tab,new.tab.temp,by="Items",all=T)
      
      names(res.tab.temp)=paste(apac.categ.temp,names(res.tab.temp),sep="_")
      
      res.tab=merge(res.tab,res.tab.temp,by=0,all=T)
      
    }
  }
  if(i>1) names(res.tab)[1]="Items"
  
  res=list(apac.categ,res.tab,new.tab)
  names(res)=c("apac.categ.names","item.distrib","item.max.cat")
  return(res)
}







