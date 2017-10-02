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

resp.analysis<-function(mat.cit.abs.pres,mat.cit.rk){
  
  resp.list.len=apply(mat.cit.rk,2,max)
  resp.res=data.frame(resp.list.len)
  nb.resp=dim(mat.cit.abs.pres)[2]
  
  item.freq=apply(mat.cit.abs.pres,1,sum)
  item.freq=data.frame(item.freq)
  
  #Respondents' summed frequency of cited items & correlation between order of cited items and their frequency of citation
  #across lists
  for (i in 1:nb.resp){
    #List of cited items for informant i
    tax.cit=row.names(mat.cit.abs.pres)[which(mat.cit.abs.pres[,i]==1)]
    #rank at which items were cited
    tax.cit.rank=mat.cit.rk[which(mat.cit.abs.pres[,i]>0),i]
    #frequency of the cited items across lists
    tax.cit.freq=item.freq[which(row.names(item.freq)%in%tax.cit),"item.freq"]
    
    #summed frequency of cited items
    resp.freq.sum=sum(tax.cit.freq)
    resp.res[i,"resp.freq.sum"]=resp.freq.sum
    
    #Pearson correlation between order of cited items and their frequency across all lists
    resp.corr=cor(tax.cit.rank,tax.cit.freq,method="pearson")
    resp.res[i,"rk.freq.corr"]=round(resp.corr,3)
    #resp.res[i,"p.val"]=resp.corr$p.value
  }
  
  #Respondet average frequency of cited items
  resp.res$avg.freq=round(resp.res$resp.freq.sum/resp.res$resp.list.len,3)
  
  #reordering table
  resp.res=resp.res[,c("resp.list.len","resp.freq.sum","avg.freq","rk.freq.corr")]
  
  #resp.res=merge(resp.res,class.info,by=0,all=TRUE)
  #row.names(resp.res)=resp.res[,1]
  #resp.res=resp.res[,-1]
  
  return(resp.res)
}

  
