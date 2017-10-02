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

resp.prox.fn<-function(mat.cit.abs.pres,res.FL,freq,resp.var.data){
  
  #debug
    #mat.cit.abs.pres<-res1$mat.cit.abs.pres
    #freq<-c(0,100)
  
  freq.min=freq[1]/100
  freq.max=freq[2]/100
  
  ls.items.freq=res.FL[which(res.FL$freq.cit.rel>freq.min),]
  ls.items.freq=ls.items.freq[which(ls.items.freq$freq.cit.rel<freq.max),]
  ls.items.freq=ls.items.freq$Cited_Items
  
  mat.cit.abs.pres=mat.cit.abs.pres[which(row.names(mat.cit.abs.pres)%in%ls.items.freq),]
  
  mat.dist.resp=dist(t(mat.cit.abs.pres),method="binary")
  
  res.dispersion=as.data.frame("")
  res.adonis=as.data.frame("")
  sum.res=as.data.frame("")
  
  if(resp.var.data[1,1]!=""){
    resp.var.data[,1]<-as.character(resp.var.data[,1])
    for(i in 2:dim(resp.var.data)[2]){
      resp.var.data[,i]=factor(resp.var.data[,i])
    }
    
    res.dispersion<-matrix(data = 0,
                          nrow = (dim(resp.var.data)[2]-1),
                          ncol=6,
                          dimnames = list(names(resp.var.data[2:length(names(resp.var.data))]),c("Nb. NA's","DF","Sum sq.", "Mean sq.","F value","p-value")))
    
    sum.res<-matrix(data=0,
                   nrow=(dim(resp.var.data)[2]-1),
                   ncol=1,
                   dimnames=list(names(resp.var.data[2:length(names(resp.var.data))]),"Text")
                   )
    sum.res<-as.data.frame(sum.res)
    
    text1<-paste("For the selected variable intra-group dispersion <b>IS NOT</b> homogeneous across groups.",
                 "<b>Multivariate Analysis of Variance cannot be performed</b><br/>.",
                 "<i>Please refer to the 'Betwenn Class Analysis' sub-tab for detailed results.</i>",
                 sep="<br/>")
    text2<-paste("For the selected variable intra-group dispersion <b>IS</b> homogeneous across groups.",
                 "Multivariate Analysis of Variance indicates that variation between groups <b>IS NOT significantly</b> higher than variation within groups.<br/>",
                 "<i>Please refer to the 'Betwenn Class Analysis' sub-tab for detailed results.</i>",
                 sep="<br/>")
    text3<-paste("For the selected variable intra-group dispersion <b>IS</b> homogeneous across groups.",
                 "Multivariate Analysis of Variance indicates that variation between groups <b>IS significantly</b> higher than variation within groups.<br/>",
                 "<i>Please refer to the 'Betwenn Class Analysis' sub-tab for detailed results.</i>",
                 sep="<br/>")
    
    dist.temp<-mat.dist.resp
    resp.var.data.temp<-resp.var.data
    
    for(i in 2:dim(resp.var.data)[2]){

      name.NA=as.character(resp.var.data.temp[which(is.na(resp.var.data.temp[,i])),1])
      n.NA=length(name.NA)
      
      if(n.NA>0){
        dist.temp=as.matrix(dist.temp)
        dist.temp=dist.temp[-which(rownames(dist.temp)%in%name.NA),-which(colnames(dist.temp)%in%name.NA)]
        dist.temp=as.dist(dist.temp)
        resp.var.data.temp=resp.var.data.temp[-which(is.na(resp.var.data.temp[,i])),]
      }
      
      row.names(resp.var.data.temp)<-resp.var.data.temp[,1]
      resp.var.data.temp<-resp.var.data.temp[labels(dist.temp),]
      
      res.beta=betadisper(dist.temp,resp.var.data.temp[,i])
      res.anova=anova(res.beta)
      res.dispersion[(i-1),]=c(n.NA,as.numeric(round(res.anova[1,],4)))
    }
    
    resp.var.homogene=as.numeric(which(res.dispersion[,"p-value"]>0.05)+1)
    resp.var.non.homogene=as.numeric(which(res.dispersion[,"p-value"]<=0.05)+1)
    
    sum.res[resp.var.non.homogene-1,1]<-text1
    sum.res[resp.var.homogene-1,1]<-text2
    
    res.adonis=adonis(as.formula(paste("dist.temp","~",paste(colnames(resp.var.data.temp)[resp.var.homogene],collapse="+"),sep="")),
                      data=resp.var.data.temp,
                      permutations=999)
    
    var.sign<-row.names(res.adonis$aov.tab)[which(res.adonis$aov.tab[,"Pr(>F)"]<0.05)]
    
    if(length(var.sign)!=0) sum.res[which(row.names(sum.res)%in%var.sign),1]<-text3
    
    res.adonis<-as.data.frame(res.adonis$aov.tab)
    for(i in 2:6){
      res.adonis[,i]<-round(res.adonis[,i],3)
      res.adonis[which(is.na(res.adonis[,i])),i]<-""
    }
    
  }
  
  if(resp.var.data[1,1]==""){
    dist.temp<-mat.dist.resp
    resp.var.data.temp<-resp.var.data
  } 
  
  ls.res=list(dist.temp,res.dispersion,res.adonis,sum.res,resp.var.data.temp)
  names(ls.res)=c("resp.mat.dist","res.dispersion","res.adonis","sum.res","new.resp.var.data")
  
  return(ls.res)
  
}