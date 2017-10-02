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

plot.resp.prox<-function(dist.data,resp.var,freq){
  #resp.var<-resp.var.name1
  #dist.data<-resp.prox
  min.freq<-freq[1]
  max.freq<-freq[2]
  
  resp.var.data<-dist.data$new.resp.var.data
  
  dist=dist.data$resp.mat.dist
  dist.dudi=dudi.pco(dist,scannf=F,nf=2)

  if(resp.var=="No Variable"){
    p<-ggplot(dist.dudi$li,aes(A1,A2))+
      geom_vline(xintercept=0,linetype="dotted")+
      geom_hline(yintercept=0,linetype="dotted")+
      geom_text(aes(label=row.names(dist.dudi$li)),vjust="inward",hjust="inward")+
      labs(title="Respondent by respondent proximity - Principal coordinates analysis ",
           subtitle=paste(dim(dist.dudi$li)[1]," respondents. Distance estimated w/ Jaccard index (pres/abs of items across resp.'s lists)\n using items cited by ",min.freq," to ",max.freq,"% of respondents",sep=""),x="Dim 1",y="Dim 2")
  }
  
  if(resp.var!="No Variable"){
    for(i in 2:dim(resp.var.data)[2]){
      resp.var.data[,i]=factor(resp.var.data[,i])
    }
    
    nb.mod=length(unique(resp.var.data[,resp.var]))
    if(nb.mod<10){
      col.pt=brewer.pal(n=9,"Set1")[1:nb.mod]  
    }else{
      col.pt=c(brewer.pal(n=9,"Set1"),brewer.pal(n=8,"Accent"))[1:nb.mod]
    }
    
    p<-s.class(dist.dudi$li,resp.var.data[,resp.var],col=col.pt,plot=F,main="Resp. by Resp proximity - Principal coordinates analysis\n",
               psub=list(position="topleft",text=resp.var),sub=paste("\n",dim(dist.dudi$li)[1]," respondents. \nDistance estimated w/ Jaccard index (pres/abs of items across resp.'s lists)\n using items cited by ",min.freq," to ",max.freq,"% of respondents\n",sep=""),
               pellipses=list(alpha=0.3),
               plegend=list(drawKey=TRUE,drawColorKey=TRUE))
  }
  
  return(p)
}



