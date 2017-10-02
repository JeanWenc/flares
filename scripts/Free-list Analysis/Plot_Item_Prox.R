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

plot.item.prox<-function(dist.matrices,res.FL,norm.item.data,#ls.categories,
                         apac.categories,
                         freq,prox.index,label.size,
                         plot.type,item.categ,norm.col.temp,
                         min,max){
 #debug
  #dist.matrices<-item.prox
  #prox.index<-item.prox.index
  #label.size<-7 #1 to 100
  #plot.type<-item.prox.plot.type2
  #item.categ<-item.categ.temp
  #norm.col.temp<-norm.type
  #min<-dendo.min #for cuttree
  #max<-dendo.max #for cuttree
  
  min.freq=freq[1]/100
  max.freq=freq[2]/100
  
  label.size=label.size/100
  
  ls.items=res.FL[which(res.FL$freq.cit.rel>=min.freq),]
  ls.items=ls.items[which(ls.items$freq.cit.rel<=max.freq),]
  ls.items=ls.items[,1]
  
  plot.type=as.numeric(plot.type)# 1=dendogram; 2= MDS
  
  if(item.categ!="None"){
    norm.col=1
    if(norm.col.temp!="None") norm.col=which(names(norm.item.data)==norm.col.temp)
    
    stop=FALSE
    if(!is.null(apac.categories)){
      item.categ.col=which(names(apac.categories$item.max.cat)==item.categ)
      if(length(item.categ.col)!=0){
        stop=TRUE
        norm.and.cat.list=apac.categories$item.max.cat[,c(1,item.categ.col)]
      }
    }
    if(stop==FALSE){
      item.categ.col=which(names(norm.item.data)==item.categ)
      norm.and.cat.list=norm.item.data[,c(norm.col,item.categ.col)]
      norm.and.cat.list=unique(norm.and.cat.list)
    }
    
    norm.and.cat.list=norm.and.cat.list[which(norm.and.cat.list[,1]%in%ls.items),]
    norm.and.cat.list[,2]=factor(norm.and.cat.list[,2])
    rownames(norm.and.cat.list)=norm.and.cat.list[,1]
  }
  
  if(prox.index=="Henley index"){
    dist.mat=dist.matrices[[prox.index]]
    
    dist.mat=dist.mat[which(row.names(dist.mat)%in%ls.items),which(colnames(dist.mat)%in%ls.items)]
    
    dist.mat.dis=as.dist(dist.mat)
    
    if(item.categ!="None") norm.and.cat.list=norm.and.cat.list[row.names(dist.mat),]
    
    if(plot.type==1){
      hc.ob=hclust(dist.mat.dis)
      
      k<-best.cutree(hc = hc.ob,min = min,max=max,loss = F,graph=F)
      
      par(oma=c(1,1,1,1)) 
      res<-plot.dendro(hc.ob,k,norm.and.cat.list,item.categ,prox.index,label.size)
      res$plot<-res$plot+
        labs(title="Dendrogram of item by item proximity",subtitle=paste(prox.index," - Freq. of citation of plotted items ranges from ",freq[1],"% to ",freq[2],"% - Ideal groups = ",k, " (with min k=",min, " & max k=",max,")",sep=""))
      return(res)
    }
    
    if(plot.type==2){
      fit=cmdscale(dist.mat.dis,eig=TRUE,k=2)
      
      x <- fit$points[,1]
      y <- fit$points[,2]
      df<-data.frame(x=x,y=y)
      
      if(item.categ!="None"){
        df[,item.categ]<-norm.and.cat.list[row.names(df),item.categ]
        
        p<-ggplot(df,aes(x,y,group=df[,item.categ]))+
          geom_vline(xintercept=0,linetype="dotted")+
          geom_hline(yintercept=0,linetype="dotted")+
          geom_text(size=label.size*5,aes(label=row.names(df),colour=df[,item.categ]),vjust="inward",hjust="inward")+
          guides(color=guide_legend(title=item.categ))
          
        
      }else{
        p<-ggplot(df,aes(x,y))+
          geom_vline(xintercept=0,linetype="dotted")+
          geom_hline(yintercept=0,linetype="dotted")+
          geom_text(size=label.size*5,aes(label=row.names(df)),vjust="inward",hjust="inward")
      }
      
      p<-p+labs(title="Multidimensional scaling of Item-by-Item Proximity",subtitle=paste("Distance calculated with Henley index - Frequency of mention of plotted items ranges from ",freq[1],"% to ",freq[2],"%.",sep=""),x="V1",y="V2")+
        theme(panel.grid.minor=element_blank())
      
      tree.cut.df<-"None"
      
      res<-list(p,tree.cut.df)
      names(res)<-c("plot","partition")
      
      return(res)
    }
  }
  
  if(prox.index=="Successive count"){
    
    dist.mat=dist.matrices[[prox.index]]
    dist.mat=dist.mat[which(row.names(dist.mat)%in%ls.items),which(colnames(dist.mat)%in%ls.items)]
    
    if(item.categ!="None"){
      norm.and.cat.list=norm.and.cat.list[row.names(dist.mat),]
    }
    
    #res.CA=CA(dist.mat,graph=FALSE)
    res.CA=ca::ca(dist.mat, nd=4)
    
    #corres.analysis
    if(plot.type==2){
      
      #df<-as.data.frame(res.CA$col$coord[,c(1,2)])
      df<-as.data.frame(res.CA$colcoord[,c(1,2)])
      names(df)<-c("x",'y')
      
      if(item.categ!="None"){
        df[,item.categ]<-norm.and.cat.list[row.names(df),item.categ]
        
        p<-ggplot(df,aes(x,y,group=df[,item.categ]))+
          geom_hline(yintercept = 0,linetype="dotted")+
          geom_vline(xintercept = 0,linetype="dotted")+
          geom_text(size=label.size*5,aes(label=row.names(df),colour=df[,item.categ]),vjust="inward",hjust="inward")+
          guides(color=guide_legend(title=item.categ))
        
      }else{
        p<-ggplot(df,aes(x,y))+
          geom_hline(yintercept = 0,linetype="dotted")+
          geom_vline(xintercept = 0,linetype="dotted")+
          geom_text(size=label.size*5,aes(label=row.names(df)),vjust="inward",hjust="inward")
      }
      
      p<-p+labs(title="Correspondance analysis of Item-by-Item Proximity",subtitle=paste("Distance calculated by 'Successive count' - Frequency of mention of plotted items ranges from ",freq[1],"% to ",freq[2],"%.",sep=""),x="Dim 1",y="Dim 2")+
        theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
      
      tree.cut.df<-"None"
      
      res<-list(p,tree.cut.df)
      names(res)<-c("plot","partition")
      
      return(res)
    }
    
    #dendogram
    if(plot.type==1){
      #dist.CA<-dist(res.CA$col$coord,method="euclidian")
      dist.CA<-dist(res.CA$colcoord,method="euclidian")
      
      hc.ob=hclust(dist.CA)
      
      k<-best.cutree(hc = hc.ob,min = min,max=max,loss = F,graph=F)
      
      par(oma=c(1,1,1,1)) 
      res<-plot.dendro(hc.ob,k,norm.and.cat.list,item.categ,prox.index,label.size)
      res$plot<-res$plot+
        labs(title="Dendrogram of item by item proximity",subtitle=paste(prox.index," - Freq. of citation of plotted items ranges from ",freq[1],"% to ",freq[2],"% - Ideal groups = ",k, " (with min k=",min, " & max k=",max,")",sep=""))
      return(res)
    }
  }
  
}






