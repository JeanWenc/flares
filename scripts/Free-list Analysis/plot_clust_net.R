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

plot.clust.net<-function(patch.flow.tab,e.width.mult,v.size.mult,arrow.size,ls.categories,categ.for.netw,with.items,deg.ch,e.curve,freq,res.FL){
  
  #debug
  #categ.for.netw=categ.for.netw1
  #patch.flow.tab<-patch.flow1
  #e.width.mult<-1
  #v.size.mult<-5
  #arrow.size<-0.5
  #with.items<-FALSE
  #freq<-c(0,100)
  
  #deg.ch="meanFreq" #"in"/"out"/"meanFreq"/"meanN"
  #e.curve=0.1
  
  min.freq<-freq[1]/100
  max.freq<-freq[2]/100
  
  n.res.tab<-which(names(patch.flow.tab$patches)==categ.for.netw)
  res.tab<-patch.flow.tab$patches[[n.res.tab]]
  
  ls.categories<-ls.categories[-which(ls.categories%in%"None")]
  
  res.tab$Prev_Cat_SubCat<-paste(res.tab$Prev_Cat,res.tab$Prev_SubCat,sep="_")
  res.tab[res.tab$Prev_Cat_SubCat=="NA_NA","Prev_Cat_SubCat"]<-NA
  
  ls.resp<-unique(res.tab$ID_Resp)
  
  if(categ.for.netw=="All"){
    patch.tab<-as.data.frame(table(res.tab$ID_Resp,res.tab$Cat_SubCat,res.tab$ID_Stat))  
  }else{
    patch.tab<-as.data.frame(table(res.tab$ID_Resp,res.tab$Cat_SubCat,res.tab$ID_Fluid))  
  }
  
  patch.tab<-patch.tab[patch.tab$Freq>0,]
  
  patch.tab2<-as.data.table(patch.tab)
  patch.tab2<-as.data.frame(patch.tab2[,list(tot_num=sum(Freq),mean_num=mean(Freq),run_num=length(Freq)),by=c('Var1','Var2')])
  
  i.for.edge=1
  for.edge<-data.frame("Source"=character(),"Target"=character(),"patch.type"=character(),stringsAsFactors = F)
  for(i in 1:length(ls.resp)){
    resp.id<-ls.resp[i]
    temp<-res.tab[which(res.tab$ID_Resp==resp.id),]
    
    for.edge.temp<-temp[which(!is.na(temp$Patch_Type)),c("Prev_Cat_SubCat","Prev","Item","Cat_SubCat","Patch_Type")]
    for.edge.temp<-for.edge.temp[-1,]
    
    if(dim(for.edge.temp)[1]!=0){
      
      for(j in 1:dim(for.edge.temp)[2]) for.edge.temp[,j]<-as.character(for.edge.temp[,j])
      
      for(j in 1:dim(for.edge.temp)[1]){
        if(for.edge.temp[j,"Patch_Type"]=="static"){
          if(with.items==TRUE){
            for.edge[i.for.edge,]<-c(as.character(for.edge.temp[j,c("Prev_Cat_SubCat","Prev")]),"fluid")
            for.edge[i.for.edge+1,]<-c(as.character(for.edge.temp[j,c("Prev","Item")]),"fluid")
            for.edge[i.for.edge+2,]<-c(as.character(for.edge.temp[j,c("Item","Cat_SubCat")]),"fluid")  
            i.for.edge=i.for.edge+3
          }else{
            for.edge[i.for.edge,]<-c(as.character(for.edge.temp[j,c("Prev_Cat_SubCat","Cat_SubCat")]),"static")
            i.for.edge=i.for.edge+1  
          }
        }else{
          for.edge[i.for.edge,]<-c(as.character(for.edge.temp[j,c("Prev_Cat_SubCat","Cat_SubCat")]),"static")
          i.for.edge=i.for.edge+1
        }
      }
    }
  }
  
  for.edge$Freq<-1
  ls.del<-grep("NA",for.edge$Source)
  ls.del<-c(ls.del,grep("NA",for.edge$Target))
  if(length(ls.del)!=0) for.edge<-for.edge[-ls.del,]
  
  for.edge1<-as.data.table(for.edge)
  for.edge1<-as.data.frame(for.edge1[,list(Freq=sum(Freq)),by=c('Source','Target','patch.type')])
  for.edge1$ord<-seq(1:dim(for.edge1)[1])
  
  for.edge1$Freq.rel<-for.edge1$Freq/max(for.edge1$Freq)
  for.edge1<-for.edge1[which(for.edge1$Freq.rel>=min.freq),]
  for.edge1<-for.edge1[which(for.edge1$Freq.rel<=max.freq),]
  
  for.edge1$ed.lty<-factor(for.edge1$patch.type,levels=c("static","fluid"),ordered=T)
  for.edge1$ed.lty<-as.numeric(for.edge1$ed.lty)
    
  net.temp<-graph.edgelist(as.matrix(for.edge1[,c(1,2)]))
  
  if(categ.for.netw=="All"){
    ls.categories1<-data.frame(categ=ls.categories,nchar=nchar(ls.categories))
    ls.categories1<-ls.categories1[order(ls.categories1$nchar,decreasing=T),]
    ls.categories1<-as.character(ls.categories1[,1])
    
    node.labels<-names(V(net.temp))
    categ.names<-names(V(net.temp))
    for(i in 1:length(ls.categories1)){
      categ<-ls.categories1[i]
      to.change<-grep(categ,node.labels)
      if(length(to.change)!=0){
        node.labels[to.change]<-sub(paste(categ,"_",sep=""),"",node.labels[to.change])
        categ.names[to.change]<-categ
        }
    }
    if(with.items==TRUE){
      categ.names[which(categ.names%in%res.FL$Cited_Items)]<-"Items"
    }
  }else{
    categ.names<-categ.for.netw
    node.labels<-names(V(net.temp))
    node.labels<-sub(paste(categ.for.netw,"_",sep=""),"",node.labels)
  }
  ##################Getting meanFreq and mean number of items per node##########################
  if(deg.ch%in%c("meanFreq","meanN")){
    temp.tab<-patch.flow.tab$sum.categories
    tot.n.items.categ<-lapply(temp.tab,function(x)apply(x[,grep(pattern = "_n.items",x=colnames(x))],2,sum))
    if(deg.ch=="meanFreq"){
      mean.x.categ<-lapply(temp.tab,function(x)apply(x[,grep(pattern = "sum.freq",x=colnames(x))],2,sum))  
    }else{
      mean.x.categ<-lapply(temp.tab,function(x) apply(x[,grep(pattern="_n.items",x=colnames(x))],2,function(x2) length(which(x2>0))))  
    }
    
    if(categ.for.netw=="All"){
      tot.n.items.categ<-unlist(tot.n.items.categ)
      mean.x.categ<-unlist(mean.x.categ)
      
      net.names.temp<-gsub("_",".",names(V(net.temp)))
    }else{
      tot.n.items.categ<-tot.n.items.categ[[categ.for.netw]]
      mean.x.categ<-mean.x.categ[[categ.for.netw]]
      
      net.names.temp<-gsub(paste(categ.for.netw,"_",sep=""),"",names(V(net.temp)))
      net.names.temp<-gsub("_",".",net.names.temp)
    }
    
    if(deg.ch=="meanFreq"){
      mean.categ<-round(mean.x.categ/tot.n.items.categ,3)
    }else{
      mean.categ<-round(tot.n.items.categ/mean.x.categ,3)
    }
    
    pat.str<-ifelse(deg.ch=="meanFreq","_sum.freq","_n.items")
    
    names(mean.categ)<-sub(pattern = pat.str,replacement = "",names(mean.categ))
    names(mean.categ)<-gsub(pattern="_",".",names(mean.categ))
    
    mean.categ<-mean.categ[net.names.temp]
    names(mean.categ)<-net.names.temp
    
    if(with.items==TRUE){
      na.mean.categ<-as.numeric(which(is.na(mean.categ)))
      
      if(deg.ch=="meanFreq"){
        na.name.mean.categ<-names(mean.categ)[which(is.na(mean.categ))]
        
        res.FL.temp<-res.FL
        row.names(res.FL.temp)<-res.FL$Cited_Items
        row.names(res.FL.temp)<-gsub("_",".",row.names(res.FL.temp))
        
        res.FL.temp<-res.FL.temp[na.name.mean.categ,"freq.cit.rel"]
        
        mean.categ[na.mean.categ]<-res.FL.temp  
      }else{
        mean.categ[na.mean.categ]<-1
      }
    }
    node.size<-(as.numeric(mean.categ)*100)
  }else{#if in-degree or out-degree
    node.size<-as.numeric(graph.strength(net.temp, V(net.temp),mode=deg.ch,loops=FALSE))
  }
  
  net.temp=igraph::set.vertex.attribute(net.temp,"categ",V(net.temp),categ.names)
  net.temp=igraph::set.vertex.attribute(net.temp,"node.labels",V(net.temp),node.labels)
  net.temp=igraph::set.vertex.attribute(net.temp,"node.size",V(net.temp),node.size)
  
  net.temp<-igraph::set.edge.attribute(net.temp,"weight",E(net.temp),for.edge1$Freq)
  net.temp<-igraph::set.edge.attribute(net.temp,"e.lty",E(net.temp),for.edge1$ed.lty)
  
  if(e.curve==0) e.curve<-FALSE
  
  col.categ<-factor(V(net.temp)$categ)
  levels(col.categ)<-brewer.pal(12,"Set3")[1:length(levels(col.categ))]
  
  plot(net.temp,
       layout=layout_nicely,
       edge.width=(log(E(net.temp)$weight)+1)*e.width.mult,
       edge.curved=e.curve,
       edge.lty=E(net.temp)$e.lty,
       edge.color="grey75",
       edge.arrow.size=arrow.size,
       edge.arrow.width=arrow.size*2,
       vertex.color=as.character(col.categ),
       vertex.size=(log(V(net.temp)$node.size)+1)*v.size.mult,
       vertex.label=V(net.temp)$node.labels,
       vertex.label.color="grey25",
       vertex.label.family="sans",
       vertex.label.font=2,rescale=TRUE
       )
  legend("topleft",legend=levels(factor(V(net.temp)$categ)), col=as.character(levels(col.categ)), pch=16,pt.cex = 1.3, title="Categories")
}
