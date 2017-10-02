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

categ.clustering<-function(FL.list,norm.type,ls.categories, resp.var.data,tree.partition){
  
  #Debug
    #FL.list<-res1$FL.list
    #norm.type=norm.type2
    #resp.var.data=resp.var.data1

  #This is to calculate clust.proba, but problem with formula in Robbins & nolan 2000 estimation of overall C proba only works for variable with two modalities
  #It's to calculate combinations noted with parentheses
  #comb<-function(n,x){
  #  return(factorial(n)/(factorial(x)*factorial(n-x)))
  #}
  
  if(norm.type=="None") norm.type<-"Cited_Items"
  
  #adding tree.partition to FL.list
  if(is.data.frame(tree.partition)){
    FL.list=merge(FL.list,tree.partition,by.x=norm.type,by.y=0,all=T)
    FL.list$tree.cut<-as.character(FL.list$tree.cut)
    FL.list[which(is.na(FL.list$tree.cut)),"tree.cut"]<-"k0"
    FL.list$tree.cut<-factor(FL.list$tree.cut)
  }else{
    tree.part.col<-which(ls.categories=="tree_partition")
    if(length(tree.part.col)!=0) ls.categories<-ls.categories[-tree.part.col]
  }
  
  #reordering FL.list
  FL.list=FL.list[order(FL.list$tot_order),]
  ls.categories=ls.categories[-which(ls.categories=="None")]
  ls.resp=unique(FL.list$ID_RESP)
  
  #Transforming "" into NA
  for(i in 1:length(ls.categories)){
    ls.lev<-which(levels(FL.list[,ls.categories[i]])=="")
    if(length(ls.lev)!=0){
      levels(FL.list[,ls.categories[i]])[ls.lev]<-NA
    }
  }
  
  #creating the bias.tab
  bias.tab<-data.frame(empty="")#ligne importante car s'il n'y a aucune catégorie dichotomique bias.tab ne sera jamais créé.
  bias.tab.temp=apply(as.data.frame(FL.list[,ls.categories]),2,function(x) length(levels(as.factor(x))))
  names(bias.tab.temp)=ls.categories
  bias.categ=names(bias.tab.temp)[which(bias.tab.temp==2)]
  if(length(bias.categ)!=0){
    for(i in 1:length(bias.categ)){
      if(i==1) {
        column.names<-paste(bias.categ[i],sort(levels(FL.list[,bias.categ[i]])),sep="_")
      }else{
        column.names<-c(column.names,paste(bias.categ[i],sort(unique(FL.list[,bias.categ[i]])),sep="_"))
        }
    }
    bias.tab<-matrix(data=NA,nrow=length(ls.resp),ncol=length(column.names), dimnames=list(ls.resp,column.names))
  }
  
 
  #creating the clust.Tab and cat.sum.tab
  for(i in 1:length(ls.categories)){
    categ.moda.names<-sort(unique(FL.list[,ls.categories[i]]))
    if(i==1) {
      column.names<-c(paste(ls.categories[i],categ.moda.names,sep="_"),ls.categories[i])
      }else{
      column.names<-c(column.names,c(paste(ls.categories[i],categ.moda.names,sep="_"),ls.categories[i]))
      }
    clust.tab<-matrix(data=NA,nrow=length(ls.resp),ncol=length(column.names), dimnames=list(ls.resp,column.names))
  }
  
  #This would be necessary to calculate probalities of obtaining cluster scores but problem with Nolan & Robbins 2000
  #clust.poba.tab<-clust.tab

  for(i in 1:length(ls.resp)){
    resp.ID=ls.resp[i]
    freelist=FL.list[which(FL.list$ID_RESP==resp.ID),]
    list.length=dim(freelist)[1]
    
    ##########NOLAN Dichotomous Bias######################
    
    if(length(bias.categ)!=0){
      for(j in 1:length(bias.categ)){
        ls.subcat=as.character(sort(unique(FL.list[,bias.categ[j]])))
        for(k in 1:2){
          ls.row.subcat1<-which(freelist[,bias.categ[j]]==ls.subcat[k])
          
          if(length(ls.row.subcat1)!=0){
            sum.rk.subcat1<-sum(ls.row.subcat1)
            
            L.sc1<-length(ls.row.subcat1)
            L.sc2<-list.length-L.sc1
            
            if(L.sc2==0){
              b.param<-1
              }else{
              b.param<-((L.sc1*(L.sc1+(2*L.sc2)+1))-(2*sum.rk.subcat1))/(2*L.sc1*L.sc2)      
              }
          } else {b.param<-0}
          
          bias.tab[resp.ID,(2*j-1)]<-round(b.param,3)
          bias.tab[resp.ID,(2*j)]<-1-round(b.param,3)
        }
      }
    }
    
    ###########STOP NOLAN Dichotomous Bias################
    
    categ=data.frame(freelist[,ls.categories])
    names(categ)=ls.categories
    
    ######NOLAN Clustering coefficient##########
    #column count for clust.tab
    
    sel.col<-1
    
    for(j in 1:length(ls.categories)){
      
      list.test<-rle(as.character(categ[,j]))
      
      categ.moda<-sort(as.character(unique(FL.list[,ls.categories[j]])))
      
      #calculating clustering coefficient for each modality of the variable
      for(k in 1:length(categ.moda)){
        ls.categ.moda<-which(list.test$values==categ.moda[k])
        
        if(length(ls.categ.moda)!=0){
          n.items<-sum(list.test$lengths[ls.categ.moda])
          n.runs<-length(ls.categ.moda)
          min.clus<-ifelse((list.length-n.items)<(n.items-1),(list.length-n.items+1),n.items)

          if(min.clus==1){
            c.param=1
            }else{
              c.param<-(min.clus-n.runs)/(min.clus-1)  
              }
          
          if(!exists("max.moda")){
            max.moda=n.items
            } else if (n.items>max.moda){
              max.moda=n.items
            }
          } else {c.param=0}
        clust.tab[resp.ID,sel.col]<-round(c.param,3)
        
        #below formula is to calculate the proba of obtaining the c.param considering the number of runs, the number of items and the total number of items (see Nolan & Robbins 2000)
        #on hold because the formula for the proba of overall C (below) in Nolans & Robbins 2000 only works if two modalities for the variable.
        #clust.proba<-comb(n.items-1,n.items-n.runs)*comb(list.length-n.items+1,n.runs)/comb(list.length,n.items)
        #clust.proba.tab[resp.ID,sel.col]<-round(clust.proba,4)
        
        sel.col<-sel.col+1  
        }# end k loop
      
      #calculating overall clustering coefficient (for each modality of the variable)
      n.items<-list.length
      n.runs<-length(list.test$lengths)
      max.clus<-length(unique(list.test$values))
      min.clus<-ifelse((list.length-max.moda)<(max.moda-1),2*(list.length-max.moda)+1,n.items)
      
      if(min.clus==1){
        c.param=1
        }else{
          c.param<-(min.clus-n.runs)/(min.clus-max.clus)
          if(is.na(c.param)) c.param<-0
        }
      
      clust.tab[resp.ID,sel.col]<-round(c.param,3)
      
      #see Robbins & Nolan the formula they offer only works for variable with two modalities.
      #clust.proba<-
      
      sel.col=sel.col +1
      
      rm(max.moda)
      
      }# end j loop (ls.cateogires)
    
    ######End of NOLAN Clustering coefficient###    
    }# end i loop (ls.resp)
  
  ####################################################
  #################Averaging and z-score############
  ####################################################
  
  res.sum.tab<-list()
  
  if(length(bias.categ)!=0){
    zscore.ls<-list(clust.tab,bias.tab)
    names(zscore.ls)<-c("clust.tab","bias.tab")
  }else{
    zscore.ls<-list(clust.tab)
    names(zscore.ls)<-("clust.tab")
  }
  
  for(i in 1:length(zscore.ls)){
    sel.tab<-names(zscore.ls)[i]
    
    temp.tab<-as.matrix(round(apply(zscore.ls[[i]],2,mean),3))  
    colnames(temp.tab)<-"Total Sample"
    temp.tab<-t(temp.tab)
    n.resp=dim(zscore.ls[[i]])[1]
    
    #calculating mean of scores according to resp categories
    if(is.data.frame(resp.var.data)){
      k2=2
      for(j in 2:dim(resp.var.data)[2]){
        resp.var.data[,j]<-as.factor(resp.var.data[,j])
        ls.moda<-as.character(levels(resp.var.data[,j]))
        #ls.moda<-as.character(unique(resp.var.data[,j]))
        for(k in 1:length(ls.moda)){
          ls.resp.moda<-as.character(resp.var.data[which(resp.var.data[,j]==ls.moda[k]),1])
          temp.tab.resp<-zscore.ls[[i]][which(rownames(zscore.ls[[i]])%in%ls.resp.moda),]
          
          n.resp=c(n.resp,length(ls.resp.moda))
          #n.resp=c(n.resp,dim(temp.tab.resp)[1])
          
          if(length(ls.resp.moda)==1){
            temp.line<-temp.tab.resp
          }else{
            temp.line<-as.numeric(round(apply(temp.tab.resp,2,mean),3))
          }
                            
          temp.tab<-rbind(temp.tab,temp.line)

          #temp.tab<-rbind(temp.tab,as.numeric(round(apply(temp.tab.resp,2,mean),3)))  
          
          row.name.temp=paste(names(resp.var.data)[j],ls.moda[k],sep="_")
          rownames(temp.tab)[k2]<-row.name.temp
          
          k2=k2+1
        }
      } 
    }
    
    temp.tab<-cbind(temp.tab,n.resp)
    
    z.tab<-temp.tab
    z.tab[,"n.resp"]<-sqrt(1/(4*z.tab[,"n.resp"]))
    
    p.val<-temp.tab
    
    #if bias categoires
    if(i==2){
      for(j in 1:length(bias.categ)){
        col<-c(2*j-1,2*j)
        z.tab[,col]<-round((z.tab[,col]-0.5)/z.tab[,dim(z.tab)[2]],3)
        z.tab[which(z.tab[,col]<0)]<-NA
        p.val[,col]<-1-pnorm(z.tab[,col])
      }
    }else{#for clustering
      last.col<-dim(z.tab)[2]
      z.tab[,-last.col]<-round((z.tab[,-last.col]-0.5)/z.tab[,last.col],3)
      p.val[,-last.col]<-1-pnorm(z.tab[,-last.col])
    }
    
    z.tab[,dim(z.tab)[2]]<-""
    z.tab[is.na(z.tab)]<-""
    z.tab[which(z.tab!="")]<-paste("(z=",z.tab[which(z.tab!="")],")",sep="")
    
    p.val[which(as.numeric(p.val)<0.01)]<-"***"
    p.val[which(as.numeric(p.val)<0.05)]<-"**"
    p.val[which(as.numeric(p.val)<0.1)]<-"*"
    p.val[which(as.numeric(p.val)>=0.1)]<-""
    p.val[,dim(p.val)[2]]<-""
    p.val[is.na(p.val)]<-""
    
    z.tab[which(p.val=="")]<-""
    
    temp.tab2<-matrix(paste(temp.tab,p.val,z.tab),ncol=dim(temp.tab)[2],nrow=dim(temp.tab)[1])
    dimnames(temp.tab2)<-dimnames(temp.tab)
    temp.tab2<-as.data.frame(temp.tab2)
    
    res.sum.tab[[i]]<-temp.tab2
    names(res.sum.tab)[i]<-sel.tab
  }
  
  if(i==2){
    bias.tab.sum<-as.data.frame(res.sum.tab[[2]])
  }else{
    bias.tab.sum<-data.frame(empty="")
  }
  
  clust.tab.sum<-as.data.frame(res.sum.tab[[1]])
  
  bias.tab<-as.data.frame(bias.tab)
  clust.tab<-as.data.frame(clust.tab)
  res<-list(bias.tab,bias.categ,clust.tab,bias.tab.sum,clust.tab.sum)
  names(res)<-c("dichot_bias","dichot_bias_categ","clustering","dichot_bias_sum","clustering_sum")
  
  return(res)
}