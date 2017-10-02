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

patch.flow<-function(FL.list,norm.type,ls.categories, resp.var.data,tree.partition,min.max,debug){
  
  #Debug
  #FL.list<-res1$FL.list
  #debug=TRUE
  #norm.type=norm.type2
  #resp.var.data=resp.var.data1
  if(norm.type=="None") norm.type<-"Cited_Items"
  
  ls.resp<-unique(FL.list$ID_RESP)
  
  res.fl<-table(FL.list[,norm.type])
  res.fl<-res.fl/length(ls.resp)
  
  if(is.data.frame(tree.partition)){
    FL.list=merge(FL.list,tree.partition,by.x=norm.type,by.y=0,all=T)
    FL.list$tree.cut<-as.character(FL.list$tree.cut)
    FL.list[which(is.na(FL.list$tree.cut)),"tree.cut"]<-"k0"
    FL.list$tree.cut<-factor(FL.list$tree.cut)
  }else{
    tree.part.col<-which(ls.categories=="tree_partition")
    if(length(tree.part.col)!=0) ls.categories<-ls.categories[-tree.part.col]
  }
  
  FL.list=FL.list[order(FL.list$tot_order),]
  
  ls.categories=ls.categories[-which(ls.categories=="None")]
  
  for(i in 1:length(ls.categories)){
    ls.lev<-which(levels(FL.list[,ls.categories[i]])=="")
    if(length(ls.lev)!=0){
      levels(FL.list[,ls.categories[i]])[ls.lev]<-NA
    }
  }
  ##########################
  res.tab.ls<-list()
  
  res.tab=matrix(data = NA,nrow = dim(FL.list)[1],ncol=12,
                 dimnames = list(seq(1:dim(FL.list)[1]),c("ID_Resp","ID_Fluid","ID_Stat","Cat","SubCat","Cat_SubCat","Order","Item","Patch_Type","Prev","Prev_Cat","Prev_SubCat"))
  )
  res.tab=as.data.frame(res.tab)
  
  res.tab$ID_Resp=FL.list$ID_RESP
  res.tab$Order=FL.list$Order
  res.tab$Item=FL.list[,norm.type]
  res.tab[seq(from=2, to=dim(FL.list)[1],by=1),"Prev"]<-as.character(FL.list[seq(1:(dim(FL.list)[1]-1)),norm.type])
  
  #list to put a summary of n.runs, n.items by informant
  cat.sum.tab<-list()
  
  for(i in 1:length(ls.categories)){
    categ.moda.names<-sort(unique(FL.list[,ls.categories[i]]))
    
    cat.sum.tab.df.names<-as.vector(t(outer(categ.moda.names,c("n.items","n.runs","mean.items.p.run","sum.freq"),paste,sep="_")))
    cat.sum.tab.df<-matrix(data=0,ncol=length(cat.sum.tab.df.names),nrow=length(ls.resp),dimnames=list(ls.resp,cat.sum.tab.df.names))
    cat.sum.tab[[i]]<-cat.sum.tab.df
    names(cat.sum.tab)[i]<-ls.categories[i]
    
    res.tab.temp<-res.tab
    res.tab.temp$Cat=ls.categories[i]
    res.tab.temp$SubCat=FL.list[,ls.categories[i]]
    res.tab.temp$Cat_SubCat=paste(res.tab.temp$Cat,res.tab.temp$SubCat,sep="_")
    
    res.tab.ls[[i]]<-res.tab.temp
    names(res.tab.ls)[i]<-ls.categories[i]
  }
  
  if(length(ls.categories)>1){
    res.tab.ls[[i+1]]<-res.tab
    names(res.tab.ls)[i+1]<-"All"
  }
  
  ls.n<-length(res.tab.ls)
  
  #Loop initialisation
  static<-1
  fluid<-1
  fluid2<-1
  res.tab.start<-1
  res.tab.start2<-1
  
  prog<-1/length(ls.resp)
  
  for(i in 1:length(ls.resp)){
    resp.ID<-ls.resp[i]
    freelist<-FL.list[which(FL.list$ID_RESP==resp.ID),]
    list.length<-dim(freelist)[1]
    
    categ<-data.frame(freelist[,ls.categories])
    names(categ)<-ls.categories
    
    list.start<-1
    next.patch<-"start"
    
    repeat{
      #The below condition can only be met after a first run of the repeat loop
      #It is met if the last item of the previous patch and the first item of the next patch share at least one subcategory
      if(next.patch=="static"){
        if(min.max=="min"){
          min.i.value<-min(i.value[which(i.value>1)])
          min.max.i.value<-min.i.value
        }else{
          max.i.value<-max(i.value)
          min.max.i.value<-max.i.value
        }
        name.min.max.i.value<-names(which(i.value==min.max.i.value))[1]
        name.min.max.i.value2<-list.test[[name.min.max.i.value]]$values[1]
        min.max.i.value<-min.max.i.value-1
      }
      
      #The below condition is met at the start of a respondent's list or if the last item of the previous patch and the first item of the next patch share no subcategory whatsoever
      if(list.start==1 | next.patch=="fluid"){
        line.seq=seq(from=list.start,to=dim(categ)[1],by=1)
        
        for(j in 1:length(ls.categories)){
          if(j==1) list.test<-list(as.list(rle(as.character(categ[line.seq,j]))))
          if(j!=1) list.test[[j]]<-as.list(rle(as.character(categ[line.seq,j])))
        }
        
        names(list.test)=ls.categories
        
        #####Summary of n.items, n.runs per modality of categories####
        if(list.start==1){
          for(j in 1:length(ls.categories)){
            categ.moda<-sort(as.character(unique(FL.list[,ls.categories[j]])))
            
            #calculating clustering coefficient for each modality of the variable
            for(k in 1:length(categ.moda)){
              #column count for cat.sum.tab
              sel.col2<-4*k
              
              ls.categ.moda<-which(list.test[[j]]$values==categ.moda[k])
              ls.lines<-numeric()
              
              if(length(ls.categ.moda)!=0){
                
                for(l in 1:length(ls.categ.moda)){
                  init.l<-sum(list.test[[j]]$lengths[1:ls.categ.moda[l]-1])+1
                  end.l<-sum(list.test[[j]]$lengths[1:ls.categ.moda[l]])
                  temp.seq<-seq(from=init.l,to=end.l,by=1)
                  ls.lines<-c(ls.lines,temp.seq)
                }
                it.names<-as.character(freelist[ls.lines,norm.type])
                it.freq<-as.numeric(res.fl[it.names])
                sum.freq<-sum(it.freq)#c'est une somme qui sera ensuite transformée en moyenne dans plot.clust.net
                
                n.items<-sum(list.test[[j]]$lengths[ls.categ.moda])
                n.runs<-length(ls.categ.moda)
                
                cat.sum.tab[[j]][resp.ID,(sel.col2-3)]<-n.items
                cat.sum.tab[[j]][resp.ID,(sel.col2-2)]<-n.runs
                cat.sum.tab[[j]][resp.ID,sel.col2-1]<-round(n.items/n.runs,2)
                cat.sum.tab[[j]][resp.ID,sel.col2]<-sum.freq
              }#end if length(ls.categ.moda)!=0
            }# end k
            
            #for each categories alone all patches are fluid
            run.length=list.test[[j]]$lengths
            for(k in 1:length(run.length)){
              new.patch.line<-ifelse(k==1,res.tab.start2,sum(run.length[c(1:(k-1))])+res.tab.start2)
              res.tab.ls[[j]][seq(from=new.patch.line,to=new.patch.line+(run.length[k]-1),by=1),"ID_Fluid"]<-fluid2
              fluid2<-fluid2+1
              res.tab.ls[[j]][new.patch.line,"Prev_Cat"]<-ifelse(k==1,NA,as.character(res.tab.ls[[j]][(new.patch.line-1),"Cat"]))
              res.tab.ls[[j]][new.patch.line,"Prev_SubCat"]<-ifelse(k==1,NA,as.character(res.tab.ls[[j]][(new.patch.line-1),"SubCat"]))
              res.tab.ls[[j]][new.patch.line,"Patch_Type"]<-"fluid"
            }
          }#end j
          res.tab.start2=sum(run.length)+res.tab.start2
          #if there is only one category all patches are fluid, the repeat loop just has to run once
          if(length(ls.categories)==1) break
        }#end if list.start==1
        
      
        #####End of Summary of n.items, n.runs per modality of categories
  
        i.value=unlist(
          lapply(list.test,FUN= function(x) 
            x[[1]][1]
          )
        )
        
        min.max.i.value<-1
        if(length(which(i.value>1))!=0){
          if(min.max=="min"){
            min.i.value<-min(i.value[which(i.value>1)])
            min.max.i.value<-min.i.value
          }else{
            max.i.value<-max(i.value)
            min.max.i.value<-max.i.value
          }
        } 
        name.min.max.i.value=names(which(i.value==min.max.i.value))[1]
        name.min.max.i.value2=list.test[[name.min.max.i.value]]$values[1]
      }
      
      res.tab.lines=seq(from=res.tab.start,to=((res.tab.start-1)+min.max.i.value),by=1)
      
      item.order=seq(from=list.start,to=((list.start-1)+min.max.i.value),by=1)
      
      res.tab.ls[[ls.n]][res.tab.lines,"ID_Fluid"]=fluid
      res.tab.ls[[ls.n]][res.tab.lines,"ID_Stat"]=static
      res.tab.ls[[ls.n]][res.tab.lines,"Cat"]=name.min.max.i.value
      res.tab.ls[[ls.n]][res.tab.lines,"SubCat"]=name.min.max.i.value2
      res.tab.ls[[ls.n]][res.tab.lines,"Cat_SubCat"]=paste(name.min.max.i.value,name.min.max.i.value2,sep="_")
      
      if(list.start!=1){
        res.tab.ls[[ls.n]][res.tab.start,"Patch_Type"]=next.patch
        res.tab.ls[[ls.n]][res.tab.start,"Prev_Cat"]=res.tab.ls[[ls.n]][(res.tab.start-1),"Cat"]
        res.tab.ls[[ls.n]][res.tab.start,"Prev_SubCat"]=res.tab.ls[[ls.n]][(res.tab.start-1),"SubCat"]
      }else{
        res.tab.ls[[ls.n]][res.tab.start,"Patch_Type"]="fluid"
      }
      
      #the following lines looks whether the last item of the patch belongs to a same subcategory as the first item of the next patch
      min.max.i.value.temp<-ifelse(next.patch=="static",min.max.i.value+1,min.max.i.value)
      if(min.max.i.value.temp==1) next.patch="fluid"
      
      if(min.max.i.value.temp!=1){
        line.seq=seq(from=((list.start-1)+min.max.i.value),to=dim(categ)[1],by=1)
        for(j in 1:length(ls.categories)){
          if(j==1) list.test=list(as.list(rle(as.character(categ[line.seq,j]))))
          if(j!=1) list.test[[j]]=as.list(rle(as.character(categ[line.seq,j])))
        }
        
        names(list.test)=ls.categories
        
        i.value=unlist(
          lapply(list.test,FUN= function(x) 
            x[[1]][1]
          )
        )
        
        next.patch<-ifelse(length(which(i.value>1))!=0,"static","fluid")
      }
      
      static<-static+1
      if(next.patch=="fluid") fluid<-fluid+1
      
      list.start<-list.start+min.max.i.value
      
      res.tab.start<-res.tab.start+min.max.i.value
      
      if(list.start>list.length) break
    }
    if(debug==FALSE) incProgress(prog,detail=paste(round(i/length(ls.resp)),"%",sep=" "))
  }
  
  res<-list(res.tab.ls,cat.sum.tab)
  names(res)<-c("patches","sum.categories")
  
  return(res)
}