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

plot.dendro<-function(hc,k,norm.and.cat.list,item.categ,prox.index,label.size){
  #debug
  #hc<-hc.ob
  
  dendr    <- dendro_data(hc, type="rectangle") # convert for ggplot
  
  tree.cut<-cutree(hc,k=k)
  
  tree.cut.df<-as.data.frame(tree.cut)
  tree.cut.df$tree.cut=paste("k",tree.cut.df$tree.cut,sep="")
  tree.cut.df[,1]<-as.factor(tree.cut.df[,1])
  
  if(item.categ=="None"){
    
    clust.df <- data.frame(label=sort(hc$labels), cluster=factor(tree.cut[sort(names(tree.cut))]))
  }else{
    clust.df <- data.frame(label=hc$labels, cluster=factor(norm.and.cat.list[dendr[["labels"]]$label,2]))
    row.names(clust.df)<-clust.df[,1]
  }
  
  dendr[["labels"]]   <- merge(dendr[["labels"]],clust.df, by="label")
  
  if(item.categ=="None"){
    rect <- aggregate(x~cluster,label(dendr),range)
    rect <- data.frame(rect$cluster,rect$x)
    ymax <- mean(hc$height[length(hc$height)-((k-2):(k-1))])
    ymax2<-ymax+((hc$height[length(hc$height)-(k-2)]-ymax)/2)
    p<-ggplot() + 
      geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
      geom_text(data=label(dendr), aes(x, y, label=label, hjust=0, color=cluster),size=label.size*5) +
      geom_rect(data=rect, aes(xmin=X1-.3, xmax=X2+.3, ymin=0, ymax=ymax),color="red", fill=NA)+
      geom_hline(yintercept=ymax2, color="blue")+
      coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + 
      theme_dendro()
    
  }else{
    ymax2 <- mean(hc$height[length(hc$height)-((k-2):(k-1))])  
    
    p<-ggplot() + 
      geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
      geom_text(data=label(dendr), aes(x, y, label=label, hjust=0, color=cluster),size=label.size*5) +
      geom_hline(yintercept=ymax2, color="blue")+
      coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + 
      theme_dendro()
  }
  res<-list(p,tree.cut.df)
  names(res)<-c("plot","partition")
  return(res)
  
}
  
