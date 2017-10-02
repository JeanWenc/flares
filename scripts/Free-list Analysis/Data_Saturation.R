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

data.sat<-function(mat.cit.abs.pres){
  
  nb.item=dim(mat.cit.abs.pres)[1]
  
  data.sat.tab=as.data.frame(list("",0))
  names(data.sat.tab)=c("Resp","new.cit.items")
  
  data.sat.tab$Resp=as.character(data.sat.tab$Resp)
  data.sat.tab$new.cit.items=as.numeric(data.sat.tab$new.cit.items)
  
  temp2=mat.cit.abs.pres
  
  #initialisation
  i=2
  new.item.count=0
  #Boucle qui s'arrete des qu'on a atteint le nombre total de taxons differents
  while(new.item.count<nb.item){
    #reactualisation de la longeur des listes de chaque individus apres suppresion de taxons cites par les precedents
    new.list.len=as.data.frame(apply(temp2,2,sum))
    names(new.list.len)="new.list.len"
    
    #identification de l'individu avec le plus de nouveaux items cites
    max.cit=max(new.list.len$new.list.len)
    ind.temp=row.names(new.list.len)[which(new.list.len$new.list.len==max.cit)]
    
    #si plusieurs individus identifies selection de celui avec la plus petite summed frequency, et a defaut du premier de la liste
    if(length(ind.temp)>1){
      ind.temp=ind.temp[1]
    }
    
    #reactualisation du nombre de nouveaux taxons
    new.item.count=max.cit+as.numeric(data.sat.tab[i-1,2])
    #remplissage du tableau de data saturation
    data.sat.tab[i,c(1:2)]=c(as.character(ind.temp),as.numeric(new.item.count))
    
    #identification des items cites par l'individu i
    items.to.del=which(temp2[,ind.temp]==1)
    
    #suppression de la matrice resp/item abs pres de l'individu i et des taxons qu'il a cite
    temp2=temp2[-items.to.del,-which(colnames(temp2)==ind.temp)]
    i=i+1
  }
  return(data.sat.tab)
}