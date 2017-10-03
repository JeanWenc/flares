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

setwd("C:/00_Work/03_FLAME/03_FLARES/Debug")
rm(list=ls())

library(shiny)
library(ade4)
library(FactoMineR)
library(vegan)
library(reshape2)
library(DT)
library(dendextend)
library(igraph)
library(data.table)
library(ggplot2)
library(ggdendro)
library(ggnetwork)
library(network)
library(sna)
library(RColorBrewer)
library(reshape2)
library(adegraphics)

source("C:/00_Work/03_FLAME/03_FLARES/FLARES/scripts/Respondent/Data_Example_2.R")

##############Les jeux de données####################
data=read.csv2("debug data/Djaba_Anim.csv",h=T)
data=read.csv2("debug data/Dummy dataset/1_Upload tab_ANTHROPAC Format.csv",h=T,sep=",",na.strings = c("NA",""))
data=read.csv2("C:/Users/jeanw/Desktop/Robbins & nolan/dichot_bias.csv",h=T)
data=read.csv2("C:/00_Work/04_Projets/PIIAFF/00_Données/FL_CAM/06_2017 - NelAnalyses_NelNorma/Animals/Anim_all.csv",h=T)
data=read.csv2("C:/Users/jeanw/Desktop/FLARES - PIAF/Dummy dataset/1_Upload tab_ANTHROPAC Format.csv",h=F)
data=read.csv2("C:/Users/jeanw/Desktop/Papier FLARES/FL_Oiseaux_Nanterre/fakedata.csv",h=T)
###############Vérif fichier upload###########################
#APAC ou non
source("C:/00_Work/03_FLAME/03_FLARES/FLARES/scripts/Upload/check_fl_data.R")
source("C:/00_Work/03_FLAME/03_FLARES/FLARES/scripts/Upload/ANTHROPAC_format.R")
apac="APAC"#or FLAME or ATOOLS
up.data=check.FL.data(data1 = data,apac=apac,header = F) #fonction qui utilise anthropac_format comme sous-fonction

############################NORMALISATION########################""
#Jeu de données normalisation
norm.item.data=read.csv2("debug data/Djaba_Anim_Norm.csv",h=T)
norm.item.data=read.csv2("debug data/Dummy dataset/2_Normalize Data tab_Norm2.csv",h=T,sep=";")
norm.item.data=read.csv2("C:/00_Work/04_Projets/PIIAFF/00_Données/FL_CAM/06_2017 - NelAnalyses_NelNorma/Animals/Anim_Norm.csv",h=T)
norm.item.data=read.csv2("C:/Users/jeanw/Desktop/FLARES - PIAF/Dummy dataset/2_Normalize Data tab_Norm2.csv",h=T)
norm.item.data=read.csv2("C:/Users/jeanw/Desktop/Papier FLARES/FL_Oiseaux_Nanterre/List_of_cited_items.csv",h=T)
#Chosir la colonne de normalisation
norm.type=names(norm.item.data)[3]

#######################Transfor UP.DATA en matrice rectangulaire########################
#Créer les matrices items x individus (présence/absence et Rank)
source("C:/00_Work/03_FLAME/03_FLARES/FLARES/scripts/Free-list Analysis/Item_by_Respondent_Matrix.R")
apac="APAC"#or FLAME or APAC
res1=item.resp.mat(up.data,norm.item.data = data.frame(empty=""),go.norm=FALSE,norm.type.col=2,apac=apac)
res1=item.resp.mat(up.data,norm.item.data,go.norm=TRUE,norm.type.col=norm.type,apac=apac)

############################Fichier informateur et vérification########################

#si pas de resp.var.data
resp.var.data=as.data.frame("")

#si resp.var.data
resp.var.data2=read.csv("debug data/Djaba_0Class_Info.csv",header=T,sep=";",stringsAsFactors = FALSE,na.strings = c("NA",""))
resp.var.data2=read.csv2("debug data/Dummy dataset/3_Respond Variables tab.csv",sep=",",h=T,stringsAsFactors=FALSE, na.strings=c("NA",""))
resp.var.data2=read.csv2("C:/Users/jeanw/Desktop/Robbins & nolan/resp.csv",stringsAsFactors=F,na.strings=c("NA",""))
resp.var.data2=read.csv2("C:/00_Work/04_Projets/PIIAFF/00_Données/FL_CAM/06_2017 - NelAnalyses_NelNorma/Class_info2.csv",stringsAsFactors = FALSE,na.strings=c("NA",""))

source("scripts/Respondent/check_resp_var_data.R")
resp.var.data<-check.resp.var.data(resp.var.data2,res1$mat.cit.abs.pres)

#######################Analyses FL########################

#Créer les résultats FL (fréquence, sutrop etc...)
source("C:/00_Work/03_FLAME/03_FLARES/FLARES/scripts/Free-list Analysis/FL_Analysis.R")
res.FL=FL.analysis(res1$mat.cit.abs.pres,res1$mat.cit.rk,res1$origListLength)

#Graph des résultats FL
salience.ind.checkbox2="freq.cit.rel" #"freq.cit.rel","Smith.index","Sutrop.index"
sort2=2#0=Alpha;2=Freq.cit;5=Smith;6=Sutrop
text.size=8
source("scripts/Free-list Analysis/Plot_FL_Analysis_Chart.R")
plot.fl.analysis.chart(res.FL,salience.ind.checkbox2,sort2,10,100,text.size)

#Créer le résumé des résultats
data.sum=as.data.frame(c(
  res1$nb.resp, #number of lists
  dim(res1$mat.cit.abs.pres)[1], #number of different items
  sum(res1$mat.cit.abs.pres), #total number of cited items
  round(sum(res1$mat.cit.abs.pres)/res1$nb.resp,1)
))
names(data.sum)=""
row.names(data.sum)=c("Nb of Respondents","Number of different cited items","Total number of cited items","Average list length")

#######################Catégories########################

#Récupérer les catégories APAC et créer tableau de distribution + tableau moda max
source("C:/00_Work/03_FLAME/03_FLARES/FLARES/scripts/Upload/check_apac_categories.R")
norm.item.data1=data.frame(empty="")
norm.type2="None"
#OR
norm.type2<-norm.type
norm.item.data1<-norm.item.data

apac.categories<-check.apac.categories(res1$FL.list,norm.item.data1,norm.type2)

#Vérifier la cohérence des catégories uploadé dans norm.item.data en fonction de la colonne de normalisation choisie
norm.category.names2=names(norm.item.data)[c(5,6)]
source("C:/00_Work/03_FLAME/03_FLARES/FLARES/scripts/Normalisation/check_categ_in_norm.R")
norm.categories<-check.categ.in.norm(norm.category.names2,norm.type2,norm.item.data)
norm.category.names2<-norm.category.names2[which(!norm.category.names2%in%norm.categories$ls.pb.categ)]

#créer la liste de toutes les catégories
apac.category.names2=apac.categories$apac.categ.names
ls.categories=c("None",apac.category.names2)
ls.categories=c("None","tree.cut")
ls.categories=c("None",norm.category.names2,"tree.cut")
ls.categories=c("None",apac.category.names2,"tree.cut")
ls.categories=c("None",apac.category.names2,norm.category.names2,"tree.cut")

#analyses de clustering
source("C:/00_Work/03_FLAME/03_FLARES/FLARES/scripts/Free-list Analysis/categ_clustering.R")
tree.partition<-"None"#or run plot_item_prox first
if (resp.var.data[1,1]==""){
  resp.var.data1="None"
} else{
  resp.var.data1=resp.var.data
}
categ.clust<-categ.clustering(res1$FL.list,norm.type = norm.type2,ls.categories,resp.var.data1,tree.partition)

#analyse patch flow
source("scripts/Free-list Analysis/patch_flow.R")
tree.partition<-"None"# or run plot_item_prox first
min.max<-"min"#or "max"
debug<-TRUE
patch.flow1<-patch.flow(res1$FL.list,norm.type = norm.type2,ls.categories,resp.var.data,tree.partition,min.max,debug)

#plot patch flow
source("scripts/Free-list Analysis/plot_clust_net.R")
categ.for.netw1="tree.cut"
deg.ch='meanFreq'
plot.clust.net(patch.flow1,
               e.width.mult = 1,v.size.mult = 5,arrow.size=0.5,
               ls.categories = ls.categories,categ.for.netw = categ.for.netw1,
               with.items = FALSE,deg.ch,e.curve = 0.1,freq=c(0,100),res.FL)
#######################Item by Item Proximity########################

#Créer les matrices de proximité
source("C:/00_Work/03_FLAME/03_FLARES/FLARES/scripts/Free-list Analysis/Item_Prox_Mat.R")
item.prox=item.prox.mat(res1$mat.cit.abs.pres,res1$mat.cit.rk,res.FL,debug=TRUE)

#Plot de la proximité item par item
source("C:/00_Work/03_FLAME/03_FLARES/FLARES/scripts/Free-list Analysis/Plot_Item_Prox.R")
source("C:/00_Work/03_FLAME/03_FLARES/FLARES/scripts/Free-list Analysis/fn_best_tree.R")
source("C:/00_Work/03_FLAME/03_FLARES/FLARES/scripts/Free-list Analysis/plot_dendro.R")
item.prox.plot.type2=2 #Dendogram 1; MDS 2
item.prox.index="Successive count"#"Henley index" ou "Successive count"
label.size=7
freq=c(0,100)
item.categ.temp<-"None"
dendo.min=3
dendo.max=10
plot<-plot.item.prox(item.prox,res.FL,norm.item.data,apac.categories,#ls.categories,
                     freq,item.prox.index,label.size,
                     item.prox.plot.type2,item.categ.temp,norm.type,
                     dendo.min,dendo.max)  

tree.partition<-plot$partition
#######################Data Saturation########################

#DataStauration
source("scripts/Free-list Analysis/Data_Saturation.R")
data.saturation=data.sat(res1$mat.cit.abs.pres)

#Figure pour DataSaturation
source("scripts/Free-list Analysis/Plot_Data_Saturation.R")
plot.data.saturation(data.saturation)

############Résultats sur les informateurs########################
#Résumé des classes d'informateurs
source("scripts/Respondent/Sum_resp_var.R")
res.sum.resp.var=sum.resp.var(resp.var.data)

#Respondent Analysis
source("scripts/Respondent/Resp_Analysis.R")
#Sans info sur les informateurs
res.resp=resp.analysis(res1$mat.cit.abs.pres,res1$mat.cit.rk)
#Avec info sur les informateurs
#voir plus bas

#Plot de la compétence informateurs
source("scripts/Respondent/Plot_Resp_Competence.R")
resp.var.name1="No Variable"#or....

#Mise à jour des analyses sur informateurs avec les variables informateurs
res.resp=merge(res.resp,resp.var.data,by.x=0,by.y=1,all.x=TRUE)
row.names(res.resp)=res.resp[,1]
res.resp=res.resp[,-1]
res.resp=res.resp[sort(row.names(res.resp)),]

resp.var.name1=names(resp.var.data)[2]
plot.res.comp(res.resp,resp.var.name1)

#Analyses FL par classe d'informateurs
source("scripts/Respondent/FL_Analysis_Resp_Var.R")
#Choisir la variable informateur
res.FL.resp.var=FL.analysis.resp.var(resp.var.data,res1$mat.cit.abs.pres,res1$mat.cit.rk,resp.var.name1)

#Plot des analyses FL par classe d'informateurs
source("scripts/Respondent/Plot_FL_Analysis_Chart_Resp_Var.R")
select.sal.index2="Sutrop"#Frequency or Smith or Sutrop
resp.var.name.mod2=as.character(unique(resp.var.data[,names(resp.var.data)[2]]))[1]#Sélectionner la modalité de tri
freq=c(0,100)
text.size=8
plot.fl.analysis.chart.resp.var(res.FL.resp.var,select.sal.index2,freq,resp.var.name.mod2,res.FL,resp.var.name1,text.size)


#Respondent Proximity
source("scripts/Respondent/Resp_Prox_Fn.R")
resp.prox=resp.prox.fn(res1$mat.cit.abs.pres,res.FL,c(0,100),resp.var.data)
#Pour voir la matrice de proximité inf par inf (attention il faudrait nommer les objets dans la liste resp.prox)
resp.prox[[2]]

#Plot de la proximité inf par inf (sans les informations variables informateurs)
source("scripts/Respondent/Plot_Resp_Prox.R")
resp.var.name1="No Variable"
resp.var.name1="Age"
freq2<-c(0,100)
p<-plot.resp.prox(resp.prox,resp.var.name1,freq2)
p
#Plot de proximité informateurs par informateurs avec info variables informateurs
resp.var.data1=resp.var.data
resp.var.name1=names(resp.var.data)[2]
plot.resp.prox(resp.prox,resp.var="Age")
