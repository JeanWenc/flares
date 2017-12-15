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

###########Libraries#####
library(ade4)
library(adegraphics)
library(ca)
library(dendextend)
library(data.table)
library(DT)
library(leaflet)
library(ggdendro)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(reshape2)
library(shiny)
library(sp)
library(vegan)

############source functions####
source("scripts/Users/check_email.R")

source("scripts/Upload/check_fl_data.R")
source("scripts/Upload/ANTHROPAC_format.R")
source("scripts/Upload/check_apac_categories.R")
source("scripts/Upload/my_read_fn.R")

source("scripts/Normalisation/check_categ_in_norm.R")

source("scripts/Free-list Analysis/Item_by_Respondent_Matrix.R")

source("scripts/Free-list Analysis/FL_Analysis.R")
source("scripts/Free-list Analysis/Plot_FL_Analysis_Chart.R")

source("scripts/Free-list Analysis/Item_Prox_Mat.R")
source("scripts/Free-list Analysis/Plot_Item_Prox.R")
source("scripts/Free-list Analysis/fn_best_tree.R")
source("scripts/Free-list Analysis/plot_dendro.R")

source("scripts/Free-list Analysis/categ_clustering.R")
source("scripts/Free-list Analysis/Data_Saturation.R")
source("scripts/Free-list Analysis/Plot_Data_Saturation.R")

source("scripts/Respondent/check_resp_var_data.R")
source("scripts/Respondent/Sum_resp_var.R")
source("scripts/Respondent/Resp_Analysis.R")
source("scripts/Respondent/Plot_Resp_Competence.R")
source("scripts/Respondent/FL_Analysis_Resp_Var.R")
source("scripts/Respondent/Plot_FL_Analysis_Chart_Resp_Var.R")
source("scripts/Respondent/Resp_Prox_Fn.R")
source("scripts/Respondent/Plot_Resp_Prox.R")
source("scripts/Respondent/Data_Example_2.R")
#####################################################################################################################
shinyServer(function(input, output,session){

session$onSessionEnded(stopApp)
###############VALUES##############
  values<-reactiveValues()
  values$email.ok<-"None"
  values$click_lat<-0
  values$click_long<-0
  
  users.df<-read.csv2("www/userDB/users.csv",stringsAsFactors = F)
###############REACTIVE DATA#######
  data=reactive({
    
    if(values$email.ok=="None")
      return(NULL)
    
    inFile<-input$Uploaded.file1
    
    if(is.null(inFile))
      return(NULL)
   
    h1<-TRUE
    apac<-input$input.format.choice
    if(apac=="APAC") h1<-input$header
    data1<-myReadFn(inFile$datapath,h1,input$sep,input$quote,TRUE)
    
    return(check.FL.data(data1,apac,h1))
  })
  
  norm.item.data=reactive({
    inFile3<-input$Uploaded.file3
    
    if(is.null(inFile3))
      return(NULL)
    
    norm.item1<-myReadFn(inFile3$datapath,input$header3,input$sep3,input$quote3,TRUE)
    
    if(dim(norm.item1)[2]<2)
      return(data.frame(WARNING="Your data doesn't seem to be in the correct format. Please check your seperator and quote options in the side-panel on the left."))
    
    return(norm.item1)
  })
  
  res1=reactive({
    if(is.null(data()))
      return(NULL)
    if(data()$check!="OK")
      return(NULL)
    
    apac<-input$input.format.choice
    
    norm.item.data1<-data.frame(empty="")
    go.norm<-"FALSE"
    norm.type<-"None"
    if(!is.null(norm.item.data()) && names(norm.item.data())[1]!="WARNING"){
      norm.item.data1=norm.item.data()
      go.norm=input$apply.normalization
      norm.type=input$norm.type
    }

    return(item.resp.mat(data(),norm.item.data1,go.norm,norm.type,apac))
  })
  
  res.FL=reactive({
    if(is.null(res1()))
      return(NULL)
    FL.analysis(res1()$mat.cit.abs.pres, res1()$mat.cit.rk, res1()$origListLength)
  })
  
  res.sum.FL=reactive({
    if(is.null(res1()))
      return(NULL)
    data.sum=as.data.frame(c(
      res1()$nb.resp, #number of lists
      res1()$nb.item, #number of different items
      sum(res1()$mat.cit.abs.pres), #total number of cited items
      round(sum(res1()$mat.cit.abs.pres)/res1()$nb.resp,1)
    ))
    names(data.sum)=""
    row.names(data.sum)=c("Nb of Respondents","Number of different cited items","Total number of cited items","Average list length")
    return(data.sum)
  })
  
  apac.categories=reactive({
    if(input$input.format.choice=="FLAME")
      return(NULL)
    
    if(input$apac.item.categories==FALSE)
      return(NULL)
    
    if(is.null(res1()))
      return(NULL)
    
    norm.item.data1=data.frame(empty="")
    norm.type="None"
    if(!is.null(norm.item.data()) && names(norm.item.data())[1]!="WARNING"){
      norm.item.data1<-norm.item.data()
      if(input$apply.normalization==TRUE) norm.type=input$norm.type
    }
    
    return(check.apac.categories(res1()$FL.list,norm.item.data1,norm.type))  
  })
  
  norm.categories=reactive({
    if(is.null(norm.item.data()))
      return(NULL)
    if(names(norm.item.data())[1]=="WARNING")
      return(NULL)
    if(is.null(input$categ.type))
      return(NULL)
    if(input$apply.normalization==FALSE)
      return(NULL)
    return(check.categ.in.norm(input$categ.type,input$norm.type,norm.item.data()))
  })
  
  ls.categories=reactive({
 
    ls.categ="None"
    
    if(!is.null(apac.categories())) ls.categ=c(ls.categ,apac.categories()$apac.categ.names)
    
    if(!is.null(norm.item.data()))
      if(names(norm.item.data())[1]!="WARNING")
        if(!is.null(input$categ.type)){
          ls.categ2=input$categ.type
          if(!is.null(norm.categories())){
            if(names(norm.categories()[[1]])[1]!="OK")
              ls.categ2=ls.categ2[which(!ls.categ2%in%norm.categories()[[2]])]
          }
          ls.categ=c(ls.categ,ls.categ2)
        }
    
    if(!is.null(res.FL())){
      if(!is.null(input$tree.partition)){
        if(input$tree.partition==TRUE) ls.categ=c(ls.categ,"tree.cut")   
      }
    }
    return(ls.categ) 
  })
  
  categ.clust<-reactive({
    if(is.null(res1()))
      return(NULL)
    if(length(ls.categories())==1)
      return(NULL)
    norm.type="None"
    if(!is.null(norm.item.data()) && names(norm.item.data())[1]!="WARNING" && input$apply.normalization==TRUE) norm.type=input$norm.type
    
    resp.var.data1="None"
    if(!is.null(resp.var.data()) & is.data.frame(resp.var.data())) resp.var.data1<-resp.var.data()
    
    tree.partition<-"None"
    if(!is.null(item.prox.plot())){
      if(!is.null(input$tree.partition)){
        if(input$tree.partition==TRUE) tree.partition<-item.prox.plot()$partition
      }
    } 
    
    categ.clust1<-categ.clustering(res1()$FL.list,norm.type,ls.categories(), resp.var.data1,tree.partition)
    return(categ.clust1)
  })
  
  data.samples2=reactive({
    if(is.null(res1()))
      return(NULL)
    data.example2(res1()$mat.cit.abs.pres)
  })
  
  item.prox=reactive({
    debug=FALSE
    if(is.null(res.FL()))
      return(NULL)
    withProgress(
      message="Computing prox. matrices",value=0,{
    item.prox.mat(res1()$mat.cit.abs.pres,res1()$mat.cit.rk,res.FL(),debug)
      })
  })
  
  item.prox.plot<-reactive({
    if(is.null(item.prox()))
      return(NULL)
    
    item.prox.plot.type2=input$item.prox.plot.type
    
    item.categ.temp="None"
    if(!is.null(input$item.categories)) item.categ.temp=input$item.categories
    
    norm.col.temp="None"
    norm.item.data=data.frame(empty="")
    if(!is.null(norm.item.data())&& names(norm.item.data())[1]!="WARNING"){
      norm.item.data1=norm.item.data()
      if(input$apply.normalization==TRUE) norm.col.temp=input$norm.type
    }
    
    if(is.null(input$dendo.n)){
      dendo.min1<-0
      dendo.max1<-0
    }else{
      dendo.min1<-input$dendo.n[1]
      dendo.max1<-input$dendo.n[2]
    }
    
    temp.plot<-plot.item.prox(item.prox(),res.FL(),norm.item.data1,apac.categories(),#ls.categories(),
                              input$freq.slider.item.prox,input$item.prox.index,input$resize.item.prox.labels,
                              item.prox.plot.type2,item.categ.temp,norm.col.temp,
                              dendo.min1,dendo.max1)
    
    return(temp.plot)
  })
  
  data.saturation=reactive({
    if(is.null(res1()))
      return(NULL)
    data.sat(res1()$mat.cit.abs.pres)
  })
  
  resp.var.data=reactive({
    inFile2<-input$Uploaded.file2
    
    if(is.null(inFile2)) 
      return(NULL)
    
    if(is.null(res1()))  
      return(NULL)
    
    #resp.var.data1=read.csv(inFile2$datapath,header=input$header2,sep=input$sep2,quote=input$quote2,stringsAsFactors = FALSE,na.strings = c("NA",""))
    resp.var.data1<-myReadFn(inFile2$datapath,input$header2,input$sep2,input$quote2,FALSE)
    
    res.temp=check.resp.var.data(resp.var.data1,res1()$mat.cit.abs.pres)
    
    return(res.temp)
  })
  
  res.resp=reactive({
    if(is.null(res1()))
      return(NULL)
    res.resp1=resp.analysis(res1()$mat.cit.abs.pres,res1()$mat.cit.rk)
    if(!is.null(resp.var.data()) & is.data.frame(resp.var.data())){
      resp.var.data1=resp.var.data()
      res.resp1=merge(res.resp1,resp.var.data1,by.x=0,by.y=1,all.x=TRUE)
      row.names(res.resp1)=res.resp1[,1]
      res.resp1=res.resp1[,-1]
      res.resp1=res.resp1[sort(row.names(res.resp1)),]
    }
    return(res.resp1)
  })
  
  #resp.prox() is a list object containing:
  #- [[1]] named: "resp.mat.dist": respondent by respondent distance matrix (using Jaccard)
  #- [[2]] named: "res.dispersion": inter-group homogeneity of intra-group dispersion (if no respondent variable data (i.e. resp.var.data())) the data.frame is empty ([1,1]=="")
  #- [[3]] named: "res.aonis": between class analysis of variance (if no respondent variable data (i.e. resp.var.data())) the data.frame is empty ([1,1]=="")
  resp.prox=reactive({
    #Objects res1() and res.FL() need to have been created
    if(is.null(res1()) | is.null(res.FL()))
      return(NULL)
    
    resp.var.data1=as.data.frame("")
    
    #if user has uploaded respondent variable data table use that data table
    if(!is.null(resp.var.data()) & is.data.frame(resp.var.data())) resp.var.data1=resp.var.data()
      
    resp.prox1=resp.prox.fn(res1()$mat.cit.abs.pres,res.FL(),input$select.freq.slider3,resp.var.data1)
    
    return(resp.prox1)
  })
  
  res.sum.resp.var=reactive({
    if(is.null(resp.var.data()))
      return(NULL)
    if(!is.data.frame(resp.var.data()))
      return(NULL)
    sum.resp.var(resp.var.data())
  })
  
  res.FL.resp.var=reactive({
    if(is.null(resp.var.data()))
       return(NULL)
    if(!is.data.frame(resp.var.data()))
      return(NULL)
    if(input$resp.var.name=="No Variable")
      return(NULL)
    FL.analysis.resp.var(resp.var.data(),res1()$mat.cit.abs.pres,res1()$mat.cit.rk,input$resp.var.name)
  })
  
###############CONDITIONS#####
  output$enable_UI_main<-reactive({
    return(values$email.ok!="None")
  })
  outputOptions(output,'enable_UI_main', suspendWhenHidden=FALSE)
  
  output$enable_UI <- reactive({
      if(!is.null(data()))
        return(data()$check=="OK")  
    })
  outputOptions(output, 'enable_UI', suspendWhenHidden=FALSE)
  
  output$enable_UI2 <- reactive({
    return(!is.null(resp.var.data()) && is.data.frame(resp.var.data()) && input$resp.var.name!= "No Variable")
  })
  outputOptions(output, 'enable_UI2', suspendWhenHidden=FALSE)
  
  output$enable_UI3 <- reactive({
    return(!is.null(norm.item.data()) && names(norm.item.data())[1]!="WARNING")
  })
  outputOptions(output, 'enable_UI3', suspendWhenHidden=FALSE)
  
  output$enable_UI4<-reactive({
    return(!is.null(resp.var.data()) && is.data.frame(resp.var.data()))
  })
  outputOptions(output, 'enable_UI4',suspendWhenHidden=FALSE)
  
###############OUTPUTS######
  source(file = "scripts/server/srv_tab_1_intro.R",local=TRUE)$value
  
  source(file="scripts/server/srv_tab_2_upload.R",local=TRUE)$value
  
  source(file="scripts/server/srv_tab_3_norm.R",local=TRUE)$value
  
  source(file="scripts/server/srv_tab_4_item_analyses.R",local=TRUE)$value
})