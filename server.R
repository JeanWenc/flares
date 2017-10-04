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
library(ggdendro)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(reshape2)
library(shiny)
library(vegan)

############source functions####

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

###############REACTIVE DATA#######
  data=reactive({
    inFile<-input$Uploaded.file1
    
    if(is.null(inFile))
      return(NULL)
   
    h1<-TRUE
    apac<-input$input.format.choice
    if(apac=="APAC") h1<-input$header
    data1<-myReadFn(inFile$datapath,h1,input$sep,input$quote,TRUE)
    #read.csv(inFile$datapath,header=h1,sep=input$sep,quote=input$quote)
    
    return(check.FL.data(data1,apac,h1))
  })
  
  norm.item.data=reactive({
    inFile3<-input$Uploaded.file3
    
    if(is.null(inFile3))
      return(NULL)
    
    norm.item1<-myReadFn(inFile3$datapath,input$header3,input$sep3,input$quote3,TRUE)
    #norm.item1=read.csv(inFile3$datapath,header=input$header3,sep=input$sep3,quote=input$quote3)
    
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
  ###################################################################################################################
  #####################TabPanel(UPLOAD)#####
                      ####SideBarPanel#####
  output$check.header<-renderUI({
    if(input$input.format.choice=="APAC")
      checkboxInput("header",label="My data contains a header",value = TRUE)
    #if(input$apac.format.checkbox==TRUE)
    #  checkboxInput("header",label="My data contains a header",value = TRUE)
  })
  
  output$panel2.sidebar.text1<-renderUI({
    if(input$input.format.choice=="APAC")
    #if(input$apac.format.checkbox==TRUE)
      HTML("Tick this checkbox if the first line of your data table is NOT a respondent ID.")
  })
  
  output$check.apac.item.categories<-renderUI({
    if(input$input.format.choice%in%c("APAC","ATOOLS"))
      checkboxInput("apac.item.categories",label= "My uploaded file includes categorical information concerning mentioned items")
    #if(input$apac.format.checkbox==TRUE)
      #checkboxInput("apac.item.categories",label= "My ANTHROPAC formatted file includes categorical information concerning mentioned items")
  })
  
  output$panel2.sidebar.text2<-renderUI({
    if(input$input.format.choice%in%c("APAC","ATOOLS"))
      HTML(paste("Check the above box only if you have - in the column or columns adjacent to the one containing the cited items - information concerning each of the cited items.",
                 "Please go to the 'Data Format Example' sub-tab for more details.",
                 sep="<br/>"))
    #if(input$apac.format.checkbox==TRUE)
      #HTML(paste("Check the above box only if you have - in the column or columns adjacent to the one containing the cited items - information concerning each of the cited items.",
      #           "Please go to the 'ANTHROPAC Data Format Example' sub-tab for more details.",
      #           sep="<br/>"))
  })
  
  #####################TabPanel(UPLOAD)
                      ####TabsetPanel(UPLOADED DATA)####
  output$show.FL.data1<-renderTable({
    if(is.null(data()))
      return(NULL)
    if(data()$check!="OK")
      return(NULL)
    data()$FLAME_format
  },include.rownames=F,bordered = TRUE,striped = TRUE,na = "")
  
  output$error.mess1<-renderUI({
    if(is.null(data()))
      return(NULL)
    if(data()$check=="OK")
      return(NULL)
    str1<-"<span style='color:rgb(188,46,46);'><strong style='font-size:20px'>WARNING</strong><br/>"
    str2<-"<strong>Your uploaded data doesn't seem to be in the correct format.</strong><br/>"
    if(data()$check=="NOT_APAC" | data()$check=="NOT_ATOOLS"){
      str3<-"<i>Please refer to data format presented below.</i><br/><br/></span>"
      res=paste(str1,str2,str3,sep="")
    }
    if(data()$check=="NOT_FLAME"){
      str3<-"<i>Please check your choice of seperators & quotes and refer to the data format example below.<br/>"
      str4<-"OR<br/>"
      str5<-"Your data may be in the Spreadsheet or ANTHROPAC format (if this is the case, please select the correct format in the drop-down list located in the sidebar panel on the left).</i></span><br/><br/>"
      res=paste(str1,str2,str3,str4,str5,sep='')
    }
    if(data()$check=="APAC_HEADER"){
      str3<-"<strong>Are you sure your uploaded data contains a header?</strong><br/>"
      str4<-"The first line of your uploaded data contains a hashtag (#) which indicates a respondent ID.<br/>"
      str5<-"You may want to uncheck the 'header' checkbox in the sidepanel on the left.</span><br/>"
      res=paste(str1,str2,str3,str4,str5,sep='')
    }
    return(HTML(res))
  })
  
  output$duplicates<-renderUI({
    if(is.null(data()))
      return(NULL)
    if(data()$check!="OK")
      return(NULL)
    str<-ifelse(res1()$warn.doub[1]==0,"No pair of duplicates was found in your dataset.",paste("<b>",res1()$warn.doub[1],"</b>",sep=""))
    if(res1()$warn.doub[1]>0){
      str=c(str,ifelse(res1()$warn.doub[1]==1,
                       "<b>pair of duplicates was found in your dataset.</b><br/>",
                       "pairs of duplicates were found in your datasest.<br/>"))
      str=c(str,"Second occurences of each pair will not be taken into account in further analyses.<br/><br/>")
      str=c(str,ifelse(res1()$warn.doub[1]==1,
                       "Duplicates appear in the list of the following respondent:<br/>",
                       "Duplicates appear in the lists of the following respondents:<br/>"))
      str=c(str,paste("<b>",paste(res1()$list.resp.doub,collapse=", "),"</b>",sep=""))
    }
    return(HTML(str))
  })
  
  output$show.sample1<-renderTable({
    if(!is.null(data()) && data()$check=="OK")
      return(NULL)
    if (input$input.format.choice=="FLAME"){
      read.csv2("data/Upload_Data_Format_Exple.csv",h=T)
    }else if(input$input.format.choice=="APAC"){
      read.csv2("data/Upload_APAC_Data_Format_Exple.csv",h=T)
    }else{
      read.csv2("data/Upload_ATOOLS_Data_Format_Exple.csv",h=T)
    }
  },include.rownames=F,striped = T,bordered = T,align = c)
  
  output$show.sample.text<-renderUI({
    if(!is.null(data()) && data()$check=="OK")
      return(NULL)
    temp<-read.csv2("data/data_format_text.csv",stringsAsFactors = F,h=T,col.names = c("FLAME","APAC","ATOOLS"))
    mess<-temp[1,input$input.format.choice]
    return(HTML(mess))
  })
  
  #####################TabPanel(UPLOAD)
                      ####TabsetPanel(ITEM CATEGORICAL INFORMATION)####
  output$panel2.tab2.text1<-renderUI({
    err="<b>Results are displayed only if you upload item categorical information, which is only possible in the Spreadsheet and ANTHROPAC formats.</b>"
    ref="<i>Please browse through the drop-down list in the sidebar panel and then the Data Format Examples provided in the 'Uploading Data' sub-tab for more details.</i>"
    err1=paste(err,ref,sep="<br/><br/>")
    err2=paste(err,
               "<b>AND</b>",
               "<b>If you tick the checkbox (in the lower part of the left sidepanel) indicating that you have uploaded categorical information.</b>",
               ref,
               sep="<br/><br/>")
    
    if(input$input.format.choice=="FLAME")
      return(HTML(err1))
    
    if(input$apac.item.categories==FALSE)
      return(HTML(err2))
    
    if(is.null(apac.categories()))
      return(HTML("<b>You have not uploaded item categorical information with your uploaded file.</b>"))
    
    return(HTML(paste("<b>The table shown below indicates, for each item, the number of informants who assigned it to each of your defined categories.</b>",
                "<i>You may download the table using the download button on the right.</i>",
                sep="<br/><br/>")))
  })
  
  output$show.download.apac.categ.tab<-renderUI({
    if(!is.null(apac.categories()))
      return(downloadButton("download.apac.categ.tab",label = "Download"))
  })
  
  output$show.csvtype6<-renderUI({
    if(!is.null(apac.categories()))
      radioButtons('csvtype6','Choose CVS Format',
                   c("Semicolon (French)"=2,
                     "Comma (English)"=1))
  })
  
  output$download.apac.categ.tab<-downloadHandler(
    filename = "Distribution_of_items_by_categories.csv",
    content=function(file){
      to.dwnl=apac.categories()$item.distrib
      if(input$csvtype6==1) write.csv(to.dwnl,file,row.names = F)
      if(input$csvtype6==2) write.csv2(to.dwnl,file,row.names=F)
    })
  
  output$show.apac.categ.tab<-DT::renderDataTable({
    if(!is.null(apac.categories()))
      return(datatable(apac.categories()$item.distrib,options=list(lengthMenu=list(c(10,20,-1),c('10','20','All')),
                                                                   pageLength=20)))
  })
  
 
  ###################################################################################################################
  #####################TabPanel(NORMALIZE DATA)####
                      ####Sibebar####      
  output$panel3.sidebar.text1<-renderText({
    if(is.null(res.FL()))
      return("Fucntions will be available once you have uploaded your data.")
  })
  
  #####################TabPanel(NORMALIZE DATA)
                      ####TabsetPanel(LIST of CITED ITEMS)####
  output$show.cited.items.list<-DT::renderDataTable({
    if(is.null(norm.item.data())){
      cited.items.list=data.frame(res.FL()[,1],rep("",dim(res.FL())[1]))
      names(cited.items.list)=c("Cited Items","Normalized name")
      return(datatable(cited.items.list,options=list(lengthMenu=list(c(10,20,-1),c('10','20','All')),
                                                     pageLength=20)))  
    }
    
    if(names(norm.item.data())[1]!="WARNING")
      return(datatable(norm.item.data(),
                       options = list(lengthMenu=list(c(10,20,-1),c('10','20','All')),
                                      pageLength=20)))
  })
  
  output$panel3.tab1.text2<-renderUI({
    if(is.null(norm.item.data()))
      HTML("<b>Download list of cited items</b>")
  })
  
  output$show.download.show.cited.items.list<-renderUI({
    if(is.null(norm.item.data()))
      downloadButton('download.show.cited.items.list', 'Download')
  })
  
  output$show.csvtype1<-renderUI({
    if(is.null(norm.item.data()))
      radioButtons('csvtype1','Choose CVS Format',
                   c("Semicolon (French)"=2,
                     "Comma (English)"=1))
  })
  
  output$download.show.cited.items.list<-downloadHandler(
    filename = "List_of_cited_items.csv",
    content=function(file){
      cited.items.list=data.frame(res.FL()[,1],rep("",dim(res.FL())[1]))
      names(cited.items.list)=c("Cited Items","Normalized name")
      if(input$csvtype1==1) write.csv(cited.items.list,file,row.names = F)
      if(input$csvtype1==2) write.csv2(cited.items.list,file,row.names=F)
    })
  
  output$panel3.tab1.text1<-renderUI({
    if(is.null(norm.item.data())){
      str<-"<b>The list shown below contains, in alphabetical order, all of the different cited items.</b><br/>"
      str<-c(str,"For normalization or categorization purposes you may download the list below (by clicking on the download button located below on the right-hand side of the table) and fill as many columns as you wish (each with a different header).<br/>")
      str<-c(str,"<em>This may be useful to correct mispellings, harmonize synonyms or even translate the items as typed-in in your original free-list datasheet.</em><br/><br/>")
      str<-c(str,"You can either create columns for <b>normalization</b> (analyses will run on those normalized names) or for <b>categorization</b> (freelist analyses will not run on the categories but category clustering analyses will be made available).<br/><br/>")
      str<-c(str,"<b>Do not modify content of the first column ('Cited Items').</b><br/><br/>")
      str<-c(str,"<i>Refer to the 'Data Format Example sub-tab to see how your file should be formatted.</i><br/><br/>")
    
      str<-c(str,"<b>You may then upload the list you have downloaded and filled-in with the upload sidepanel on the left.</b>")
     
      if(input$input.format.choice%in%c("APAC","ATOOLS"))
        if(input$apac.item.categories==TRUE){
          str<-c(str,"<br/><br/><b>N.B. You have uploaded (with your ANTHROPAC formatted file) item categorical information. This information will be taken into account even if you don't upload any file here.</b>")
          }
      
      return(HTML(str))
      }
    
    if(names(norm.item.data())[1]=="WARNING")
      return(HTML(as.character(norm.item.data()[1,1])))
    
    all.col=colnames(norm.item.data())[-1]
    norm.col=all.col[!all.col%in%input$categ.type]
    if(length(norm.col)==0) return(HTML("<b>You haven't uploaded any normalization information.</b>"))
    
    if(input$apply.normalization==FALSE){
      str1=""
      ls.missing=res.FL()[,1][which(!res.FL()[,1]%in%norm.item.data()[,1])]
      if(length(ls.missing)==1){
        str1<-paste(
          "The following item is present in your free-lists but absent from your normalization table: <br/>",
          "<b>",
          ls.missing,
          ".</b><br/><br/>",
          "It would be preferable, to avoid glitches, that you add this item to your datasheet and upload it once again.",
          sep="")
      }
      if(length(ls.missing)>1){
        str1<-paste(
          "The following items are present in your free-lists but absent from your normalization table: <br/>",
          "<b>",
          paste(ls.missing,collapse=", "),
          ".</b><br/><br/>",
          "It would be preferable, to avoid glitches, that you add these items to your datasheet and upload it once again.",
          sep="")
      }
      return(HTML(str1))
    }
    
    if(input$apply.normalization==TRUE){
      n.orig.item=length(unique(norm.item.data()[,1]))
      n.norm.item=length(unique(norm.item.data()[,input$norm.type]))
      str2<-paste("<b>",
                  paste("After normalization number of items has gone from ",
                        n.orig.item,
                        " to ",
                        n.norm.item,
                        sep=""),
                  "</b><br/><br/>",
                  "<b>Applying normalization resets all analyses.</b>")
      return(HTML(str2))
    }
  })
  
  
  #displays under conditions elicited in ouptut$disable_UI3 (above)
  output$select.categ.type<-renderUI({
    checkboxGroupInput("categ.type", 
                       label = "Select which of these columns contain item categorical information (if any)?", 
                       choices = c(colnames(norm.item.data()[-1])),
                       selected = NULL)
  })
  
  #displays under conditions elicited in ouptut$disable_UI3 (above)
  output$select.norm.type<-renderUI({
    all.col=colnames(norm.item.data())[-1]
    norm.col=all.col[!all.col%in%input$categ.type]
    if(length(norm.col)!=0) return(selectInput("norm.type","Select the normalized column you wish to use.",c(norm.col)))
  })
  
  #displays under conditions elicited in ouptut$disable_UI3 (above)
  output$select.apply.normalization<-renderUI({
    all.col=colnames(norm.item.data())[-1]
    norm.col=all.col[!all.col%in%input$categ.type]
    if(length(norm.col)!=0) checkboxInput('apply.normalization','Apply uploaded normalization',FALSE)
  })
  
  #displays under conditions elicited in ouptut$disable_UI3 (above)
  output$categ.warning<-renderUI({
    if(is.null(input$categ.type))
      return(NULL)
    if(!is.null(norm.categories())){
      if(names(norm.categories()[[1]])[1]!="OK")
        return(HTML(paste("<b>All of your categorical variables cannot be used because for some of them (printed below): </b>",
                          paste(norm.categories()[[2]],collapse=", ",sep=""),
                          "<b>at least one item (with the normalization you have chosen) belongs to different categories.</b><br/>",
                          "The other categorical variables (if any) will be used and all other freelist analyses are available.<br/>",
                          "<i>A data table indicating the problematic items can be downloaded below.</i><br/>",
                          sep="<br/>")
                    ))
    }
    return(NULL)
  })
  
  #displays under conditions elicited in ouptut$disable_UI3 (above)
  output$show.download.pb.items.tab<-renderUI({
    if(!is.null(norm.categories()))
      if(names(norm.categories()[[1]])[1]!="OK")
        return(downloadButton('download.pb.items.tab','Download'))
  })
  
  #displays under conditions elicited in ouptut$disable_UI3 (above)
  output$show.csvtype5<-renderUI({
    #if(!is.null(res1()) & is.null(norm.item.data()))
    if(!is.null(norm.categories()))
      if(names(norm.categories()[[1]])[1]!="OK")
      radioButtons('csvtype5','Choose CVS Format',
                   c("Semicolon (French)"=2,
                     "Comma (English)"=1))
  })
  
  output$download.pb.items.tab<-downloadHandler(
    filename = "List_of_items_with_several_categories.csv",
    content=function(file){
      to.dwnl=norm.categories()[[1]]
      if(input$csvtype5==1) write.csv(to.dwnl,file,row.names = F)
      if(input$csvtype5==2) write.csv2(to.dwnl,file,row.names=F)
    })
 
  #displays under conditions elicited in ouptut$disable_UI3 (above)
  output$panel3.tab1.text3<-renderUI({
    if(!is.null(apac.categories()))
      HTML(paste("<br/><br/><b>You have inputted item categorical information in your uploaded ANTHROPAC formatted file.</b>",
                 "The name(s) of the colum(s) you have uploaded are:",
                 paste("<b>",paste(apac.categories()$apac.categ.names,collapse=", ",sep=""),"<b><br/>",sep=""),
                 "By default, data from these columns will be used for category clustering analyses.<br/>",
                 sep="<br/>"))
  })
  
  
  #####################TabPanel(NORMALIZE DATA)
                      ####TabsetPanel(DATA FORMAT EXAMPLE)####
  output$show.sample4<-renderTable({
    if(is.null(res1()))
      return(NULL)
    return(read.csv2("data/Norm_Data_Format_Exple.csv",h=T))
  },include.rownames=F,align=c,bordered = T,striped=T)
  
  
  
  ###################################################################################################################
  #####################TabPanel(ITEM ANALYSIS)
                      ####SIDEBARPANEL####
  output$fl.analysis.sum<-renderUI({
    if(is.null(res.sum.FL()))
      return(NULL)
    htmlTable=character()
    for(i in 1:dim(res.sum.FL())[1]){
      htmlTable=paste(htmlTable,paste("<span style='margin-right:10px'><strong>",row.names(res.sum.FL())[i],": </strong>",sep=""),sep="")
      htmlTable=paste(htmlTable,paste(as.character(res.sum.FL()[i,1]),"</span><br/>",sep=""),sep="")
      paste(htmlTable)
    }
    return(HTML(htmlTable))
  })
  
  output$show.download.item.resp.matrix<-renderUI({
    if(is.null(res1()))
      return(NULL)
    downloadButton("download.item.resp.matrix",label="Download")
  })
  
  output$show.csvtype.item.resp.mat<-renderUI({
    if(is.null(res1()))
      return(NULL)
    return(radioButtons('csvtype.item.resp.mat','Choose CVS Format',
                        c("Semicolon (FR)"=2,
                          "Comma (UK/US)"=1),inline = T))
  })
  
  output$show.select.item.resp.matrix<-renderUI({
    if(is.null(res1()))
      return(NULL)
    radioButtons("select.item.resp.matrix","Type of matrix",
                 list("Presence/Absence"="pres","Rank"="rk"),inline = T)
  })
  
  output$download.item.resp.matrix<-downloadHandler(
    filename = "Item_Resp_Mat.csv",
    content=function(file){
      if(input$select.item.resp.matrix=="pres"){
        obj2dwnl<-res1()$mat.cit.abs.pres
      }else{
        obj2dwnl<-res1()$mat.cit.rk
      }
      if(input$csvtype.item.resp.mat==1) write.csv(obj2dwnl,file,row.names = T)
      if(input$csvtype.item.resp.mat==2) write.csv2(obj2dwnl,file,row.names=T)
    })
  
  #####################TabPanel(FREE-LIST ANALYSIS)
                      ####TabsetPanel(CULT. SALIENCY)####
                          ####TabPAnel (TABLE)####
  output$panel2.text1<-renderText({
    if(is.null(res.FL()))
      return("Analyses will appear once you have uploaded your data.")
    #paste("")
  })
  
  output$fl.analysis.table<-DT::renderDataTable({
    if(is.null(res.FL()))
      return(NULL)
    res.FL1=res.FL()
    colnames(res.FL1)=c("Cited_Items","Freq. of Mention","Rel. Freq. of Mention","Mean Rank of Citation","Smith Index","Sutrop Index")
    return(datatable(res.FL1,options=list(lengthMenu=list(c(10,20,-1),c('10','20','All')),
                                            pageLength=20)))
  })
  
  output$download.FL.results<-downloadHandler(
    filename = "FL_Analysis_Results.csv",
    content=function(file){
      if(input$csvtype2==1) write.csv(res.FL(),file,row.names = F)
      if(input$csvtype2==2) write.csv2(res.FL(),file,row.names=F)
    })
  
  #####################TabPanel(FREE-LIST ANALYSIS)
                      ####TabsetPanel(CULT. SALIENCY)
                          ####TabPAnel (CHART)####
  output$fl.analysis.chart<-renderPlot({
    if(is.null(res.FL()))
      return(NULL)
    if(length(input$salience.ind.checkbox)==0)
      return(NULL)
    
    plot.fl.analysis.chart(res.FL(),input$salience.ind.checkbox,input$sort,input$freq.slider[1],input$freq.slider[2],input$text.size.FL.chart)
  },height=800,width=1200)
  
  output$download.FL.plot<-downloadHandler(
    filename=function(){"FL_Chart.pdf"},
    content=function(file){
      pdf(file,paper="a4r",width=12)
      print(plot.fl.analysis.chart(res.FL(),input$salience.ind.checkbox,input$sort,input$freq.slider[1],input$freq.slider[2],input$text.size.FL.chart))
      dev.off()
    }
  )
  
  #####################TabPanel(FREE-LIST ANALYSIS)
                      ####TabsetPanel(ITEM by ITEM PROXIMITY)####
  
  output$select.item.prox.plot.type<-renderUI({
    if(input$item.prox.index=="Successive count")
      return(radioButtons("item.prox.plot.type",
                          "Select the type of plot you wish to display",
                          c("Corr. Analysis"=2,"Dendrogram"=1)))
    return(radioButtons("item.prox.plot.type",
                 "Select the type of plot you wish to display",
                 c("MDS"=2,"Dendrogram"=1)))
  })
  
  output$download.item.prox.mat<-downloadHandler(
    filename="Item_by_Item_proximity_matrix.csv",
    content=function(file){
      if(input$item.prox.index=="Successive count"){
        obj2dwnl<-item.prox()[["Successive count"]]
      }else{
        obj2dwnl<-item.prox()[["Henley index"]]
      }
      if(input$csvtype.item.prox.mat==1) write.csv(obj2dwnl,file,row.names = T)
      if(input$csvtype.item.prox.mat==2) write.csv2(obj2dwnl,file,row.names=T)
    }
  )
  
  output$select.dendo.n<-renderUI({
    if(input$item.prox.plot.type!=1)
      return(NULL)
    sliderInput("dendo.n",label = "Limit range of possible clusters",min = 2,max=20,value = c(3,10),step = 1,ticks = T,dragRange = T,width = '80%')
    #numericInput("dendo.min",label = "Min. nb. of clusters",value = 3,min = 2,max=20,step = 1,width='80%')
  })
  
  output$select.item.categories<-renderUI({
    if(!is.null(ls.categories())){
      if(length(ls.categories())!=1){
        ls.cat.temp<-ls.categories()
        ls.tree.part<-which(ls.cat.temp=="tree.cut")
        if(length(ls.tree.part!=0)) ls.cat.temp<-ls.cat.temp[-ls.tree.part]
        return(selectInput("item.categories","Select the item categorical variable you whish to display",c(ls.cat.temp)))
      }else{
        return(NULL)
      } 
    }
  })
  
  output$show.tree.partition<-renderUI({
    if(is.null(res.FL()))
      return(NULL)
    if(input$item.prox.plot.type!=1)
      return(NULL)
    return(checkboxInput("tree.partition",value=FALSE,label = "Do you wish to use the dendrogram ideal partition as a category for the item categorical analyses?"))
  })
  
  output$show.download.tree.part.text0<-renderUI({
    if(is.null(res.FL()))
      return(NULL)
    if(input$item.prox.plot.type!=1)
      return(NULL)
    
    return(HTML("<b>DENDROGRAM'S IDEAL CLUSTERS</b>"))
  })
  
  output$show.download.tree.part.text1<-renderUI({
    if(is.null(res.FL()))
      return(NULL)
    if(input$item.prox.plot.type!=1)
      return(NULL)
    
    return(HTML("<b>Download dendrogram ideal partition</b>"))
  })
  
  output$show.download.tree.part<-renderUI({
    if(is.null(res.FL()))
      return(NULL)
    if(input$item.prox.plot.type!=1)
      return(NULL)
    
    return(downloadButton("download.tree.part",label = "Download"))
  })
  
  output$show.csvtype.tree.part<-renderUI({
    if(is.null(res.FL()))
      return(NULL)
    if(input$item.prox.plot.type!=1)
      return(NULL)
    
    return(radioButtons('csvtype.tree.part','Choose CVS Format',
                        c("Semicolon (French)"=2,
                          "Comma (English)"=1)))
  })
  
  output$download.tree.part<-downloadHandler(
    filename = "dendrogram_ideal_partition.csv",
    content=function(file){
      to.dwnl=item.prox.plot()$partition
      if(input$csvtype.tree.part==1) write.csv(to.dwnl,file,row.names = T)
      if(input$csvtype.tree.part==2) write.csv2(to.dwnl,file,row.names=T)
    })
  
  output$show.item.prox<-renderPlot({
    if(is.null(item.prox()))
      return(NULL)
    if(is.null(item.prox.plot()))
      return(NULL)
    return(item.prox.plot()$plot)
  },height=800,width=800)
  
  
  output$download.show.item.prox<-downloadHandler(
    filename="Item_by_Item_proximity.pdf",
    content=function(file){
      pdf(file,paper="a4r",width=12)
      print(item.prox.plot()$plot)
      dev.off()
    }
  )
  
  #####################TabPanel(ITEM ANALYSIS)
                      ####TabsetPanel(ITEM CATEGORIES ANALYSES)
                          ####TabPAnel (DICHOT BIAS)####
  output$dichot.bias.mess1<-renderUI({
    err.message<-HTML("<b>Analyses will only appear if you upload or use categorical information for your items. You may do so when: </b><ul><li>Uploading a Spreadsheet or ANTHROPAC formatted file.</li><li>Uploading a normalization and categorization table.</li><li>Plotting item-by-item proximity as a dendrogram and ticking the appropriate checkbox in order to use the tree partition as categories for your items. </li></ul>")
    if(is.null(ls.categories()))
      return(err.message)
    if(is.null(categ.clust()))
      return(err.message)
    if(length(categ.clust()$dichot_bias_categ)==0)
      return(HTML("<b>None of your uploaded item categories are dichotomous.</b>"))
    
    return(HTML(paste(
      "<b>The table below summarizes for each item category whether there exists, among your respondents, a bias in favor of one of the two modalities.</b><br/>",
      "The score in each column varies from 0 to 1.",
      "A high value for one modality indicates that, overall, respondents tended to cite items of that modality earlier in their lists than items of the other modality.<br/>",
      "<i>A z-test is carried out in order to test for statistical significance of the reported scores. If no z-score appears this means the clustering score is not significantly different from random.</i>",
      sep="<br/>")))
  })
  
  output$dichot.bias.mess2<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(length(categ.clust()$dichot_bias_categ)==0)
      return(NULL)
    return(HTML("<b>Statistical significance levels:</b><i> * p-value < 0.1 ; ** p-value < 0.05 ; *** p-value < 0.01</i>"))
  })
  
  output$select.resp.var.name.dichot<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(is.null(categ.clust()))
      return(NULL)
    if(is.null(resp.var.data()))
      return(NULL)
    if(!is.data.frame(resp.var.data()))
      return(NULL)
    return(selectInput("resp.var.name.dichot","Select the respondent variable you wish to show",c("All",colnames(resp.var.data()[-1]))))
  })
  
  output$select.categ.dichot<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(is.null(categ.clust()))
      return(NULL)
    if(length(categ.clust()$dichot_bias_categ)==0)
      return(NULL)
    return(selectInput("categ.dichot","Select the item category you wish to show",c("All",categ.clust()$dichot_bias_categ)))
  })
  
  output$show.dichot.bias<-renderTable({
    if(is.null(ls.categories()))
      return(NULL)
    if(length(categ.clust()$dichot_bias_categ)==0)
      return(NULL)
    
    temp.tab<-categ.clust()$dichot_bias_sum
    
    if(!is.null(input$resp.var.name.dichot)){
      if(input$resp.var.name.dichot!="All"){
        var.temp<-input$resp.var.name.dichot
        temp.tab<-temp.tab[c(1,grep(x=row.names(temp.tab),pattern=var.temp)),]
      }
    }
    
    if(!is.null(input$categ.dichot)){
      if(input$categ.dichot!="All"){
        categ.temp<-input$categ.dichot
        temp.tab<-temp.tab[,c(grep(x=names(temp.tab),pattern=categ.temp),dim(temp.tab)[2])]
      }
    }
    
    return(t(temp.tab))
  },striped = T,bordered=T,align = "c",rownames = T)
  
  output$show.download.dichot.bias<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(length(categ.clust()$dichot_bias_categ)==0)
      return(NULL)

      return(downloadButton("download.dichot.bias",label = "Download"))
  })
  
  output$show.csvtype.dichot.bias<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(length(categ.clust()$dichot_bias_categ)==0)
      return(NULL)
    
    return(radioButtons('csvtype.dichot.bias','Choose CVS Format',
                   c("Semicolon (French)"=2,
                     "Comma (English)"=1)))
  })
  
  output$download.dichot.bias<-downloadHandler(
    filename = "dichot_bias_summary_table.csv",
    content=function(file){
      to.dwnl=categ.clust()$dichot_bias_sum
      if(input$csvtype.dichot.bias==1) write.csv(to.dwnl,file,row.names = T)
      if(input$csvtype.dichot.bias==2) write.csv2(to.dwnl,file,row.names=T)
    })
  
  output$dichot.bias.mess3<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(length(categ.clust()$dichot_bias_categ)==0)
      return(NULL)
    return(HTML("You may download, by clicking on button below, the complete table containing clustering scores respondent by respondent."))
  })

  output$show.download.dichot.bias.tot<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(length(categ.clust()$dichot_bias_categ)==0)
      return(NULL)
    
    return(downloadButton("download.dichot.bias.tot",label = "Download"))
  })
  
  output$show.csvtype.dichot.bias.tot<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(length(categ.clust()$dichot_bias_categ)==0)
      return(NULL)
    
    return(radioButtons('csvtype.dichot.bias.tot','Choose CVS Format',
                        c("Semicolon (French)"=2,
                          "Comma (English)"=1)))
  })
  
  output$download.dichot.bias.tot<-downloadHandler(
    filename = "dichot_bias_total_table.csv",
    content=function(file){
      to.dwnl=categ.clust()$dichot_bias
      if(input$csvtype.dichot.bias.tot==1) write.csv(to.dwnl,file,row.names = T)
      if(input$csvtype.dichot.bias.tot==2) write.csv2(to.dwnl,file,row.names=T)
    })
  
  #####################TabPanel(ITEM ANALYSES)
                      ####TabsetPanel(ITEM CATEEGORIES ANALYSIS)
                          ####TabPAnel (CLUSTERING)####
  
  
  output$clustering.mess1<-renderUI({
    err.message<-HTML("<b>Analyses will only appear if you upload or use categorical information for your items. You may do so when: </b><ul><li>Uploading a Spreadsheet or ANTHROPAC formatted file.</li><li>Uploading a normalization and categorization table.</li><li>Plotting item-by-item proximity as a dendrogram and ticking the appropriate checkbox in order to use the tree partition as categories for your items. </li></ul>")
    if(is.null(ls.categories()))
      return(err.message)
    if(is.null(categ.clust()))
      return(err.message)
    
    return(HTML(paste(
      "<b>The table below summarizes for each item category whether respondents tended to cite in grouped clusters items belonging to the different modalities of that category.</b><br/>",
      "The score in each column varies from 0 to 1.",
      "A high value for one modality indicates that, overall, respondents tended to cluster the items belonging to that category modality.<br/>",
      "<i>A z-test is carried out in order to test for statistical significance of the reported scores. If no z-score appears this means the clustering score is not significantly different from random.</i>",
      sep="<br/>")))
  })
  
  output$select.resp.var.name.clust<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(is.null(categ.clust()))
      return(NULL)
    if(is.null(resp.var.data()))
      return(NULL)
    if(!is.data.frame(resp.var.data()))
      return(NULL)
    return(selectInput("resp.var.name.clust","Select the respondent variable you wish to show",c("All",colnames(resp.var.data()[-1]))))
  })
  
  output$select.categ.clust<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(is.null(categ.clust()))
      return(NULL)
    return(selectInput("categ.clust","Select the item category you wish to show",c("All",ls.categories()[-1])))
  })
  
  output$show.clustering<-renderTable({
    if(is.null(ls.categories()))
      return(NULL)
    if(is.null(categ.clust()))
      return(NULL)
    temp.tab<-categ.clust()$clustering_sum
    
    if(!is.null(input$resp.var.name.clust)){
      if(input$resp.var.name.clust!="All"){
        var.temp<-input$resp.var.name.clust
        temp.tab<-temp.tab[c(1,grep(x=row.names(temp.tab),pattern=var.temp)),]
      }
    }
    
    if(!is.null(input$categ.clust)){
      if(input$categ.clust!="All"){
        categ.temp<-input$categ.clust
        temp.tab<-temp.tab[,c(grep(x=names(temp.tab),pattern=categ.temp),dim(temp.tab)[2])]
      }
    }
    
    return(t(temp.tab))
  },striped = T,bordered=T,align = "c",rownames = T)
  
  output$clustering.mess2<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(length(categ.clust()$dichot_bias_categ)==0)
      return(NULL)
    return(HTML("<b>Statistical significance levels:</b><i> * p-value < 0.1 ; ** p-value < 0.05 ; *** p-value < 0.01</i>"))
  })
  
  output$show.download.clustering<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(is.null(categ.clust()))
      return(NULL)
    
    return(downloadButton("download.clustering",label = "Download"))
  })
  
  output$show.csvtype.clustering<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(is.null(categ.clust()))
      return(NULL)
    
    return(radioButtons('csvtype.clustering','Choose CVS Format',
                        c("Semicolon (French)"=2,
                          "Comma (English)"=1)))
  })
  
  output$download.clustering<-downloadHandler(
    filename = "dichot_bias_summary_table.csv",
    content=function(file){
      to.dwnl=categ.clust()$clustering_sum
      if(input$csvtype.clustering==1) write.csv(to.dwnl,file,row.names = T)
      if(input$csvtype.clustering==2) write.csv2(to.dwnl,file,row.names=T)
    })
  
  output$clustering.mess3<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(is.null(categ.clust()))
      return(NULL)
    return(HTML("You may download, by clicking on button below, the complete table containing clustering scores respondent by respondent."))
  })
  
  output$show.download.clustering.tot<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(is.null(categ.clust()))
      return(NULL)
    
    return(downloadButton("download.clustering.tot",label = "Download"))
  })
  
  output$show.csvtype.clustering.tot<-renderUI({
    if(is.null(ls.categories()))
      return(NULL)
    if(is.null(categ.clust()))
      return(NULL)
    
    return(radioButtons('csvtype.clustering.tot','Choose CVS Format',
                        c("Semicolon (French)"=2,
                          "Comma (English)"=1)))
  })
  
  output$download.clustering.tot<-downloadHandler(
    filename = "dichot_bias_total_table.csv",
    content=function(file){
      to.dwnl=categ.clust()$clustering
      if(input$csvtype.clustering.tot==1) write.csv(to.dwnl,file,row.names = T)
      if(input$csvtype.clustering.tot==2) write.csv2(to.dwnl,file,row.names=T)
    })
  

  #####################TabPanel(ITEM ANALYSES)
                      ####TabsetPanel(DATA SATURATION)####
  
  output$data.saturation<-renderPlot({
    if(is.null(data.saturation()))
      return(NULL)
    plot.data.saturation(data.saturation())
  },height=800,width=800)
  
  output$download.data.saturation<-downloadHandler(
    filename="Data_Saturation.pdf",
    content=function(file){
      pdf(file,paper="a4",width=12)
      print(plot.data.saturation(data.saturation()))
      dev.off()
    }
  )
  
  output$data.sat.text<-renderText({
    if(is.null(data.saturation()))
      return(NULL)
    paste((dim(data.saturation())[1]-1)," respondents have cited all of the "
          ,res1()$nb.item,
          " cited items. The remaining ",
          res1()$nb.resp-(dim(data.saturation())[1]-1),
          " respondents have added no new information.",sep="")
  })
  
  
  ###################################################################################################################
  #####################TabPanel(Respondent Analysis)####
                      ####SideBarPanel####                  
  
  output$panel5.sidebar.text1<-renderText({
    if(is.null(res.FL()))
      return("Analyses will appear once you have uploaded your data.")
  })
  
  output$select.resp.var.name<-renderUI({
    if(is.null(resp.var.data()))
      return(NULL)
    if(!is.data.frame(resp.var.data()))
      return(NULL)
    selectInput("resp.var.name","Select the respondent variable you wish to plot",c("No Variable",colnames(resp.var.data()[-1])))
  })
  
  #####################TabPanel(Respondent Analysis) 
                      ####TabsetPanel(Sample Distribution)####
  output$panel5.tab1.text1<-renderUI({
    if(is.null(resp.var.data()))
      return(HTML("<b>Sample distribution only appears if you upload data with respondent variables (e.g. gender, age etc...)</b>"))
    if(!is.data.frame(resp.var.data()))
      return(HTML(resp.var.data()))
  })
  
  output$show.res.sum.resp.var<-renderTable({
    if(is.null(resp.var.data()))
      return(NULL)
    if(!is.data.frame(resp.var.data()))
      return(NULL)
    res.sum.resp.var()
  },include.rownames=F,bordered = TRUE,striped = TRUE,na = "NA")
  
  output$show.download.resp.sample.tab<-renderUI({
    if(is.null(resp.var.data()))
      return(NULL)
    if(!is.data.frame(resp.var.data()))
      return(NULL)
    return(downloadButton("download.resp.sample.tab",label = "Download"))
  })
  
  output$show.csvtype7<-renderUI({
    if(is.null(resp.var.data()))
      return(NULL)
    if(!is.data.frame(resp.var.data()))
      return(NULL)
    return(radioButtons('csvtype7','Choose CVS Format',
                        c("Semicolon (French)"=2,
                          "Comma (English)"=1))
           )
  })
  
  output$download.resp.sample.tab<-downloadHandler(
    filename = "Sample_Distribution_Resp_Variables.csv",
    content=function(file){
      to.dwnl=res.sum.resp.var()
      if(input$csvtype7==1) write.csv(to.dwnl,file,row.names = F)
      if(input$csvtype7==2) write.csv2(to.dwnl,file,row.names=F)
    })

  #####################TabPanel(Respondent Analysis)
                      ####TabsetPanel(Informant Competence)####
                          ####TabSetPanel(Table Results)####
 
  output$download.resp.anal.res.tab<-downloadHandler(
    filename = "Resp_Analysis_Results.csv",
    content=function(file){
      res.resp2=res.resp()
      colnames(res.resp2)[1:4]=c("List Length","Summed frequency of mentioned items","Avg. freq. of mentioned items","Rank to Freq. correlation")
      if(input$csvtype3==1) write.csv(res.resp2,file)
      if(input$csvtype3==2) write.csv2(res.resp2,file)
    }
  )
  
  output$resp.anal.res.tab<-DT::renderDataTable({
    if(is.null(res.resp()))
      return(NULL)
    res.resp2=res.resp()
    res.resp2$Resp_ID=row.names(res.resp2)
    row.names(res.resp2)=seq(1:dim(res.resp2)[1])
    res.resp2=res.resp2[,c(dim(res.resp2)[2],seq(1:(dim(res.resp2)[2]-1)))]
    colnames(res.resp2)[1:5]=c("Resp. ID","List Length","Summed frequency of mentioned items","Avg. freq. of mentioned items","Rank to Freq. correlation")
    
    return(datatable(res.resp2,options=list(lengthMenu=list(c(10,20,-1),c('10','20','All')),
                                                                 pageLength=20)))
  })
  
  #####################TabPanel(Respondent Analysis)
                      ####TabsetPanel(Informant Competence)
                          ####TabSetPanel(Chart)####
  
  output$download.resp.anal.res.chart<-downloadHandler(
    filename="Resp_Competence_chart.pdf",
    content=function(file){
      pdf(file,paper="a4r",width=12)
      resp.var.name1="No Variable"
      if(!is.null(resp.var.data())& is.data.frame(resp.var.data()))
        resp.var.name1=input$resp.var.name
      plot.res.comp(res.resp(),resp.var.name1)
      dev.off()
    }
  )
  output$resp.anal.res.chart<-renderPlot({
    if(is.null(res.resp()))
      return(NULL)
    resp.var.name1="No Variable"
    if(!is.null(resp.var.data()) & is.data.frame(resp.var.data()))
      resp.var.name1=input$resp.var.name
    
    print(plot.res.comp(res.resp(),resp.var.name1))
      
  },height=800,width=1200)
  
  #####################TabPanel(Respondent Analysis)
                      ####TabsetPanel(Respondent Proximity)####
                          ####TabSetPanel(Methods)####
  
  #####################TabPanel(Respondent Analysis)
                      ####TabsetPanel(Respondent Proximity)
                          ####TabSetPanel(Between Class Analysis)####
  
  output$panel5.tab3b.text1<-renderUI({
    if(is.null(resp.var.data()))
      return(HTML("<b>Between class analysis only appears if you upload data with respondent variables (e.g. gender, age etc...)</b>"))
    if(!is.data.frame(resp.var.data()))
      return(HTML(resp.var.data()))
    return(NULL)
  })
  
  output$show.disper.tab<-renderTable({
    return(resp.prox()$res.dispersion)
  },include.rownames=T,bordered = TRUE,striped = TRUE)
  
  output$download.disper.tab<-downloadHandler(
    filename = "Resp_Var_Disper_Tab.csv",
    content=function(file){
      to.dwnl<-resp.prox()$res.dispersion
      if(input$csvtype8==1) write.csv(to.dwnl,file)
      if(input$csvtype8==2) write.csv2(to.dwnl,file)
    }
  )
  
  output$show.adonis.tab<-renderTable({
    return(resp.prox()$res.adonis)
  },include.rownames=T,bordered = TRUE,striped = TRUE)
  
  output$download.adonis.tab<-downloadHandler(
    filename = "Resp_Var_Adonis_Tab.csv",
    content=function(file){
      to.dwnl<-resp.prox()$res.adonis
      if(input$csvtype9==1) write.csv(to.dwnl,file)
      if(input$csvtype9==2) write.csv2(to.dwnl,file)
    }
  )
  
  #####################TabPanel(Respondent Analysis)
                      ####TabsetPanel(Respondent Proximity)
                          ####TabSetPanel(Resp. Prox. Plot)####
  output$show.adonis.sum.res<-renderUI({
    return(HTML(resp.prox()$sum.res[which(row.names(resp.prox()$sum.res)==input$resp.var.name),1]))
  })
  
  output$show.plot.resp.prox<-renderPlot({
    if(is.null(res.resp()))
      return(NULL)
    resp.var.name2="No Variable"
    if(!is.null(resp.var.data()) & is.data.frame(resp.var.data()))
      resp.var.name2=input$resp.var.name
    p<-plot.resp.prox(resp.prox(),resp.var.name2,input$select.freq.slider3)
    return(p)
  },height=800,width=800)
  
  output$download.show.plot.resp.prox<-downloadHandler(
    filename="Resp_by_Resp_Prox_Chart.pdf",
    content=function(file){
      pdf(file,paper="a4",width=12)
      resp.var.name2="No Variable"
      if(!is.null(resp.var.data()) & is.data.frame(resp.var.data()))
        resp.var.name2=input$resp.var.name
      p<-plot.resp.prox(resp.prox(),resp.var.name2,input$select.freq.slider3)
      print(p)
      dev.off()
    }
  )
  
  #####################TabPanel(Respondent Analysis) 
                      ####TabsetPanel(Items' saliency)####
                          ####TabSetPanel(Table Results)####
  output$panel5.tab4a.text1<-renderUI({
    if(is.null(resp.var.data()))
      return(HTML("<b>Results only appear if you upload data with respondent variables (e.g. gender, age etc...)</b>"))
    if(!is.data.frame(resp.var.data()))
      return(HTML(resp.var.data()))
    if(input$resp.var.name=="No Variable")
      return(HTML("<b>Please select a respondent variable in the left panel (grey background).</b>"))
  })
  
  output$show.res.FL.resp.var<-DT::renderDataTable({
    if(is.null(resp.var.data()))
      return(NULL)
    if(!is.data.frame(resp.var.data()))
      return(NULL)
    res.to.show<-res.FL.resp.var()
    res.to.show$Cited_items<-row.names(res.to.show)
    res.to.show<-res.to.show[,c(dim(res.to.show)[2],seq(1:(dim(res.to.show)[2]-1)))]
    row.names(res.to.show)<-seq(1:dim(res.to.show)[1])
    return(datatable(res.to.show,options=list(lengthMenu=list(c(10,20,-1),c('10','20','All')),
                                                     pageLength=20)))
  })
  
  output$download.show.res.FL.resp.var<-downloadHandler(
    filename = "FL_Analysis_with_Resp_Var.csv",
    content=function(file){
      dnl.data=res.FL.resp.var()
      if(input$csvtype4==1) write.csv(dnl.data,file)
      if(input$csvtype4==2) write.csv2(dnl.data,file)
    }
  )
  
  #####################TabPanel(Respondent Analysis)  
                      #####TabsetPanel(Items' saliency)
                          ####TabSetPanel(Chart)####  
  
  output$panel5.tab4b.text1<-renderUI({
    if(is.null(resp.var.data()))
      return(HTML("<b>Results only appear if you upload data with respondent variables (e.g. gender, age etc...)</b>"))
    if(!is.data.frame(resp.var.data()))
      return(HTML(resp.var.data()))
    if(input$resp.var.name=="No Variable")
      return(HTML("<b>Please select a respondent variable in the left panel (grey background).</b>"))
  })
  
  output$select.resp.var.name.mod<-renderUI({
    if(is.null(resp.var.data()))
      return(NULL)
    if(!is.data.frame(resp.var.data()))
      return(NULL)
    radioButtons("resp.var.name.mod","Choose variable modality to sort data with",
                 c(as.character(levels(resp.var.data()[,as.character(input$resp.var.name)])))
    )
  })
  
  output$select.text.size.FL.resp<-renderUI({
    if(is.null(resp.var.data()))
      return(NULL)
    if(!is.data.frame(resp.var.data()))
      return(NULL)
    sliderInput("text.size.FL.resp","Choose label size for x axis",min = 6,max=16,value = 10,step = 1,ticks = T)
    
  })
  
  output$show.res.FL.resp.var.chart<-renderPlot({
    if(is.null(resp.var.data()))
      return(NULL)
    if(is.data.frame(resp.var.data()))
      plot.fl.analysis.chart.resp.var(res.FL.resp.var(),input$select.sal.index,input$select.freq.slider2,input$resp.var.name.mod,res.FL(),input$resp.var.name,input$text.size.FL.resp)
  },height=800,width=1200)
  
  output$download.show.res.FL.resp.var.chart<-downloadHandler(
    filename="FL_Analysis_Chart_with_Resp_Variables.pdf",
    content=function(file){
      pdf(file,paper="a4r",width=12)
      print(plot.fl.analysis.chart.resp.var(res.FL.resp.var(),input$select.sal.index,input$select.freq.slider2,input$resp.var.name.mod,res.FL(),input$resp.var.name,input$text.size.FL.resp))
      dev.off()
    }
  )
  
  #####################TabPanel(Respondent Analysis)
                      ####TabsetPanel(Data Format Example)####
  output$show.sample3<-renderTable({
    if(is.null(data.samples2()))
      return(NULL)
    return(data.samples2())
  },include.rownames=F,bordered = TRUE,striped = TRUE)
  
####end####
})