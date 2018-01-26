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
               "Please look at the example on the right side of the screen.",
               sep="<br/>"))
  #if(input$apac.format.checkbox==TRUE)
  #HTML(paste("Check the above box only if you have - in the column or columns adjacent to the one containing the cited items - information concerning each of the cited items.",
  #           "Please go to the 'ANTHROPAC Data Format Example' sub-tab for more details.",
  #           sep="<br/>"))
})

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
    return(HTML("<b>The file you have uploaded does not contain item categorical information.</b>"))
  
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
    radioButtons('csvtype6','Choose CSV Format',
                 c("Semicolon (FR)"=2,
                   "Comma (ENG)"=1))
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