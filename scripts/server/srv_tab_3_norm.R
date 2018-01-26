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

####Sibebar####      
output$panel3.sidebar.text1<-renderText({
  if(is.null(res.FL()))
    return("Fucntions will be available once you have uploaded your data.")
})

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
                 c("Semicolon (FR)"=2,
                   "Comma (ENG)"=1))
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
    str<-c(str,"You can either create columns for <b>normalization</b> (original items will be replaced by those from the normalization column of your choice) or for <b>categorization</b> (whether provided by respondents or defined by researcher, item categories are used for specific analyses regarding category preferential bias or category clustering within lists).<br/><br/>")
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
  if(length(norm.col)!=0) return(selectInput("norm.type","Select normalization column",c(norm.col)))
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
                   c("Semicolon (FR)"=2,
                     "Comma (ENG)"=1))
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

####TabsetPanel(DATA FORMAT EXAMPLE)####
output$show.sample4<-renderTable({
  if(is.null(res1()))
    return(NULL)
  return(read.csv2("data/Norm_Data_Format_Exple.csv",h=T))
},include.rownames=F,align=c,bordered = T,striped=T)
