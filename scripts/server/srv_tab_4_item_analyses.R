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


####TabsetPanel(CULT. SALIENCY)####
####TabPanel (TABLE)####
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
  return(checkboxInput("tree.partition",value=FALSE,label = "Do you wish to use the estimated 'best' partition as a category for the item categorical analyses?"))
})

output$show.download.tree.part.text0<-renderUI({
  if(is.null(res.FL()))
    return(NULL)
  if(input$item.prox.plot.type!=1)
    return(NULL)
  
  return(HTML("<b>DENDROGRAM'S BEST PARTITION</b>"))
})

output$show.download.tree.part.text1<-renderUI({
  if(is.null(res.FL()))
    return(NULL)
  if(input$item.prox.plot.type!=1)
    return(NULL)
  
  return(HTML("<b>Download dendrogram best partition</b>"))
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
  filename = "dendrogram_best_partition.csv",
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
    "<b>The table below summarizes for each items' dichotomous category whether there exists, among your respondents, a bias in favor of one of the two sub-categories.</b>",
    "Dichotomous category bias is measured through a score, named 'B' (see the 'Methods' sub-tab).",
    "The B score in each column varies from 0 to 1.",
    "A high value for one sub-category indicates that, overall, respondents tend to cite items of that sub-category earlier in their lists than items of the other sub-category.<br/>",
    "<i>A z-test is carried out in order to test for statistical significance of the reported B scores. If no z-score appears this means the B score is not significantly different from random.</i>",
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
  return(HTML("You may download, by clicking on button below, the complete table containing dichotomous bias scores respondent by respondent."))
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

####TabsetPanel(ITEM CATEEGORIES ANALYSIS)
####TabPAnel (CLUSTERING)####


output$clustering.mess1<-renderUI({
  err.message<-HTML("<b>Analyses will only appear if you upload or use categorical information for your items. You may do so when: </b><ul><li>Uploading a Spreadsheet or ANTHROPAC formatted file.</li><li>Uploading a normalization and categorization table.</li><li>Plotting item-by-item proximity as a dendrogram and ticking the appropriate checkbox in order to use the tree partition as categories for your items. </li></ul>")
  if(is.null(ls.categories()))
    return(err.message)
  if(is.null(categ.clust()))
    return(err.message)
  
  return(HTML(paste(
    "<b>The table below summarizes for each item category whether respondents tended to cite in grouped clusters items belonging to the different modalities of that category.</b>",
    "Clustering is measured through a score named 'C' (see the 'Methods' sub-tab).",
    "The score in each column varies from 0 to 1.",
    "A high value for one sub-category indicates that, overall, respondents tended to cluster the items belonging to that sub-category.<br/>",
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
