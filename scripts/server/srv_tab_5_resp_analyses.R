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

####TabsetPanel(Upload)####
output$panel5.tab1.text1<-renderUI({
  if(is.null(resp.var.data()))
    return(HTML("<p><b>Data concerning respondent variables should be uploaded as a .csv file.</b></p>
                <p>Your data should be formatted as follows:</p>"))
  if(!is.data.frame(resp.var.data()))
    return(HTML(resp.var.data()))
})

output$show.res.sum.resp.var<-renderTable({
  if(is.null(resp.var.data())){
    if(is.null(data.samples2()))
      return(NULL)
    return(data.samples2())
  }
  if(!is.data.frame(resp.var.data())){
    if(is.null(data.samples2()))
      return(NULL)
    return(data.samples2())
  }else{
    return(res.sum.resp.var())
  }
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

####TabsetPanel(Informant Competence)
####TabSetPanel(Chart)####

output$download.resp.anal.res.chart<-downloadHandler(
  filename="Resp_Competence_chart.pdf",
  content=function(file){
    pdf(file,paper="a4r",width=12)
    resp.var.name1="No Variable"
    if(!is.null(resp.var.data())& is.data.frame(resp.var.data()))
      resp.var.name1=input$resp.var.name
    print(plot.res.comp(res.resp(),resp.var.name1))
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
