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

output$show.submit.button<-renderUI({
  if(length(grep(".+@.+[.].+",input$user.email))==0)
    return(NULL)
  return(actionButton("submit.button",label = "Submit"))
})

output$show.make.public<-renderUI({
  if(length(grep(".+@.+[.].+",input$user.email))==0)
    return(NULL)
  if(input$user.email%in%users.df$mail){
    if(input$new.dataset==FALSE)
      return(NULL)
  }
  return(checkboxInput("make.public",label="I allow other FLARES' users to access the information provided in the form below.",value = F))
})

output$show.user.name<-renderUI({
  if(is.null(input$submit.button))
    return(NULL)
  if(input$user.email%in%users.df$mail)
    return(HTML("<strong style='font-size:1.2em'>Welcome back!</strong>"))
  return(textInput("user.name",label="Full Name",value="",placeholder="John Doe"))
})

output$show.user.inst<-renderUI({
  if(is.null(input$submit.button))
    return(NULL)
  if(input$user.email%in%users.df$mail)
    return(checkboxInput(inputId = "new.dataset",value = FALSE,label = "I'm working with a new dataset."))
  return(textInput("user.inst",label="Institution",value="",placeholder="University of ..."))
})

output$show.user.dataset<-renderUI({
  if(is.null(input$submit.button))
    return(NULL)
  if(input$user.email%in%users.df$mail){
    if(input$new.dataset==FALSE)
      return(NULL)
  }
  return(textInput("user.dataset",label="Cultural domain (e.g. Animals, Trees, etc.)",value="",placeholder="Insects"))
})

output$show.user.language<-renderUI({
  if(is.null(input$submit.button))
    return(NULL)
  if(input$user.email%in%users.df$mail){
    if(input$new.dataset==FALSE)
      return(NULL)
  }
  return(textInput("user.language",label="Language Free-Lists were collected in?",value="",placeholder="French"))
})

output$show.user.country<-renderUI({
  if(is.null(input$submit.button))
    return(NULL)
  if(input$user.email%in%users.df$mail){
    if(input$new.dataset==FALSE)
      return(NULL)
  }
  return(textInput("user.country",label="Where have the free-lists been collected?",value="",placeholder="France"))
})

output$show.map.text<-renderUI({
  if(is.null(input$submit.button))
    return(NULL)
  if(input$user.email%in%users.df$mail){
    if(input$new.dataset==FALSE)
      return(NULL)
  }
  return(HTML("<p><strong>Click on map to get exact coordinates.</strong></p>"))
})

observeEvent(input$submit.button,{
  if(is.null(input$user.name)){
    user.name.temp<-""
    user.inst.temp<-""
  }else{
    user.name.temp<-input$user.name
    user.inst.temp<-input$user.inst
  }
  if(is.null(input$user.dataset)){
    user.dataset.temp<-""
    user.language.temp<-""
    user.country.temp<-""
  }else{
    user.dataset.temp<-input$user.dataset
    user.language.temp<-input$user.language
    user.country.temp<-input$user.country
  }
  if(is.null(input$make.public)){
    make.public.temp<-""
  }else{
    make.public.temp<-input$make.public
  }
  
  temp.mess<-check.email(input$user.email,user.name.temp,user.inst.temp,user.dataset.temp,user.language.temp,values$click_lat,values$click_long,user.country.temp,make.public.temp)
  values$email.ok<-temp.mess
  users.df<-read.csv2("www/userDB/users.csv",stringsAsFactors = F)
})

output$map<-renderLeaflet({
  if(is.null(input$submit.button))
    return(NULL)
  if(input$user.email%in%users.df$mail){
    if(input$new.dataset==FALSE)
      return(NULL)
  }
  leaflet()%>%
    setView(lng = 0,lat=0,zoom = 1)%>%
    addProviderTiles(provider = providers$OpenTopoMap)
})

observeEvent(input$map_click, {
  click <- input$map_click
  values$click_lat <- click$lat
  values$click_long <- click$lng
  
  leafletProxy("map") %>%
    clearMarkers()%>%
    addMarkers(lng=click$lng,lat=click$lat,label=NULL)
})

users.df2<-users.df
users.df2<-users.df2[which(users.df2$public=="TRUE"),]
users.df2<-users.df2[which(!is.na(users.df2$lat)),]

pntstoplot<-reactive({
  users.df3<-users.df2
  if(!is.null(input$select.domain)){
    if(input$select.domain!="All"){
      users.df3<-as.data.frame(users.df2[which(users.df2$dataset==input$select.domain),])
    }
  }
  pntstoplot.temp<-SpatialPointsDataFrame(coords = data.frame(lng=as.numeric(users.df3$lng),lat=as.numeric(users.df3$lat)),data=users.df3[,c("name","instit","dataset","language","mail")])
  rownames(pntstoplot.temp@data)<-seq(1:length(pntstoplot.temp))
  return(pntstoplot.temp)
})

output$userMap<-renderLeaflet({
  leaflet()%>%
    setView(lng=0,lat=25,zoom=2)%>%
    addProviderTiles(provider=providers$OpenTopoMap)%>%
    addMarkers(data=pntstoplot(),labelOptions=list(noHide=T),layerId = as.character(seq(1:length(pntstoplot()))))
})

observeEvent(input$userMap_marker_click,{
  click2<-input$userMap_marker_click
  popup.line<-as.data.frame(pntstoplot()@data[click2$id,])
  
  leafletProxy("userMap")%>%
    clearPopups()%>%
    addPopups(click2$lng,click2$lat,popup=paste("<b>",popup.line$dataset," (",popup.line$language,")</b></br>",popup.line$name,"</br>",popup.line$instit,"</br>",popup.line$mail,sep=""))
})


output$show.select.domain<-renderUI({
  return(selectInput("select.domain",label = "Select domain",choices=c("All",unique(users.df2$dataset)),width="150px"))
})

output$licence<-renderText({
  licenceText<-paste(readLines("LICENSE"),collapse="\n")
  licenceText<-substring(licenceText,4)
  return(licenceText)
})