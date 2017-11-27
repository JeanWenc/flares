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

output$show.map.text<-renderUI({
  if(is.null(input$submit.button))
    return(NULL)
  if(input$user.email%in%users.df$mail){
    if(input$new.dataset==FALSE)
      return(NULL)
  }
  return(HTML("<p><strong>Where have the free-lists been collected?</strong></br><i>Please click on map!</i></p>"))
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
  }else{
    user.dataset.temp<-input$user.dataset
    user.language.temp<-input$user.language
  }
  if(is.null(input$make.public)){
    make.public.temp<-""
  }else{
    make.public.temp<-input$make.public
  }
  
  temp.mess<-check.email(input$user.email,user.name.temp,user.inst.temp,user.dataset.temp,user.language.temp,values$click_lat,values$click_long,make.public.temp)
  values$email.ok<-temp.mess
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