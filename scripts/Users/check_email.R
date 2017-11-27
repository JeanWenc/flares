check.email<-function(user.email,user.name,user.inst,user.dataset,user.language,lat,lng,make.public){
  
  today<-as.character(format(Sys.time(),"%Y_%b_%d"))
  if(lat==0 & lng==0){
    lat<-NA
    lng<-NA
  }
  
  user.temp<-read.csv2("data/users.csv",stringsAsFactors = F)
  
  user.temp.row<-which(user.temp$mail==user.email)
  
  if(length(user.temp.row)==0){
    mess="Welcome!"
  }else{
    mess="Welcome Back!"
  }
  
  user.temp[dim(user.temp)[1]+1,]<-c(dim(user.temp)[1]+1,user.email,user.name,user.inst,user.dataset,user.language,lat,lng,today,NA,make.public)
  
  write.csv2(user.temp,file = "data/users.csv",row.names = F)
  return(mess)
}

