check.email<-function(user.email){
  user.temp<-read.csv2("data/users.csv",stringsAsFactors = F)
  user.temp.row<-which(user.temp$mail==user.email)
  if(length(user.temp.row)==0){
    user.temp[dim(user.temp)[1]+1,]<-c(dim(user.temp)[1]+1,user.email,1)
    mess="Welcome!"
  }else{
    user.temp[user.temp.row,"N_conn"]<-user.temp[user.temp.row,"N_conn"]+1
    mess="Welcome Back!"
  }
  write.csv2(user.temp,file = "data/users.csv",row.names = F)
  return(mess)
}

