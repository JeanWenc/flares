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
check.email<-function(user.email,user.name,user.inst,user.dataset,user.language,lat,lng,user.country,make.public){
  
  today<-as.character(format(Sys.time(),"%Y_%b_%d"))
  if(lat==0 & lng==0){
    lat<-NA
    lng<-NA
  }
  
  user.temp<-read.csv2("www/userDB/users.csv",stringsAsFactors = F)
  
  user.temp.row<-which(user.temp$mail==user.email)
  
  if(length(user.temp.row)==0){
    mess="Welcome!"
  }else{
    mess="Welcome Back!"
  }
  
  if(user.email!="jeanwencelius@gmail.com"){
    user.temp[dim(user.temp)[1]+1,]<-c(dim(user.temp)[1]+1,user.email,user.name,user.inst,user.dataset,user.language,lat,lng,today,user.country,make.public)
    write.csv2(user.temp,file = "www/userDB/users.csv",row.names = F)
  }
  
  return(mess)
}

