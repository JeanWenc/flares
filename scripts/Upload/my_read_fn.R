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

myReadFn<-function(data,header1,sep1,quote1,SaF){
  
  data1<-read.csv(file=data,header=header1,sep=sep1,quote = quote1,comment.char ="*",na.strings = c("NA",""),stringsAsFactors = SaF)  
  
  if(header1==FALSE){
    temp<-data1[1,1] 
  }else{
    temp<-names(data1)[1]
  }
  if(length(grep(pattern = "\u00EF",x = temp))>0){
    data1<-read.csv(file=data,header=header1,sep=sep1,quote = quote1,comment.char ="*",na.strings = c("NA",""),stringsAsFactors = SaF,fileEncoding = "UTF-8-BOM") 
  }else{
    enc<-guess_encoding(data,n_max = -1,threshold = 0.05)
    if(length(enc)!=0){
      if(length(grep("ISO",x = enc[1,1]))!=0 | length(grep("windows-12",x = enc[1,1]))){
        data1<-read.csv(file=data,header=header1,sep=sep1,quote = quote1,comment.char ="*",na.strings = c("NA",""),stringsAsFactors = SaF,encoding = "latin1")
        for(j in 1:dim(data1)[2]){
          data1[,j]<-as.character(data1[,j])
          for(i in 1:dim(data1)[1]){
            data1[i,j]<-enc2utf8(data1[i,j])
          }
        }
      }
    }
  }
  return(data1)
}