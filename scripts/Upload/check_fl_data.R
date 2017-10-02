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

check.FL.data<-function(data1,apac,header){
  if(apac=="FLAME"){
    data2=data.frame("")
    res=list(data1,data2,"OK")
    names(res)=c("FLAME_format","list_format","check")
    
    if(dim(data1)[2]<2){
      res$check="NOT_FLAME"
    }
  }#end apac==FALSE
  
  if(apac=="APAC"){
    if(length(grep("#",data1[,1]))<2){
      res=list("","","NOT_APAC")
      names(res)=c("FLAME_format","list_format","check")
    }
      
    if(length(grep("#",data1[,1]))>1){
      if(header==FALSE){
        res=anth_form(data1,apac)
        res[["check"]]="OK"  
        }#end header==FALSE
      
      if(header==TRUE){
        if(length(grep("#",names(data1)))!=0 | length(grep("X.",names(data1)))!=0){
          res=list("","","APAC_HEADER")
          names(res)=c("FLAME_format","list_format","check")
          }
        if(length(grep("#",names(data1)))==0 & length(grep("X.",names(data1)))==0){
          res=anth_form(data1,apac)
          res[["check"]]="OK"
          }
        }#end header==TRUE
      }  
  }#end apac==TRUE
  
  if(apac=="ATOOLS"){
    if(dim(data1)[2]<2){
      res=list("","","NOT_ATOOLS")
      names(res)=c("FLAME_format","list_format","check")
    }else if(length(unique(data1[,1]))<2){
      res=list("","","NOT_ATOOLS")
      names(res)=c("FLAME_format","list_format","check")
    }else{
      res=anth_form(data1,apac)
      res[["check"]]="OK"
    }
  }
  
  return(res)
}