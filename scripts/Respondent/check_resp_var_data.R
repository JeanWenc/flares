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

check.resp.var.data<-function(resp.var.data1,mat.cit.abs.pres){

  #debug
  #resp.var.data1<-resp.var.data2
  #mat.cit.abs.pres<-res1$mat.cit.abs.pres
  
  if(dim(resp.var.data1)[2]<2){
    res="<b>Your uploaded respondent variables file doesn't seem to be in the correct format.<br/>Please verify your seperator and quote options in the left side-panel.</b>"
  }
  
  if(dim(resp.var.data1)[2]>1){
    #Removing "#" from Respondent IDS
    ls.resp=resp.var.data1[,1]
    if(length(grep("#",ls.resp))!=0){
      ls.resp=sub("#","",ls.resp)
    }
    resp.var.data1[,1]=ls.resp
    
    resp.var.data1=resp.var.data1[which(resp.var.data1[,1]%in%names(mat.cit.abs.pres)),]
    res=resp.var.data1
    
    ls.rm<-numeric()
    for(i in 2:dim(res)[2]){
      res[,i]<-factor(res[,i])
      if(length(levels(res[,i]))<2) ls.rm[length(ls.rm)+1]<-i
    }
    if(length(ls.rm)!=0) res<-res[,-ls.rm]
    
    #looking for missing informants in the uploaded respondent variable file
    ls.missing.resp=which(!names(mat.cit.abs.pres)%in%ls.resp)
    
    if(length(ls.missing.resp)!=0){
      ls.missing.resp=paste(names(mat.cit.abs.pres)[ls.missing.resp],collapse=", ")
      res=paste(
       "Results don't appear because the following informants are missing from your uploaded respondent variables table: ",
       paste("<b>",ls.missing.resp,"</b>",sep=""),
       "Please add these informants (if informantion is missing for these informants write 'NA' in cells concerning them) and proceed to upload your table once again.",
       sep="<br/>"
       )
    }
  }
  return(res)
}
