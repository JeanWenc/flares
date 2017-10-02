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

data.example2<-function(mat){
  Resp_ID=names(mat)
  sample=as.data.frame(Resp_ID)
  nb.resp=dim(sample)[1]
  
  arrond=round(nb.resp/2,0)
  
  
  Gender=rep(c("F","M"),arrond)
  Place_of_Birth=rep(c("Town1","Town2"),arrond)
  if (arrond!=nb.resp/2){
    Gender=c(Gender,"F")
    Place_of_Birth=c(Place_of_Birth,"Town1")
  }
  Etc...=rep("...",nb.resp)
  sample[,2:4]=list(Gender,Place_of_Birth,Etc...)
  names(sample)=c("Resp_ID","Gender","Place of Birth","Etc...")
  
  return(sample)
}