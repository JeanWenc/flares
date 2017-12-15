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
library(leaflet)

shinyUI(
  navbarPage("FLARES - Free List Analysis under R Environment using Shiny",selected = "Introduction",

  source(file = "scripts/ui/tab_1_intro.R",local=TRUE)$value, 
  
  source(file="scripts/ui/tab_2_upload.R",local=TRUE)$value,  
  
  source(file="scripts/ui/tab_3_norm.R",local=TRUE)$value,
  
  source(file="scripts/ui/tab_4_item_analyses.R",local=TRUE)$value,
  
  source(file="scripts/ui/tab_5_resp_analyses.R",local=TRUE)$value,
             
  div(HTML("<div align=right style=margin-bottom:0px;padding-top:0px;padding-bottom:3px;position:fixed;border-top-left-radius:2px;bottom:0;right:0;background-color:LightGray;> <span style=font-size:70%;color:gray;margin-top:0px;margin-right:3px;margin-left:3px;vertical-align:middle;>Copyright (C) 2017 Jean Wencélius - GNU License - AGPLv3 </span></div>"))
))
