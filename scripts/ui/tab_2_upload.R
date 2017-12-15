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

tabPanel("Upload",
         titlePanel("Upload file containing free lists"),
         sidebarLayout(
           sidebarPanel(
             conditionalPanel("!output.enable_UI_main",
                              HTML("<h4>To continue to use FLARES, please <b>submit</b> your email adddress in the <b>'Introduction'</b> sidepanel.</h4>")),
             conditionalPanel("output.enable_UI_main",               
                              selectInput("input.format.choice",
                                          label="Select your file format",
                                          choices = list("Lists by columns (FLAME)"="FLAME",
                                                         "Spreadsheet format (AnthrTools)"="ATOOLS",
                                                         "ANTHROPAC format"="APAC"),
                                          selected="FLAME"
                              ),#end selectInput
                              #checkboxInput("apac.format.checkbox", label = "ANTHROPAC formatted file"),
                              p("For the selected file format a data format example is provided on the right hand side of your screen."),
                              fluidRow(
                                column(2),
                                column(10,htmlOutput("check.header"))#end column
                              ),#end fluidRow
                              fluidRow(
                                column(2),
                                column(10,htmlOutput("panel2.sidebar.text1"))
                              ),#end fluidRow
                              tags$hr(),
                              htmlOutput("check.apac.item.categories"),
                              htmlOutput("panel2.sidebar.text2"),
                              tags$hr(),
                              fileInput('Uploaded.file1','Choose your CSV file', 
                                        accept=c('text/csv',
                                                 'text/comma-seperated-values',
                                                 'text/plain',
                                                 '.csv')
                              ),#fileInput
                              tags$hr(),
                              fluidRow(
                                column(6,
                                       radioButtons('sep','Seperator',
                                                    c(Semicolon=';',
                                                      Comma=',',
                                                      Tab='\t')
                                       )#radioButtons
                                ),#end column
                                column(6,
                                       radioButtons('quote','Quote',
                                                    c(None='',
                                                      'Double Quote'='"',
                                                      'Single Quote'="'")
                                       )#radioButtons
                                )#end column
                              )#end fluidrow
             )#end condtionalPanel
           ),#sidebarPanel
           mainPanel(
             conditionalPanel("output.enable_UI_main",
                              tabsetPanel(type="tabs",
                                          tabPanel("Uploading data",
                                                   br(),
                                                   htmlOutput("error.mess1"),
                                                   fluidRow(
                                                     column(6,htmlOutput("show.sample.text")),
                                                     column(6,htmlOutput("show.sample1"))
                                                   ),
                                                   htmlOutput("duplicates"),
                                                   br(),
                                                   tableOutput('show.FL.data1')
                                          ),#tabPanel
                                          tabPanel("Item categorical information",
                                                   br(),
                                                   fluidRow(
                                                     column(6,htmlOutput("panel2.tab2.text1")),
                                                     column(2,
                                                            br(),
                                                            htmlOutput("show.download.apac.categ.tab")
                                                     ),#end column
                                                     column(4,
                                                            htmlOutput("show.csvtype6")
                                                     )#end column
                                                   ),#end fluidRow
                                                   tags$hr(),
                                                   DT::dataTableOutput("show.apac.categ.tab")
                                          )#end tabPanel
                              )#end tabsetPanel
             )#endConditionalPanel
           )#mainPanel
         )#sidebarLayout
)#tabPanel