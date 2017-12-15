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

tabPanel("Normalization & Categorization",
         titlePanel("Normalization and Categorization"),
         sidebarLayout(
           sidebarPanel(
             br(),
             h4(textOutput("panel3.sidebar.text1")),
             conditionalPanel("output.enable_UI",
                              h4("Upload a .csv file with normalisation"),
                              fileInput('Uploaded.file3','Choose your CSV file', 
                                        accept=c('text/csv',
                                                 'text/comma-seperated-values',
                                                 'text/plain',
                                                 '.csv')
                                        ),#fileInput
                              tags$hr(),
                              checkboxInput('header3','Header',TRUE),
                              fluidRow(
                                column(6,
                                       radioButtons('sep3','Seperator',
                                                    c(Semicolon=';',
                                                      Comma=',',
                                                      Tab='\t')
                                                    )#radioButtons
                                       ),#end column
                                column(6,
                                       radioButtons('quote3','Quote',
                                                    c(None='',
                                                      'Double Quote'='"',
                                                      'Single Quote'="'")
                                                    )#radioButtons
                                       )#end column
                                )#end fluidRow
                              )#ConditionalPanel
             ),#SidebarPanel
           mainPanel(
             conditionalPanel("output.enable_UI",
                              tabsetPanel(type="tabs",
                                          tabPanel("List of cited items",
                                                   br(),
                                                   htmlOutput("panel3.tab1.text1"),
                                                   conditionalPanel("output.enable_UI3",
                                                                    tags$hr(),
                                                                    fluidRow(
                                                                      column(4,htmlOutput("select.categ.type")),#Column                                                                      
                                                                      column(8,htmlOutput("categ.warning"),
                                                                             fluidRow(
                                                                               column(4,br(),htmlOutput("show.download.pb.items.tab")),#end column
                                                                               column(4,htmlOutput("show.csvtype5"))
                                                                               ),#end FluidRow
                                                                             htmlOutput("panel3.tab1.text3")
                                                                             )#end column
                                                                      ),#endfluidRow
                                                                    tags$hr(),
                                                                    fluidRow(
                                                                      column(4,htmlOutput("select.norm.type")),
                                                                      column(3,br(),br(),htmlOutput("select.apply.normalization"))
                                                                      )#FluidRow
                                                                    ),#Conditional Panel
                                                   tags$hr(),
                                                   fluidRow(
                                                     column(8,DT::dataTableOutput('show.cited.items.list')),
                                                     column(4,
                                                            htmlOutput("panel3.tab1.text2"),
                                                            htmlOutput("show.download.show.cited.items.list"),
                                                            br(),
                                                            htmlOutput("show.csvtype1")
                                                            )#end column
                                                     )#end fluidRow
                                                   ),#tabPanel
                                          
                                          tabPanel("Data Format Example",
                                                   br(),
                                                   strong("You must upload your items list with normalized names or item categories in a .csv or. txt file." ),
                                                   p(HTML("<i>If your data is in an Excel document you can save it as a .csv file before uploading.</i>")),
                                                   br(),
                                                   p("No cells should be empty."),
                                                   br(),
                                                   strong("Whether it be for normalization or categorization you may create as many columns as you wish (with different headers)."),
                                                   br(),
                                                   br(),
                                                   p("In the table below:"),
                                                   HTML("<ul>
                                                        <li>The first column contains the items as they are typed-in in the original data.</li>
                                                        <li>Columns 2 to 4 can be qualified as <b>'normalization columns'</b>.</li>
                                                        <li>Columns 5 and 6 can be considered as <b>'categorization columns'</b>.</li></ul>"),
                                                   tableOutput("show.sample4")
                                                   )#tabPanel
                                          )#tabsetPanel
                              )#conditionalPanel
             )#MainPanel
           )#sideBarLayout
         )#tabPanel