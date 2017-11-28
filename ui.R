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
  #####################################################################################################################
  #####INTRODUCTION
  #####################################################################################################################
  source(file = "scripts/ui/tab_1_intro.R",local=TRUE)$value, 
  
  #####################################################################################################################
  #####UPLOAD
  #####################################################################################################################
    tabPanel("Upload",
             tags$head(HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-108158721-2'></script>")),
             tags$head(includeScript("analytics/google-analytics.js")),
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }",
                        ".divShadow {width:80%;margin:25px auto; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);padding:10px;}"
                        ),
             
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
             ),#tabPanel
  
  #####################################################################################################################
  #####NORMALIZE DATA
  #####################################################################################################################
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
                                                                                  column(4,
                                                                                         htmlOutput("select.categ.type")
                                                                                         ),#Column
                                                                                  column(8,htmlOutput("categ.warning"),
                                                                                         fluidRow(column(4,br(),htmlOutput("show.download.pb.items.tab")),#end column
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
                                                               tableOutput("show.sample4")
                                                               )#tabPanel
                                                      
                                                      
                                                      )#tabsetPanel
                                          )#conditionalPanel
                         )#MainPanel
                       )#sideBarLayout
                     ),#tabPanel
  
  #####################################################################################################################
  #####ITEM ANALYSES
  #####################################################################################################################
             tabPanel("Item Analyses",
                      titlePanel("Free-List Analyses"),
                      sidebarLayout(
                        sidebarPanel(
                          br(),
                          h4(textOutput("panel2.text1")),
                          conditionalPanel("output.enable_UI",
                                           h4("Dataset summary"),
                                           htmlOutput("fl.analysis.sum"),
                                           tags$hr(),
                                           h4("Download Item-by-Resp Matrix"),
                                           fluidRow(
                                             column(8,
                                                    htmlOutput("show.select.item.resp.matrix"),
                                                    htmlOutput("show.csvtype.item.resp.mat")
                                                    ),
                                             column(4,br(),br(),
                                                    htmlOutput("show.download.item.resp.matrix"))
                                             
                                           )
                                           
                          )#conditionalPanel
                        ),#SidebarPanel
                        mainPanel(
                          conditionalPanel("output.enable_UI",
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Cultural Saliency",
                                                                tabsetPanel(selected="Table",
                                                                            type="pills",
                                                                            tabPanel("Table",
                                                                                     br(),
                                                                                     h4("Download Free-List Results Table"),
                                                                                     fluidRow(
                                                                                       column(2,
                                                                                              br(),
                                                                                              downloadButton('download.FL.results','Download')),
                                                                                       column(4,
                                                                                              radioButtons('csvtype2','Choose CVS Format',
                                                                                                           c("Semicolon (French)"=2,
                                                                                                             "Comma (English)"=1))
                                                                                              )#end column
                                                                                       ),#end fluidRow
                                                                                     br(),
                                                                                     br(),
                                                                                     DT::dataTableOutput('fl.analysis.table')
                                                                                     ),#tabPanel
                                                                            tabPanel("Chart",
                                                                                     br(),
                                                                                     fluidRow(
                                                                                       column(3,
                                                                                              br(),
                                                                                              br(),
                                                                                              sliderInput("freq.slider",
                                                                                                          label="Select range of frequency of mention (in %)",
                                                                                                          0,100,
                                                                                                          dragRange=TRUE,
                                                                                                          value=c(0,100),
                                                                                                          ticks=TRUE,
                                                                                                          width = '100%'),#SliderInput
                                                                                              br(),
                                                                                              radioButtons('sort',"Select how you wish to order the chart",
                                                                                                           c('Alphabetically'=1,
                                                                                                             'Frequency of mention'=3,
                                                                                                             'Smith Index'=5,
                                                                                                             'Sutrop Index'=6)),#radioButtons
                                                                                              br(),
                                                                                              checkboxGroupInput("salience.ind.checkbox", 
                                                                                                                 label = "Select which salience index to display:", 
                                                                                                                 choices = list("Frequencey of Citation" = "freq.cit.rel", 
                                                                                                                                "Smith Index" = "Smith.index", 
                                                                                                                                "Sutrop Index" = "Sutrop.index"),
                                                                                                                 selected = c("freq.cit.rel","Smith.index","Sutrop.index")),
                                                                                              br(),
                                                                                              sliderInput("text.size.FL.chart",
                                                                                                          label="Select label size",
                                                                                                          6,16,
                                                                                                          value=10,
                                                                                                          ticks=TRUE),
                                                                                              
                                                                                              strong("Download Cultural Salience Chart"),
                                                                                              br(),
                                                                                              downloadButton('download.FL.plot', 'Download')
                                                                                              ),#end column
                                                                                       column(9,plotOutput('fl.analysis.chart'))
                                                                                       )#end fluidRow
                                                                                     )#tabPanel
                                                                )#end tabsetPanel
                                                       ),#end tabPanel
                                                       
                                                       tabPanel('Item by Item Proximity',
                                                                br(),
                                                                fluidRow(
                                                                  column(4,
                                                                         radioButtons("item.prox.index",
                                                                                      label="Select the proximity index you wish to apply",
                                                                                      c('Successive count',
                                                                                        'Henley index')
                                                                                      )#RadiotButtons
                                                                         ),#column
                                                                  column(4,htmlOutput("select.item.prox.plot.type")),
                                                                  column(4,strong("Download Item-by-Item Prox. Matrix"),
                                                                         downloadButton("download.item.prox.mat","Download"),
                                                                         radioButtons("csvtype.item.prox.mat",label =NULL,
                                                                                                 c("Semicolon (FR)"=2,
                                                                                                   "Comma (UK/US)"=1),inline = T))
                                                                  ),#end fluidRow
                                                                tags$hr(),
                                                                htmlOutput("show.download.tree.part.text0"),
                                                                br(),
                                                                fluidRow(
                                                                  column(4, htmlOutput("select.dendo.n")),
                                                                  column(4,br(),htmlOutput("show.tree.partition")),
                                                                  column(4,
                                                                         htmlOutput("show.download.tree.part.text1"),
                                                                         br(),
                                                                         fluidRow(
                                                                           column(4,br(),htmlOutput("show.download.tree.part")),
                                                                           column(8,htmlOutput("show.csvtype.tree.part"))
                                                                           )#end fluidRow
                                                                         )#end column
                                                                  ),#end fluidRow
                                                                tags$hr(),
                                                                fluidRow(
                                                                  column(3,
                                                                         br(),
                                                                         br(),
                                                                         htmlOutput("select.item.categories"),
                                                                         sliderInput("freq.slider.item.prox",
                                                                                     label="Select range of frequency of mention (in %)",
                                                                                     0,100,
                                                                                     dragRange=TRUE,
                                                                                     value=c(0,100),
                                                                                     ticks=TRUE,
                                                                                     width = '100%'),#SliderInput
                                                                         strong("Resize plot labels: "),
                                                                         sliderInput(
                                                                           "resize.item.prox.labels",
                                                                           label="",
                                                                           0,100,
                                                                           dragRange=FALSE,
                                                                           value=70,
                                                                           ticks=TRUE,
                                                                           width = '100%'
                                                                           ),#end slider input
                                                                         strong("Download Item by Item Proximity Plot"),
                                                                         br(),
                                                                         downloadButton('download.show.item.prox','Download')
                                                                         ),#end column
                                                                  column(9,plotOutput('show.item.prox'))
                                                                  )#end fluidRow
                                                                ),#end tabPanel
                                                       tabPanel("Item categories analysis",
                                                                tabsetPanel(type="pills",
                                                                            tabPanel("Dichot. Bias",
                                                                                     br(),
                                                                                     htmlOutput("dichot.bias.mess1"),
                                                                                     br(),
                                                                                     fluidRow(
                                                                                       column(3,htmlOutput("select.categ.dichot")),
                                                                                       column(3,htmlOutput("select.resp.var.name.dichot")),
                                                                                       column(1),
                                                                                       column(2,br(),htmlOutput("show.download.dichot.bias")),
                                                                                       column(3,htmlOutput("show.csvtype.dichot.bias"))
                                                                                     ),#end fluidRow
                                                                                     br(),
                                                                                     tableOutput("show.dichot.bias"),
                                                                                     htmlOutput("dichot.bias.mess2"),
                                                                                     tags$hr(),
                                                                                     htmlOutput("dichot.bias.mess3"),
                                                                                     br(),
                                                                                     fluidRow(
                                                                                       column(2,br(),htmlOutput("show.download.dichot.bias.tot")),
                                                                                       column(3,htmlOutput("show.csvtype.dichot.bias.tot"))
                                                                                     )#end fluidRow
                                                                                     ),#end tabPanel
                                                                            tabPanel("Clustering",
                                                                                     br(),
                                                                                     htmlOutput("clustering.mess1"),
                                                                                     br(),
                                                                                     fluidRow(
                                                                                       column(3,htmlOutput("select.categ.clust")),
                                                                                       column(3,htmlOutput("select.resp.var.name.clust")),
                                                                                       column(1),
                                                                                       column(2,br(),htmlOutput("show.download.clustering")),
                                                                                       column(3,htmlOutput("show.csvtype.clustering"))
                                                                                     ),#end fluidRow
                                                                                     br(),
                                                                                     tableOutput("show.clustering"),
                                                                                     htmlOutput("clustering.mess2"),
                                                                                     tags$hr(),
                                                                                     htmlOutput("clustering.mess3"),
                                                                                     br(),
                                                                                     fluidRow(
                                                                                       column(2,br(),htmlOutput("show.download.clustering.tot")),
                                                                                       column(3,htmlOutput("show.csvtype.clustering.tot"))
                                                                                       )#end fluidRow
                                                                                     )#end TabPanel
                                                                            )#endtabsetPanel
                                                                ),#EndTabPanel
                                                       tabPanel("Data Saturation",
                                                                br(),
                                                                strong(textOutput('data.sat.text')),
                                                                br(),
                                                                strong("Download Data Saturation Plot"),
                                                                br(),
                                                                downloadButton('download.data.saturation','Download'),
                                                                br(),
                                                                plotOutput('data.saturation')
                                                                )#tabPanel
                                                       )#tabsetPanel
                                           )#conditionalPanel
                          )#mainPanel
                        )#SidebarLayout
                      ),#tabPanel
  
  #####################################################################################################################
  #####RESPONDENT ANALYSIS
  #####################################################################################################################
  tabPanel("Respondent Analyses",
           titlePanel("Respondent Analyses"),
           sidebarLayout(
             sidebarPanel(
               br(),
               h4(textOutput("panel5.sidebar.text1")),
               conditionalPanel(
                 "output.enable_UI",
                 h3("Upload your file with respondent variables"),
                 fileInput('Uploaded.file2','Choose your CSV file', 
                           accept=c('text/csv',
                                    'text/comma-seperated-values',
                                    'text/plain',
                                    '.csv')
                 ),#fileInput
                 tags$hr(),
                 checkboxInput('header2','Header',TRUE),
                 fluidRow(
                   column(6,
                          radioButtons('sep2','Seperator',
                                       c(Semicolon=';',
                                         Comma=',',
                                         Tab='\t')
                                       )#end radioButtons
                          ),#end column
                   column(6,
                          radioButtons('quote2','Quote',
                                       c(None='',
                                         'Double Quote'='"',
                                         'Single Quote'="'")
                                       )#end radioButtons
                          )#end column
                   ),#end fluidRow
                 tags$hr(),
                 htmlOutput("select.resp.var.name")
                 )#conditionalPanel
             ),#SidebarPanel
             mainPanel(
               conditionalPanel(
                "output.enable_UI",
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Sample Distribution",
                            br(),
                            htmlOutput("panel5.tab1.text1"),
                            fluidRow(
                              column(4,tableOutput("show.res.sum.resp.var")),
                              column(2,htmlOutput("show.download.resp.sample.tab")),
                              column(4,htmlOutput("show.csvtype7"))
                                     )#endFluidRow
                            ),#End tabPanel
                   
                   tabPanel("Informant Competence",
                            tabsetPanel(
                              type="pills",
                              tabPanel("Table Results",
                                       br(),
                                       strong("Download Respondent Results"),
                                       br(),br(),
                                       fluidRow(
                                         column(2,
                                                br(),
                                                downloadButton('download.resp.anal.res.tab', 'Download')
                                         ),#End column
                                         column(2,
                                                radioButtons('csvtype3','Choose CVS Format',
                                                             c(Semicolon_Fr=2,
                                                               Comma_US_UK=1)
                                                )#radioButtons
                                         )#End column
                                       ),#End fluidRow
                                       br(),
                                       DT::dataTableOutput("resp.anal.res.tab")
                                       ),#tabPanel
                              
                              tabPanel("Chart",
                                       br(),
                                       strong("Download Respondent Competence Chart"),
                                       br(),
                                       downloadButton('download.resp.anal.res.chart', 'Download'),
                                       br(),
                                       br(),
                                       plotOutput("resp.anal.res.chart")
                                       )#tabPanel
                              ) #End tabsetPAnel
                            ),#End tabPanel
                            
                   
                   tabPanel("Respondent Proximity",
                            tabsetPanel(selected="Resp. Prox. Plot",
                              type="pills",
                              tabPanel("Methods",
                                       br()
                                       ),#endTabPanel
                              tabPanel("Between class analysis",
                                       br(),
                                       htmlOutput("panel5.tab3b.text1"),
                                       conditionalPanel('output.enable_UI4',
                                                        HTML("<br/><b>The table below presents the results (for each respondent variables) of homogeneity of intra-group dispersion across groups.</b><br/>"),
                                                        HTML("<i>Homogeneity of dispersion is verified only for variables that have a p-value <b>ABOVE</b> 0.05.</i><br/><br/>"),
                                                        fluidRow(
                                                          column(7,tableOutput("show.disper.tab")),
                                                          column(2,
                                                                 br(),
                                                                 br(),
                                                                 downloadButton('download.disper.tab', 'Download')
                                                                 ),#endColumn
                                                          column(3,
                                                                 br(),
                                                                 radioButtons('csvtype8','Choose CVS Format',
                                                                               c(Semicolon_Fr=2,
                                                                                 Comma_US_UK=1)
                                                                               )#radioButtons
                                                                )#endColumn
                                                          ),#endFluidRow
                                                        HTML("<b>Only the variables for which homogeneity of dispersion is verified are used for the Mulivariate Analysis of Variance presented in the second table (below).</b>"),
                                                        tags$hr(),
                                                        HTML("<b>The table below presents the results of the Multivariate Analysis of Variance for each respondent variable presenting a homogeneous intra-group dispersion.</b><br/>"),
                                                        HTML("<i>For the variables which have a p-value <b>BELOW</b> 0.05 variation across groups can be considered as significantly higher than variation among groups.</i><br/><br/>"),
                                                        fluidRow(
                                                          column(7,tableOutput("show.adonis.tab")),
                                                          column(2,
                                                                 br(),
                                                                 br(),
                                                                 downloadButton('download.adonis.tab', 'Download')
                                                                 ),#endColumn
                                                          column(3,
                                                                 br(),
                                                                 radioButtons('csvtype9','Choose CVS Format',
                                                                               c(Semicolon_Fr=2,
                                                                                 Comma_US_UK=1)
                                                                               )#radioButtons
                                                                )#endColumn
                                                        ),#endFluidRow
                                                        HTML("For more details on these analyses, please refer to the 'Methods' sub-tab.<br/>")
                                                        )#end conditionalPanel
                                       ),#endTabPanel
                              tabPanel("Resp. Prox. Plot",
                                       br(),
                                       conditionalPanel("output.enable_UI2",
                                                        htmlOutput("show.adonis.sum.res")
                                                        ),#endContionalPanel
                                       tags$hr(),
                                       fluidRow(
                                         column(3,
                                                br(),
                                                sliderInput('select.freq.slider3',
                                                            "Select range of items' frequency of mention (in %)",
                                                            0,100,
                                                            dragRange=TRUE,
                                                            value=c(0,100),
                                                            ticks=TRUE),
                                                br(),
                                                strong("Download Respondent by Respondent Proximity Plot"),
                                                br(),
                                                downloadButton('download.show.plot.resp.prox','Download')
                                                ),#end column
                                         column(9,
                                                plotOutput("show.plot.resp.prox")
                                                )#end column
                                         )#end fluidRow
                                       )#end tabPanel
                              )#end tabsetPanel
                            ),#end tabPanel
                   
                   tabPanel("Items' saliency",
                            tabsetPanel(
                              type="pills",
                              tabPanel("Table Results",
                                       br(),
                                       htmlOutput("panel5.tab4a.text1"),
                                       conditionalPanel("output.enable_UI2",
                                                        strong("Download Free-List Analysis with Respondent Variables"),
                                                        br(),
                                                        br(),
                                                        fluidRow(column(2,
                                                                 br(),
                                                                 downloadButton('download.show.res.FL.resp.var', 'Download')
                                                                 ),#end column
                                                                 column(2,
                                                                        radioButtons('csvtype4','Choose CVS Format',
                                                                                     c(Semicolon_Fr=2,
                                                                                       Comma_US_UK=1)
                                                                        )#radioButtons
                                                                 )#end column
                                                        ),#end fluidRow
                                                        br(),
                                                        br(),
                                                        DT::dataTableOutput("show.res.FL.resp.var"),
                                                        br()
                                                        )#end ConditionalPanel,
                                       
                                       ),#end tabPanel
                              tabPanel("Chart",
                                       br(),
                                       htmlOutput("panel5.tab4b.text1"),
                                       conditionalPanel("output.enable_UI2",
                                                        fluidRow(
                                                          column(3,
                                                                 br(),
                                                                 sliderInput('select.freq.slider2',
                                                                               "Select range of frequency of mention (in %)",
                                                                               0,100,
                                                                               dragRange=TRUE,
                                                                               value=c(0,100),
                                                                               ticks=TRUE),
                                                                 br(),
                                                                 radioButtons('select.sal.index',
                                                                              "Select Item Salience index to plot",
                                                                              c('Frequency of mention'='Frequency',
                                                                                'Smith index'='Smith',
                                                                                'Sutrop index'='Sutrop')),
                                                                 br(),
                                                                 htmlOutput("select.resp.var.name.mod"),
                                                                 br(),
                                                                 htmlOutput("select.text.size.FL.resp"),
                                                                 br(),
                                                                 strong("Download Free-List Analysis Chart with Respondent Variables"),
                                                                 br(),
                                                                 downloadButton('download.show.res.FL.resp.var.chart','Download')
                                                                 ),#end Column
                                                          column(9,
                                                                 plotOutput("show.res.FL.resp.var.chart")
                                                                 )#end column
                                                          )# end FluidRow
                                                        )#end conditionalPanel
                                       )#end tabPanel
                              )# end tabsetPanel
                            ),#end tabPanel
                   tabPanel("Data Format Example",
                            br(),
                            strong("Data concerning respondent variables should be uploaded as a .csv file."),
                            p("Your data should be formatted as follows:"),
                            br(),
                            tableOutput("show.sample3")
                            )#tabPanel
                   )#tabsetPanel
               )#conditionalPanel
             )#mainPanel
           )#SideBarPanel
  ),#tabPanel
  div(HTML("<div align=right style=margin-bottom:0px;padding-top:0px;padding-bottom:3px;position:fixed;border-top-left-radius:2px;bottom:0;right:0;background-color:LightGray;> <span style=font-size:70%;color:gray;margin-top:0px;margin-right:3px;margin-left:3px;vertical-align:middle;>Copyright (C) 2017 Jean Wencélius - GNU License - AGPLv3 </span></div>"))
))
