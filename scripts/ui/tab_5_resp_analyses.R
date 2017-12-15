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
#####################################################################################################################
#####UPLOAD####           
                 tabPanel("Upload",
                          br(),
                          htmlOutput("panel5.tab1.text1"),
                          fluidRow(
                            column(4,tableOutput("show.res.sum.resp.var")),
                            column(2,htmlOutput("show.download.resp.sample.tab")),
                            column(4,htmlOutput("show.csvtype7"))
                          )#endFluidRow
                 ),#End tabPanel

#####################################################################################################################
#####INFORMANT COMPETENCE####
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
                            ),#tabPanel
                            tabPanel("Methods",
                                     HTML(paste("<h4>Informant competence and cultural centrality</h4>",
                                                "<p>Several indicators computed by FLARES allow to measure each respondent's competence or cultural centrality:</p>",
                                                "<ul>",
                                                "<li><b>List length</b>: number of items mentioned by a respondent.</br></br></li>",
                                                "<li><b>Summed frequency of mentioned items</b>: sum of the overall frequency of mention (across all lists) of the items mentioned by a given respondent.</br>",
                                                "<i>N.B. The relation between 'list length' and 'summed frequency of mentioned items' is thought to be linear for culturally central respondents (see 'Informant Competence'/'Chart' sub-tab). Outliers or specialists (i.e. those mentioning a lot of items cited by few other informants) should present a low 'average frequency of mentioned items'.</i></br></br></li>",
                                                "<li><b>Rank to Frequency correlation</b>: for a given individual it is the value of the R<sup>2</sup> coefficient of the Pearson correlation between mentioned items' rank (within the resondent's list) and items' overall frequency (across lists).</br>",
                                                "<i>N.B. Culutrally competent or central respondents should present a strong negative correlation coefficient (i.e. they would mention early in their list the most frequently cited items), total outliers should present a strong positive correlation coefficient (i.e. they mention early in their list items mentioned by very few respondents). A negative or positive coefficient close to zero indicates the absence of linear relationship between rank of mention and frequency of citation.</i></li>",
                                                "</ul>",
                                                sep="")),
                                     br()
                                     )#end tabPanel "Methods"
                          ) #End tabsetPAnel
                 ),#End tabPanel
                 
#####################################################################################################################
#####RESP PROXIMITY####
                 tabPanel("Respondent Proximity",
                          tabsetPanel(selected="Resp. Prox. Plot",
                                      type="pills",
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
                                      ),#end tabPanel
                                      
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
                                                                HTML("For more details on these analyses, please refer to the 'Methods' sub-tab.<br/>"),
                                                                br(),
                                                                br()
                                               )#end conditionalPanel
                                      ),#endTabPanel
                                      tabPanel("Methods",
                                               br(),
                                               HTML("<p>The comparison of patterns of responses (in terms of presence or absence of mentioned items) across respondents allows for the <b>exploration of inter-respondent proximity</b> and for <b>testing for significant variations between sub-groups of respondents</b> (sub-groups of any given respondent variable uploaded by user).</p>"),
                                               HTML(paste("<div class='divShadow'>",
                                                          "<h4>Estimating respondent pairwise similarity</h4>",
                                                          "<p>Respondent similarity or proximity is estimated through the use of the <b>Jaccard Index</b> to transform a rectangular binary respondent by item matrix (the value 1 indicates that resondent <i>i</i> has cited item <i>j</i>) into a square respondent by respondent distance matrix (dissimilarity matrix).</br></br>",
                                                          "The formula for the <b>Jaccard index</b> is the following:</br></p>",
                                                          "<img src='jaccardFormula.jpg' width='200px'></br>",
                                                          "<p>With:</br>",
                                                          "<b><i>M<sub>a<sub>1</sub>b<sub>0</sub></sub></i></b>: Number of items which appear in the list of respondent <i>a</i> and not in the list of respondent <i>b</i>.</br>",
                                                          "<b><i>M<sub>a<sub>0</sub>b<sub>1</sub></sub></i></b>: Number of items which do not appear in the list of respondent <i>a</i> and do appear in the list of respondent <i>b</i>.</br>",
                                                          "<b><i>M<sub>a<sub>1</sub>b<sub>1</sub></sub></i></b>: Number of items which appear in the list of respondent <i>a</i> and appear in the list of respondent <i>b</i>.</br>",
                                                          "</p></div>",
                                                          sep="")),
                                               HTML(paste("<div class='divShadow'>",
                                                          "<h4>Between class analysis</h4>",
                                                          "<p>When user uploads respondent variables (e.g. such as gender, age, residence etc.) FLARES offers the possibility to test for significant differences between sub-groups of respondents (e.g. between males and females).</br>",
                                                          "<b>The data used for the test is the square repondent-by-respondent distance matrix described in the box above and the respondent variables uploaded by user in the 'Respondent Analyses'/'Upload' sub-tab.</b></br></br>",
                                                          "The test is carried out in two steps (results are displayed in the 'Between Class Analysis' sub-tab):</p>",
                                                          "<ul>",
                                                          "<li><b>1. Checking for inter-group homogeneity of intra-group dispersion</b>: for each respondent variable provided by user, an analysis of multivariate homogeneity of intra-group dispersion is carried out.</br>",
                                                          "<span class='packColor'>>>'betadisper' function of the <a href='https://cran.r-project.org/web/packages/vegan/index.html' target='_blank'>vegan</a> R package</span>.</br>",
                                                          "<i>The test's null hypothesis is that between groups there is no difference in levels of intra-group dispersion. In consequence, homogeneity is verified when the test is <b>not significant.</br>", 
                                                          "Only variables for which homogeneity is verified are used in the next step of the analysis</b></i>.</br></br></li>",
                                                          "<li><b>2. Testing for significant differences between groups</b>: a permutational mutlivariate analysis of variance using distance matrices (perMANOVA) is carried out on all respondent variables which intra-group dispersion is homogeneous.</br>",
                                                          "<span class='packColor'>>>'adonis' function of the <a href='https://cran.r-project.org/web/packages/vegan/index.html' target='_blank'>vegan</a> R package</span>.</li>",
                                                          "</ul>",
                                                          "<p><b>Significant differences in patterns of responses between sub-groups of a respondent variable is verified for variables with a significant p-value at the perMANOVA (see 'Pr(>F)' column of the second table in the 'Between class analysis' sub-tab)</b>.</p>",
                                                          "<p><b>N.B. The results only indicate whether or not there are significant differences between sub-groups, it does not indicate what the differences are. In order to explore how different the patterns of responses are, users may refer to the 'Respondent Analyses'/'Items' saliency' sub-tab in which items' cultural salience scores are broken down by sub-groups of respondents.</b></p>",
                                                          "</div>",
                                                          sep="")),
                                               HTML(paste("<div class='divShadow'>",
                                                          "<h4>Plotting respondent-by-respondent proximity</h4>",
                                                          "<p>From the respondent-by-respondent distance matrix (see first box of this page), a Principal Coordinates Analysis (PCoA) is performed in order to generate a two-dimensional plot representing respondent-by-respondent proximity.</br>",
                                                          "<span class='packColor'>>>'dudi.pco' function of the <a href='https://cran.r-project.org/web/packages/ade4/index.html' target='_blank'>ade4</a> R package</span></p>",
                                                          "<p>When user uploads respondent variables, a drop-down list allows user to choose the variable for which sub-groups should be mapped out on the PCoA plot.</p>",
                                                          "</div>",
                                                          sep="")),
                                               br()
                                      )#endTabPanel
                          )#end tabsetPanel
                 ),#end tabPanel

#####################################################################################################################
#####ITEMS' SALIENCY####
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
                 )#end tabPanel
               )#tabsetPanel
             )#conditionalPanel
           )#mainPanel
         )#SideBarPanel
)#tabPanel