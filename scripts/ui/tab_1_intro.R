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

tabPanel("Introduction",
         
         tags$head(HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-108158721-2'></script>")),
         tags$head(includeScript("analytics/google-analytics.js")),
         tags$style(type="text/css",
                    ".shiny-output-error { visibility: hidden; }",
                    ".shiny-output-error:before { visibility: hidden; }",
                    ".divShadow {width:80%;margin:25px auto; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);padding:10px;}",
                    ".packColor {color:rgb(21,46,97);}"
                    ),#end tags$style
         
         titlePanel("Introduction"),
         
         sidebarLayout(
#########################SIDEBAR########################################################                                 
           sidebarPanel(
             h4("Tell us about yourself and your dataset!"),
             HTML("<p><strong>Submitting your email address is required to continue.</strong></br><i>All other fields are optional.</i></p>"),
             br(),
             fluidRow(
               column(6,textInput("user.email",label = HTML("<span style='color:red;'>*</span> Email address."),value="",placeholder = "john.doe@mymail.com")),
               column(6,htmlOutput("show.make.public"))
               
               ),#end fluidRow
            htmlOutput("show.submit.button"),#end column 
            tags$hr(),
            htmlOutput("show.user.name"),
            htmlOutput("show.user.inst"),
            fluidRow(
              column(6,htmlOutput("show.user.dataset")),
              column(6,htmlOutput("show.user.language"))
              ),#end fluidRow
            htmlOutput("show.user.country"),
            htmlOutput("show.map.text"),
            leafletOutput("map")
            ),#end sidebarPanel
          mainPanel(
            tabsetPanel(type="tabs",
#########################FLARES########################################################                      
                         tabPanel("FLARES",
                                  br(),
                                  HTML("<p><b>FLARES is an online, open-source software for free-list analyses.</b></p>"),
                                  HTML("<p>FLARES was developed to overcome some of the limitations of its direct ancestor <a href='http://www.mae.u-paris10.fr/lesc/spip.php?rubrique75' target='_blank'>FLAME</a> which is a set of VBA macros running under Microsoft Excel (Pennec et al., 2012).</p>"),
                                  HTML("<p>While maintaining the same philosophy - making free-list analysis as user-friendly as possible - FLARES offers:</p>"),
                                  HTML("<ul>
                                          <li style='margin:5px;'><b>An extended accessibility.</b> Web-based, you just need a web-browser and you can access FLARES from any operating system.</li>
                                          <li style='margin:5px;'><b>Regular updates.</b> Users are always sure to work with the latest version of FLARES as the application is regularly updated on the server.</li>
                                          <li style='margin:5px;'><b>Integrated statistical analyses.</b> While FLAME required the use of third-party software to conduct exploratory and multivariate analyses, the latter have been directly integrated into FLARES through the use of existing R packages (listed in the 'About' sub-tab).</li>
                                          <li style='margin:5px;'><b>A user-friendly and interactive interface.</b> The use of rStudio's shiny package allows for an interactive interface allowing user's to generate tables and aesthetic plots without ever modifying their original data.</li>
                                       </ul>"),
                                  
                                  br(),
                                  
                                  fluidRow(
                                    column(2),
                                    column(4,HTML("<p style='text-align:justify'>Please visit the other sub-tabs of this 'Introduction' to learn more about FLARES and how to use it.</p>")),
                                    column(4,HTML("<p style='text-align:justify'>If you are familiar with R and rStudio you may run FLARES locally on your computer by forking the application on <a href='https://github.com/JeanWenc' target='_blank'>GitHub</a>.</p>"))
                                    ),#end FluidRow
                                  
                                  br(),
                                  
                                  HTML("<div class='divShadow' style='display: flex;flex-wrap:wrap;align-items:center;'>
                                          <div style='width:100%;'><p style='text-align:center;'><b>The development of FLARES has been made possible through the support of:</b></p></div>
                                          
                                          <div style='width:20%;'></div>
                                          <div style='width:20%;'>
                                            <a href='http://www.fondationfyssen.fr/fr/' target='_blank'><img src='fyssen.jpg' width='100%'></a>
                                          </div>
                                          <div style='width:20%;'></div>
                                          <div style='width:20%;'>
                                            <a href='http://anr-piaf.org' target='_blank'><img src='piaf.png' width='100%'></a>
                                          </div>
                                          <div style='width:20%;'></div>
                                          
                                          <div style='width:15%;'></div>
                                          <div style='width:30%; padding:10px;'>
                                            <p style='font-size:80%;text-align:justify;'>FLARES' development was finalized in 2017 while the main author was funded by a post-doctoral grant awarded by the FYSSEN Foundation.</p>
                                          </div>
                                          <div style='width:10%;'></div>                                          
                                          <div style='width:30%;padding:10px;'>
                                            <p style='font-size:80%;text-align:justify;'>The idea of developing FLARES sprung within the international research program PIAF (Interdisciplinary Program on indigenous indicators of Fauna and Flora) during which large datasets of free-lists were collected in four different countries (Cameroon, France, USA and Zimbabwe).</p>
                                          </div>

                                       </div>"),
                                  br()
                                  ),#end tabPanel "FLARES"

#########################FREE-LISTING########################################################
                         tabPanel("Free-listing",
                                  br(),
                                  
                                  p(align="justify","Free-listing is a data collection task which was elaborated in the field of cognitive psychology in order to better understand the processes of semantic categorization. Its use has become widespread in the fields of cognitive anthropology, ethnobiology and socio-ecological studies. It is an elicitation technique by which informants are asked to cite - in written or oral form - all the items belonging to a specific super-ordinate semantic category (or cultural domain). A typical question engaging such an elicitation would be:"),
                                  
                                  p(align="center",em("Please cite, as they come to mind, all the insects that you know of.")),
                                  
                                  tags$hr(),
                                  
                                  p("This simple data collection technique may be used to:"),
                                  
                                  HTML("<ul>
                                          <li style='margin:10px;'>Explore the contents and structure of the investigated cultural domain by:</li>
                                            <ul>
                                              <li>Defining the semantic boundaries of the cultural domain.</li>
                                              <li>Uncovering the most culturally salient items of the domain (based on their frequency of mention across lists and their rank of citation within lists).
                                                See Smith & Borgatti (<a href='http://dx.doi.org/10.1525/jlin.1997.7.2.208' target='_blank'>1997</a>) and Sutrop (<a href='https://doi.org/10.1177/1525822X0101300303' target='_blank'>2001</a>).
                                              </li>
                                              <li>Estimating inter-item semantic proximity based on the position of any pair of items within respondents' lists.
                                                See Henley (<a href='https://doi.org/10.1016/S0022-5371(69)80058-7' target='_blank'>1969</a>) & Winkler-Rhodes et al. (<a href='http://dx.doi.org/10.1163/156853710X497248' target='_blank'>2010</a>).
                                              </li>
                                            </ul>
                                          <li style='margin:10px;'>Test for the existence of different patterns of response among groups of resondents by:</li>
                                            <ul>
                                              <li>Breaking down cultural salience results by categories of resondents (defined by user).</li>
                                              <li>Estimating respondents pairwise proximity based on the presence or absence of items within their lists.</li>
                                            </ul>
                                       </ul>"),
                                  
                                  br(),
                                  
                                  p("Furthemore, the free-listing task may be accompanied by follow-up interviews in which respondents are prompted to provide categorical information concerning the items they have mentioned."),
                                  
                                  HTML("<p>In such cases, statistical tests elaborated by Robbins and Nolan (<a href='http://dx.doi.org/10.1177/1525822x970090030501' target='_blank'>1997</a>, <a href='http://dx.doi.org/10.1177/1525822x0001200102' target='_blank'>2000</a>) enable to test whether respondents:</p>"),
                                  
                                  HTML("<ul>
                                        <li style='margin:10px;'>Present a bias in the order in which they mention items belonging to one category or the other (for dichotomous variables).</li>
                                        <li style='margin:10px;'>Tend to cluster items in their list based on the category mentioned items belong to.</li>
                                       </ul>"),
                                  
                                  tags$hr(),
                                  
                                  p(strong("References:")),
                                  
                                  HTML("<ul>
                                          <li><a href='http://medanth.wikispaces.com/Free+Lists' target='_blank'>Medical Anthropology Wiki - Free Lists</a></li>
                                          <li><a href='http://fmx.sagepub.com/' target='_blank'>Field Methods Journal</a></li>
                                          <li>Bernard, H. Russell. 2018. <i>Research Methods in Anthropology: Qualitative and Quantitative Approaches</i>. Sixth edition. Lanham, MD: Altamira Press.</li>
                                          <li>Borgatti, Stephen P. 1999. 'Elicitation techniques for cultural domain analysis.' In J. Schensul & M. LeCompte (Eds.), <i>Ethnographer's Toolkit</i>, pp.1-26. Walnut Creek: Altamira Press.</li>
                                          <li>D'Andrade, Roy G. 1995. <i>The development of cognitive anthropology</i>. Cambridge; New York: Cambridge University Press.</li>
                                          <li>Weller, Susan C. & A. Kimball Romney. 1988. <i>Systematic data collection</i>. Newbury Park: Sage Publications.</li>
                                       </ul>"),
                                  br()
                                  
                                  ),#end tabPanel "Free-listing"
#########################HOW TO?########################################################                         
                         tabPanel("How to?",
                                  br(),
                                  p(strong("Most instructions and references to the methods used by FLARES are built into the application and may be found in the different tabs of the application (sub-tabs with details are often named 'Methods').")),
                                  p("Below, are provided the few necessary instructions to help you start using FLARES."),
                                  
                                  div(class='divShadow',
                                      strong('1.'),
                                      br(),
                                      strong("Before you can start using FLARES, you must submit an email address in the sidepanel of the 'Introduction' tab."),br(),
                                      "If you wish to do so, you may submit other information concerning yourself (Name and institution) or your dataset.",br(),br(),
                                      p(em("You may choose to allow other FLARES' users to access the information you have provided in the 'Users across the globe' sub-tab."))
                                      ),#end div
                                  div(class="divShadow",
                                      strong('2.'),
                                      br(),
                                      HTML("FLARES requires users to <b>upload at least one file</b>:"),
                                      HTML("<ul><li style='margin:5px;'><b>In the 'Upload' tab</b>: you must upload a .csv file containing free-lists, the name/id of respondents and, eventually, categorical information for mentioned items, if provided by respondents. </br><i>Three different input formats are available and are illustrated in the 'Upload' tab.</i></li></ul>"),
                                      br(),
                                      HTML("To benefit from FLARES full capabilities you may also upload <b>two other optional files</b>:"),
                                      HTML("<ul>
                                              <li style='margin:5px;text-align:justify;'><b>In the 'Normalization & Categorization' tab</b>: you may upload a .csv file containing the unique list of all cited items, and as many supplementary columns in which original items may be corrected, translated (i.e. normalization columns) or categorized (i.e. categorization columns). </br><i>The required input format is illustrated in the 'Normalization & Categorization' tab</i>.</li>
                                              <li style='margin:5px;'><b>In the 'Respondent Analyses' tab</b>: you may upload a .csv file containing as many respondent variables as you wish. </br><i>The required input format is illustrated in the 'Respondent Analyses' tab</i>.</li>
                                           </ul>"),
                                      br(),
                                      p(align="justify",strong("N.B. Files that you upload while using FLARES are not stored on the server's hardrive. Uploaded files are used temporarily as long as your session runs. As soon as your session ends, all of your data is cleared from the server's cache memory. However, we do recommend that any data that you upload should be anonymized in order to respect your respondents' privacy."))
                                      
                                      ),# end div
                                  div(class="divShadow",
                                      strong("3."),
                                      br(),
                                      strong("General guidelines and tips:"),
                                      HTML("<ul>
                                              <li>You may generate your .csv files directly from Microsoft Excel or any other spreadsheet software.</li>
                                              <li>When possible, saving your .csv files into UTF-8 encoding is preferred.</li>
                                              <li>When possible, avoid special characters as well as periods, slashes or spaces (particularly in the column headings of your files).</li>
                                              <li>In the files you wish to upload do not leave header columns blank and do not give identical names to different columns.</li>
                                           </ul>")
                                      ),#end div
                                  p(align="center",strong("All set to go!")),
                                  br()
                                  ),#end tabPanel "User guide"
#########################USERS########################################################                         
                         tabPanel("Users across the globe",
                                  leafletOutput("userMap",width = "100%",height="550px"),
                                  absolutePanel(bottom = 30, left = 30,draggable = T,
                                                htmlOutput("show.select.domain")
                                                )#end absolutePanel
                                  ),#end tabPanel Users
#########################ABOUT########################################################                         
                         tabPanel("About",
                                  br(),
                                  div(class="divShadow",
                                      p(align="center",strong("FLARES - Free List Analysis under R Environment using Shiny"),br(),
                                      em("v 1.0")),
                                      br(),
                                      p(align="center",HTML("Fork us on GitHub and access the application's source code:</br><a href='https://github.com/JeanWenc' target='_blank'>GitHub Page</a>")),
                                      br(),
                                      p(align="center",strong("Citing FLARES"),br(),
                                      "A paper on FLARES is to be published in a peer-reviewed journal shortly.",br(),
                                      "Meanwhile, you can cite FLARES as follows:",br(),
                                      em("Wencelius, J., Garine, E., Raimond, C. 2017. FLARES. url:www.anthrocogs.com/shiny/flares/"))
                                  ),
                                  br(),
                                  p(align="center",strong("FLARES is placed under a GNU Affero General Public License v3.0")),
                                  column(12,align="center",verbatimTextOutput("licence"))
                         )#End tabPanel About
                         
             )#End tabsetPanel
             
           )#End MainPanel
           
         )# End sidebarLayout
         
)#End tabPanel  
