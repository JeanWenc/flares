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
#####################################################################################################################
#####CULTURAL SALIENCY####
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
                                                                                                'Sutrop Index'=6,
                                                                                                "B' score"=7)),#radioButtons
                                                                                 br(),
                                                                                 checkboxGroupInput("salience.ind.checkbox", 
                                                                                                    label = "Select which salience index to display:", 
                                                                                                    choices = list("Frequencey of Citation" = "freq.cit.rel", 
                                                                                                                   "Smith Index" = "Smith.index", 
                                                                                                                   "Sutrop Index" = "Sutrop.index",
                                                                                                                   "B' score"="B.score"),
                                                                                                    selected = c("freq.cit.rel","Smith.index","Sutrop.index","B.score")),
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
                                                                        ),#tabPanel
                                                               tabPanel("Methods",
                                                                        br(),
                                                                        HTML(paste("<p><b>Frequency of mention</b> across lists and <b>rank of citation</b> within lists are considered to be good indicators of items relative salience (Bousfield & Barclay <a href='http://dx.doi.org/10.1037/h0059019' target='_blank'>1950</a>).</p>",
                                                                                   "<p>Two cultural salience indices have been developed to combine these two indicators in order to score each item on a scale ranging from 0 to 1.</p>",sep="")),
                                                                        HTML(paste("<div class='divShadow'>",
                                                                                   "<h4>Smith Index</h4>",
                                                                                   "<p>The most popular index is <b>Smith's index</b> (Smith and Borgatti <a href='http://dx.doi.org/10.1525/jlin.1997.7.2.208' target='_blank'>1997</a>; Sutrop <a href='https://doi.org/10.1177/1525822X0101300303' target='_blank'>2001</a>).</p>",
                                                                                   "<img src='smithFormula.jpg' width='200px'>",
                                                                                   "<p>",
                                                                                   "With:</br>",
                                                                                   "<b>S<sub>a</sub></b>: Cultural salience of item <i>a</i></br>",
                                                                                   "<b>N</b>: Number of lists (number of respondents)</br>",
                                                                                   "<b>L<sub>i</sub></b>: Length of list <i>i</i></br>",
                                                                                   "<b>R<sub>ai</sub></b>: Citation rank of item <i>a</i> in list <i>i</i>",
                                                                                   "</p></div>",sep="")),
                                                                        HTML(paste("<div class='divShadow'>",
                                                                                   "<h4>Sutrop index</h4>",
                                                                                   "<p>Another index was developped by <b>Sutrop</b> (<a href='https://doi.org/10.1177/1525822X0101300303' target='_blank'>2001</a>) and is less sensitive than the previous with datasets containing lists which length strongly vary from one respondent to the other.</p>",
                                                                                   "<img src='sutropFormula.jpg' width='150px'>",
                                                                                   "<p>",
                                                                                   "With:</br>",
                                                                                   "<b>S<sub>a</sub></b>: Cultural salience of item <i>a</i></br>",
                                                                                   "<b>N</b>: Number of lists (number of respondents)</br>",
                                                                                   "<b>F</b>: Frequency of mention of the item across all lists</br>",
                                                                                   "and <b>mP<sub>a</sub></b>: mean rank of citation, with:</br>",
                                                                                   "<img src='sutropFormDet.jpg' width='150px'></br>",
                                                                                   "With:</br>",
                                                                                   "<b>r<sub>ai</sub></b>: rank of citation of item <i>a</i> in list <i>i</i>",
                                                                                   "</p></div>",sep="")),
                                                                        HTML(paste("<div class='divShadow'>",
                                                                                   "<h4>B' score</h4>",
                                                                                   "<p>This measure of cognitive salience, recently developped by <b>Robbins, Nolan and Chen</b> (<a href='https://doi.org/10.1177/1525822X17726726' target='_blank'>2017</a>), overcomes some shortcomings of the previous indices, notably:",
                                                                                   "<ul>",
                                                                                   "<li>The Smith index does not satisfactorily combine item list position and item frequency as the value of the index will be the same for an item listed once in final position and an item listed last by several respondents.</li>",
                                                                                   "<li>As for the Sutrop index, its minimal value never reaches 0 and the formula involves a computational error for items which are not listed (which is inconvenient when comparing the salience of items across different datasets or subgroups of informants).</li>",
                                                                                   "</ul>",
                                                                                   "</p>",
                                                                                   "<p>The B' score does vary between 0 and 1 and satisfactorily combines both item list position and item frequency.</p>",
                                                                                   "<img src='Bprime.jpg' width='250px'>",
                                                                                   "<p>",
                                                                                   "With:</br>",
                                                                                   "<b>B'<sub>a</sub></b>: Cultural salience of item <i>a</i></br>",
                                                                                   "<b>Z</b>: Number of lists (number of respondents)</br>",
                                                                                   "<b>F<sub>a</<ub></b>: Frequency of mention of item <i>a</i> across all lists</br>",
                                                                                   "and <b>B<sub>ai</sub></b>: proportion of items preceding item <i>a</i> in list <i>i</i> given by the following formula:</br>",
                                                                                   "<img src='Bscore.jpg' width='130px'></br>",
                                                                                   "With:</br>",
                                                                                   "<b>k<sub>i</sub></b>: length of list <i>i</i></br>",
                                                                                   "<b>r<sub>ai</sub></b>: rank of item <i>a</i> in list <i>i</i>",
                                                                                   "</p></div>",sep="")),
                                                                        br(),
                                                                        br()
                                                                        )#tabPanel
                                                               )#end tabsetPanel
                                                   ),#end tabPanel
#####################################################################################################################
#####ITEM BY ITEM PROXIMITY####
                                          tabPanel('Item by Item Proximity',
                                                   tabsetPanel(selected="Results",
                                                               type="pills",
                                                               tabPanel("Results",
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
                                                                                                "Comma (UK/US)"=1),inline = T)
                                                                                 )#column
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
                                                                        ),#end tabPanel Results
                                                               tabPanel("Methods",
                                                                        br(),
                                                                        HTML(paste("<p>When respondents elicit all the known items of a domain they tend to cite similar items in <b>clusters</b> within their lists (Bousfield <a href='https://doi.org/10.1080/00221309.1953.9710088' target='_blank'>1953</a>; Henley <a href='https://doi.org/10.1016/S0022-5371(69)80058-7' target='_blank'>1969</a>).</p>",
                                                                                   "<p>In consequence, comparing the ranks of citation of any pair of items offers an indication of their semantic proximity.</br>",
                                                                                   "<i>FLARES offers the possibily to calculate and plot inter-item proximity</i>.</p>",sep="")),
                                                                        HTML(paste("<div class='divShadow'>",
                                                                                   "<h4>Calculating inter-item proximity</h4>",
                                                                                   "<p>Two methods are used to estimate inter-item proximity.</br></br>",
                                                                                   "<b>1. Henley Index</b>: calculates the average difference of citation ranks for every pair of items (Henley <a href='https://doi.org/10.1016/S0022-5371(69)80058-7' target='_blank'>1969</a>),</br>",
                                                                                   "using the following formula:</p>",
                                                                                   "<img src='henleyFormula.jpg' width='250px'>",
                                                                                   "<p>With:</br>",
                                                                                   "<b>\u0394<sub>ab</sub></b>: Distance between item <i>a</i> and item <i>b</i></br>",
                                                                                   "<b>f</b>: Number of lists in which the pair <i>ab</i> was mentioned</br>",
                                                                                   "<b>l<sub>i</sub></b>: Length of list <i>i</i></br>",
                                                                                   "<b>r<sub>ai</sub></b>: Citation rank of item <i>a</i> in list <i>i</i></br>",
                                                                                   "<b>r<sub>bi</sub></b>: Citation rank of item <i>b</i> in list <i>i</i></br></br>",
                                                                                   "<b>The use of this formula results in the generation of a square item-by-item distance matrix.</b></br>",
                                                                                   "<i>This method, however, presents a drawback: two items appearing together in only one list one after the other will have the same distance score as a pair of items appearing in all lists one after the other.</br>",
                                                                                   "There are two ways to go around this:</i></p>",
                                                                                   "<ul>",
                                                                                   "<li>Discarding items that have been rarely cited (see box concerning plotting of inter-item proximity, below).</li>",
                                                                                   "<li>Using another method to estimate pairwise similarity ('Successive count' presented immediately below).</li>",
                                                                                   "</ul></br>",
                                                                                   "<b>2. Successive count</b>: counts across lists the number of times a pair of items is mentioned consecutively one after/before the other.</br>",
                                                                                   "The use of this method is inspired by the works of Brewer (<a href='https://doi.org/10.1016/0378-8733(93)90011-9' target='_blank'>1993</a>) and Romney et al. (<a href='https://doi.org/10.1111/j.1467-9280.1993.tb00552.x' target='_blank'>1993</a>).</br></br>",
                                                                                   "<b>It results in the generation of a square item-by-item contingency table in which cells represent the number of lists in which the corresponding pair of items is mentioned consecutively.</b></br>",
                                                                                   "<i>The values in the diagonal are equal to the overall frequency of mention of the corresponding item.</i>",
                                                                                   "</div>",sep="")),
                                                                        
                                                                        HTML(paste("<div class='divShadow'>",
                                                                                   "<h4>Plotting inter-item proximity</h4>",
                                                                                   "<p><b>1. Three methods are used to plot inter-item proximity.</b></p>",
                                                                                   "<ul>",
                                                                                   "<li><b>Correspondance Analysis</b>:</br>",
                                                                                   "As suggested by Weller and Romney (<a href='https://us.sagepub.com/en-us/nam/systematic-data-collection/book2418' target='_blank'>1990</a>) correspondance analysis is used to represent into a two-dimensional plot the inter-item proximity derived from the <b>'Successive count'</b> square contingency table.</br>",
                                                                                   "<span class='packColor'>>>'ca' function of the <a href='https://cran.r-project.org/web/packages/ca/index.html' target='_blank'>ca</a> R package</span></br></br></li>",
                                                                                   "<li><b>Multidimensional Scaling (MDS)</b>:</br>",
                                                                                   "MDS is used to represent inter-item proximity in a two-dimensional plot using the square item-by-item distance matrix computed with the <b>Henley Index</b>.</br>",
                                                                                   "<span class='packColor'>>>'cmdscale' function of the basic <a href='https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html' target='_blank'>stats</a> R package</span></br></br></li>",
                                                                                   "<li><b>Hierarchical Clustering Analysis (HCA - Dendrogram)</b>:</br>",
                                                                                   "HCA is also used to estimate (and plot as a dendrogram) inter-item proximity.</br>",
                                                                                   "<span class='packColor'>>>'hclust' function of the basic <a href='https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html' target='_blank'>stats</a> R package</span></br>",
                                                                                   "<span class='packColor'>>>plot generated with help of: 'dendro_data' function of the <a href='https://cran.r-project.org/web/packages/ggdendro/index.html' target='_blank'>ggdendro</a> R package</span></li>",
                                                                                   "<ul>",
                                                                                   "<li>When using the <b>Henley index</b> distance matrix, HCA is performed directly from the latter.</li>",
                                                                                   "<li>When using the <b>'Successive count'</b> contingency table, HCA is performed on a item-by-item distance matrix computed from the items' coordinates on the main factors of the correspondance analysis mentioned above.</li>",
                                                                                   "</ul>",
                                                                                   "</ul>",
                                                                                   "<p><b>2. Plotting options:</b></p>",
                                                                                   "<ul>",
                                                                                   "<li><b>Limiting the number of items</b>:</br>",
                                                                                   "Exploring inter-item proximity is more robust when looking at the most frequently cited items. For that reason it is possible to limit the number of items to be plotted according to their overall frequency of mention.</br></br></li>",
                                                                                   "<li><b>Displaying item categories</b>:</br>",
                                                                                   "If user has uploaded item categorical information he/she may choose to color-code the plot labels according to one of the uploaded items' category (for each category, its sub-categories are plotted with a different color).</br>",
                                                                                   "<i>N.B. For item categorical information that was provided by each respondent, each item is assigned to the sub-category which was the most often mentioned for that item. When ties occur (e.g. 10 respondents mentioned they liked the item 'bee' and ten other mentioned they disliked the item 'bee') tie sub-categories (e.g. like_dislike) are created (only for plotting purposes).</i></li>",
                                                                                   "</ul></div>",sep="")),
                                                                        HTML(paste("<div class='divShadow'>",
                                                                                   "<h4>Best tree partition</h4>",
                                                                                   "<p>When exploring inter-item proximity through HCA, FLARES offers the possibility to estimate the best partition for items given their position on the dendrogram and given a minimum and maximum number of desired clusters.</p>",
                                                                                   "<p><b>The optimal partition is chosen as the one maximizing relative intertia loss.</b> In other words the partition for which clusters are the less heterogeneous.</br>",
                                                                                   "<span class='packColor'>>>'cutree' function of the <a href='https://cran.r-project.org/web/packages/dendextend/index.html' target='_blank'>dendextend</a> R package</span></p>",
                                                                                   "<p>Users may choose to include this 'best' partition for analyses on item categorical information provided in the sub-tab 'Item categories analysis'.</br></p>",
                                                                                   "</div>",sep="")),
                                                                        br(),
                                                                        br()
                                                                        )#end tabPanel Methods
                                                               )#end tabsetPanel
                                                   ),#end tabPanel
#####################################################################################################################
#####ITEM CATEGORIES ANALYSIS####
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
                                                               ),#end TabPanel
                                                               tabPanel("Methods",
                                                                        br(),
                                                                        HTML("<p>The analyses carried out by FLARES on item categorical information replicate those elaborated by Robbins & Nolan (<a href='http://dx.doi.org/10.1177/1525822x970090030501' target='_blank'>1997</a>, <a href='http://dx.doi.org/10.1177/1525822x0001200102' target='_blank'>2000</a>).</p>"),
                                                                        HTML(paste("<div class='divShadow'>",
                                                                                   "<h4>Dichotomous category bias</h4>",
                                                                                   "<p><i>This analysis is only available for dichotomous variables (e.g. like/dislike, hot/cold, present/absent etc.).</i></p>",
                                                                                   "<p>The aim of the analysis is to explore whether items belonging to one of the two sub-categories are more salient than those of the other sub-category.</br>",
                                                                                   "In order to do so, the idea is to check whether items of one sub-category are more systematically cited early in respondents' lists than the items of the other sub-category.</p>",
                                                                                   "<p>Robbins & Nolan (<a href='http://dx.doi.org/10.1177/1525822x970090030501' target='_blank'>1997</a>) designed a score to measure the degree to which respondents cite items with a preferential bias for either sub-categories of a dichotomous variable.</br></p>",
                                                                                   "<p><b>Here are a few details on the score's properties:</b></p>",
                                                                                   "<ul>",
                                                                                   "<li>The score (B) is calculated for each respondent and ranges from 0 to 1 (0 indicating extreme minimum bias, 1 indicating extreme maximum bias and 0.5 inidicating no bias whatsoever).</li>",
                                                                                   "<li>For each respondent the sum of the two B scores (one for each category) is equal to one.</li>",
                                                                                   "<li>The individual B scores may be averaged across the whole sample or sub-groups of the sample.</li>",
                                                                                   "</ul>",
                                                                                   "<p><b>Assuming that B is normally distributed, a <i>Z</i>-test may be performed in order to test whether the value of the B scores (for the whole sample or sub-groups of the sample) is significantly different from random.</b></br>",
                                                                                   "<i>Please refer to the above mentioned paper for details on the methods used to calculate the B score.</i></p>",
                                                                                   "</br>",
                                                                                   "<p><b>What FLARES does:</b></p>",
                                                                                   "<ul>",
                                                                                   "<li>It presents a synthetic table with the B scores averaged across the whole sample.</br>",
                                                                                   "<i>If user uploads respondent variables (see 'Respondent Analyses' tab), FLARES breaks down results into sub groups of informants for each respondent variable defined by user.</i></li>",
                                                                                   "<li>When <i>Z</i>-test is significant, the value of the <i>z</i> statistic is indicated as well as the test's significance level.</li>",
                                                                                   "<li>While the table with the B scores for each respondent is not shown, it may be downloaded.</li>",
                                                                                   "</ul></div>",sep="")),
                                                                        HTML(paste("<div class='divShadow'>",
                                                                                   "<h4>Semantic category clustering</h4>",
                                                                                   "<p>The aim of this analysis is to test whether respondents tend to elicit items in clusters within their lists. FLARES will test whether items belonging to sub-categories of any given respondent variable (defined by user) are mentioned in clusters by respondents.</p>",
                                                                                   "<p>Robbins & Nolan (<a href='http://dx.doi.org/10.1177/1525822x0001200102' target='_blank'>2000</a>) designed a score to measure the degree to which respondents cluster items within their lists.</br>",
                                                                                   "<i>The score is calculated for each sub-category AND the overarching respondent variable as a whole</i>.</p>",
                                                                                   "<p><b>Here are a few details on the score's properties:</b></p>",
                                                                                   "<ul>",
                                                                                   "<li>For each individual the score (C) is calaculated for a given category and each of its sub-categories.</li>",
                                                                                   "<li>C ranges from 0 to 1 (0 indicating minimum clustering, 1 indicating maximum clustering).</li>",
                                                                                   "<li>The individual C scores may be averaged across the whole sample or sub-groups of the sample.</li>",
                                                                                   "</ul>",
                                                                                   "<p><b>Assuming that C is normally distributed, a <i>Z</i>-test may be performed in order to test whether the value of the C scores (for the whole sample or sub-groups of the sample) is significantly different from random.</b></br>",
                                                                                   "<i>Please refer to the above mentioned paper for details on the methods used to calculate the C score.</i></p>",
                                                                                   "</br>",
                                                                                   "<p><b>What FLARES does:</b></p>",
                                                                                   "<ul>",
                                                                                   "<li>It presents a synthetic table with the C scores averaged across the whole sample.</br>",
                                                                                   "<i>If user uploads respondent variables (see 'Respondent Analyses' tab), FLARES breaks down results into sub groups of informants as defined by user.</i></li>",
                                                                                   "<li>When <i>Z</i>-test is significant, the value of the <i>z</i> statistic is indicated as well as test's significance level.</li>",
                                                                                   "<li>While the table with the C scores for each respondent is not shown, it may be downloaded.</li>",
                                                                                   "</ul></div>",sep="")),
                                                                        br()
                                                                        )#end tabPanel Methods
                                                               )#endtabsetPanel
                                                   ),#EndTabPanel
#####################################################################################################################
#####DATA SATURATION####
                                          tabPanel("Data Saturation",
                                                   tabsetPanel(selected="Results",
                                                               type="pills",
                                                               tabPanel("Results",
                                                                        br(),
                                                                        strong(textOutput('data.sat.text')),
                                                                        br(),
                                                                        strong("Download Data Saturation Plot"),
                                                                        br(),
                                                                        downloadButton('download.data.saturation','Download'),
                                                                        br(),
                                                                        plotOutput('data.saturation')
                                                                        ),#end tabPanel Results
                                                               tabPanel("Methods",
                                                                        br(),
                                                                        HTML(paste("<p><b>Data saturation</b> can be considered as the '<i>point in data collection and analysis when new information produces little or no change</i>' (Guest et al. <a href='http://dx.doi.org/10.1177/1525822X05279903' target='_blank'>2006</a>).</br></br>",
                                                                                   "In order to explore levels of data saturation FLARES seeks to log-fit the number of newly cited items as new respondents are added to the sample.</br>",
                                                                                   "To optimize the fit, the order of respondents is <b>not random</b> nor does it reflect the order in which respondents appear in users' data files.<br/>",
                                                                                   "Instead, FLARES proceeds as follows:</p>",
                                                                                   "<ul>",
                                                                                   "<li>The first respondent is the one with the shortest list.</li>",
                                                                                   "<li>Each new respondent is then chosen to maximize the number of newly cited items.</li>",
                                                                                   "<li>Once the total number of cited items have been accounted for, FLARES stops adding new informants.</li>",
                                                                                   "<li>Finally, FLARES indicates the number of respondents who were needed to account for all items.</li>",
                                                                                   "</ul>",
                                                                                   "<p>While this method poorly represents how free-lists were actually collected, it has the advantage of offering a comparative measure between different datasets.</br>",
                                                                                   "In fact the ratio of a) number of respondents needed to account for all items to b) total number of respondents may provide an <b>indication of the degree to which the investigated cultural domain is shared</b>.</p>",sep="")),
                                                                        
                                                                        br()
                                                                        )#end tabPanel Methods
                                                               )#end tabsetPanel
                                                   )#end tabPanel Data Saturation
)#tabsetPanel Main
)#conditionalPanel
)#mainPanel
)#SidebarLayout
)#tabPanel