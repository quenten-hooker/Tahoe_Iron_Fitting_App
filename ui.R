# # Load Packages
library("ggplot2")
library('tidyr')
library("dplyr")
library("reticulate")
library('lme4')
library('DT')
library('ggeffects')
library('shiny')
library('shinyWidgets')
library('shinythemes')
library('plotly')
library('neuralnet')

#BELOW IS A TEMPLATE FOR THE SHINY APP
# This is a Shiny web application. You can run the application by clicking
#library(shiny)
#library(shinythemes)
#library(tableHTML)
#mycss <- ".irs-bar, .irs-bar-edge, .irs-single, .irs-grid-pol, .js-irs-0 .irs-to,.js-irs-0 .irs-from { background: red;  border-color: red;}"

# Define UI ----
ui <- navbarPage(theme = shinytheme("darkly"),
                 title = h4(strong("Tahoe Performance")),
                 tabPanel(title = "Consumer",
                          sidebarLayout(
                            sidebarPanel(h4(strong("7 Iron Club Delivery Inputs")),
                                         sliderInput("speed",
                                                     label = strong(HTML('&nbsp;'), "Swing Speed"),
                                                     min = 50, max = 110, step = 5, value = c(90), width = '390px'),
                                         selectInput("attack", "Angle of Attack", choices = c("Steep", "Moderate", "Shallow")),
                                         selectInput("pitch", "Shaft pitch", choices = c("Forward", "Neutral")),
                                         actionButton("predict", "Submit to predict"),
                                         width = 5
                            ),
                            mainPanel( 
                              plotlyOutput("plottrajectory"),
                              dataTableOutput("carrytable"),
                              width = 7
                            ))),
                 tabPanel(title = "Fitter",
                          sidebarLayout(
                            sidebarPanel(h4(strong("7 Iron Launch Condition Inputs")),
                                         selectInput("clubtype", "Club", choices = c("Apex MB 21", "Apex TCB 21", "Apex 21", "Apex Pro 21", "Apex DCB 21", "Paradym 23", "Paradym X 23")),
                                         sliderInput("bs",
                                                     label = strong(HTML('&nbsp;'), "Ball Speed (mph)"),
                                                     min = 80, max = 140, step = 5, value = c(120), width = '390px'),
                                         sliderInput("la",
                                                     label = strong(HTML('&nbsp;'), "Launch Angle (deg)"),
                                                     min = 5, max = 30, step = 1, value = c(16), width = '390px'),
                                         sliderInput("backs",
                                                     label = strong(HTML('&nbsp;'), "Backspin (rpm)"),
                                                     min = 4000, max = 9000, step = 500, value = c(7000), width = '390px'),
                                         sliderInput("sa",
                                                     label = strong(HTML('&nbsp;'), "Side Angle (deg)"),
                                                     min = -5, max = 5, step = .5, value = c(0), width = '390px'),
                                         sliderInput("sides",
                                                     label = strong(HTML('&nbsp;'), "Sidespin (rpm)"),
                                                     min = -2000, max = 2000, step = 250, value = c(0), width = '390px'),
                                         #actionButton("predict2", "Submit to predict 7i trajectory"),
                                         actionButton("predict3", "Predict Trajectories"),
                                         plotlyOutput("plottrajectory_gamer"),
                                         width = 5
                            ),
                            mainPanel(
                              plotlyOutput('plottrajectory_3dtest', height = '500px'),
                              dataTableOutput("carrytable_new"),
                              dataTableOutput("lctable"),
                              width = 7
                              #plotlyOutput('plottrajectory_new'),
                              #dataTableOutput("carrytable_new"),
                            )))
)
