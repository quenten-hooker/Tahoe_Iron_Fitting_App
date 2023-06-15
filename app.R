# This is a shiny web application that:
# 1) trains a model on player data through the bag
# 2) predicts launch conditions based on tahoe and tahoe hl specs
# 3) sends through Callaway aero code
# 4) plots results

# To do
# 1) include new data
# 2) check all models 
# 3) why does it crash every other time?
# 3) dynamically change length and loft?
# 3) save models?
# 4) Change UI
# 5) How to deploy with aero?


# toggle of inputs generic vs. detailed
# wedges, tahoe iron, and hybrid (selection of where)

# Input A - Generic
# Club head speed, angle of attack, low/medium/high trajectory 

# Input B - detailed
# Ball speed, LA, Backs
# maybe change length/loft.

# On Callaway's website (new deadline)

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
use_python("C:/Users/Quenten.hooker/AppData/Local/Programs/Python/Python39/python.exe", required=TRUE)
# library("tidyr")
# library("ggpubr")
# library("ggforce")
# library('xlsx')
# library('pracma')
# library('scales')
# library('jpeg')
# library('maditr')
# library('png')
# library('grid')
# library('rsconnect')


# # Additional Packages
# library("lme4")
# library("car")
# library("tidyr")
# library("lmerTest")
# library("GGally")
# library("emmeans")
# library("merTools")
# library("olsrr")
# library("readxl")
# library("psych")
# library("sjPlot")
# library("sjmisc")
# library("knitr")
# library("sp")
# library("plotly")
# library("reshape2")
# library('akima')
# library('lubridate')
# library('stringi')
# library('fuzzyjoin')
# library('stringdist')
# library('png')

# Player images

# Load player training data
Data.Combined <- read.csv('Data_All_Filtered.csv')

# Format data
Data.Combined <- within(Data.Combined, {
  Player <- factor(Player)
  Club <- factor(Club)
  Club.Speed.quad <- as.numeric(Club.Speed.quad)
  Efficiency.quad <- as.numeric(Efficiency.quad)
  Angle.Of.Attack.quad <- as.numeric(Angle.Of.Attack.quad)
  Club.Path.quad <- as.numeric(Club.Path.quad)
  Face.To.Target.quad <- as.numeric(Face.To.Target.quad)
  Face.To.Path.quad <- as.numeric(Face.To.Path.quad)
  Lie.quad <- as.numeric(Lie.quad)
  Loft.quad <- as.numeric(Loft.quad)
  Lateral.Face.quad <- as.numeric(Lateral.Face.quad)*-1
  Vertical.Face.quad <- as.numeric(Vertical.Face.quad)
})

# Through the bag tendency models 
lm.Club.Speed.quad <-lmer(Club.Speed.quad ~
                            # Fixed-effects
                            1 + Length + I(Length^2) +
                            # Random-effects
                            (1 + Length + I(Length^2) | Player), data = Data.Combined)

lm.Angle.Of.Attack.quad <-lmer(Angle.Of.Attack.quad ~
                                 # Fixed-effects
                                 1 + Length + I(Length^2) +
                                 # Random-effects
                                 (1 + Length + I(Length^2) | Player), data = Data.Combined)

lm.Pitch <-lmer(Pitch ~
                  # Fixed-effects
                  1 + Length + I(Length^2) +
                  # Random-effects
                  (1 + Length + I(Length^2) | Player), data = Data.Combined)

# Select club models of interest
Data.Combined.train <- Data.Combined %>% filter(Model == "Tahoe Std" | Model == "Tahoe HL")

# Refactor model for level pulling  
Data.Combined.train <- within(Data.Combined.train, {
  Model <- factor(as.character(Model))
})

# Launch condition models
lm.Ball.Speed.normalize <-lmer(Ball.Speed.quad ~
                                 # Fixed-effects
                                 1 + Model + Club.Speed.quad + Pitch + Length:Club.Speed.quad + Length:Pitch + Vertical.Face.quad + Lateral.Face.quad +
                                 Model:Vertical.Face.quad + Model:Lateral.Face.quad + Model:I(Vertical.Face.quad^2) + Model:I(Lateral.Face.quad^2) +
                                 # Random-effects
                                 (1 + Club.Speed.quad + Pitch + Vertical.Face.quad + Lateral.Face.quad| Player), data = Data.Combined.train)

lm.Launch.Angle.normalize <-lmer(Launch.Angle.quad ~
                                   # Fixed-effects
                                   1 + Model + Club.Speed.quad + Angle.Of.Attack.quad + Pitch + Vertical.Face.quad + Length:Pitch + Vertical.Face.quad + 
                                   Static.Loft:Club.Speed.quad + Static.Loft:Angle.Of.Attack.quad + Static.Loft:Pitch + Static.Loft:Vertical.Face.quad +
                                   # Random-effects
                                   (1 + Club.Speed.quad + Angle.Of.Attack.quad + Pitch + Vertical.Face.quad| Player), data = Data.Combined.train)

lm.Back.Spin.normalize <-lmer(Back.Spin.quad ~
                                # Fixed-effects
                                1 + Model:Length + Model:Static.Loft + Angle.Of.Attack.quad + Pitch + Vertical.Face.quad + Club.Speed.quad +
                                # Random-effects
                                (1 + Angle.Of.Attack.quad + Pitch + Vertical.Face.quad| Player), data = Data.Combined.train)

lm.Side.Angle.normalize <-lmer(Side.Angle.quad ~
                                 # Fixed-effects
                                 1 + Face.To.Target.quad + Lateral.Face.quad +
                                 # Random-effects
                                 (1 + Club.Speed.quad + Face.To.Target.quad + Lateral.Face.quad | Player), data = Data.Combined.train)

lm.Side.Spin.normalize <-lmer(Side.Spin.quad ~
                                # Fixed-effects
                                1 + Face.To.Path.quad + Lateral.Face.quad  +
                                # Random-effects
                                (1 + Face.To.Path.quad + Lateral.Face.quad | Player), data = Data.Combined.train)


new_data_create <- function(speed, attack, lean) {
  
# Setup model test dataframes
Tahoe_length <- c(35.75, 36.0, 36.625, 37.25, 37.875, 38.5)
Tahoe_static_loft <- c(42, 37, 33, 29, 26, 23)
Tahoe_HL_length <- c(35.75, 36.0, 36.75, 37.5, 38.25, 39.0)
Tahoe_HL_static_loft <- c(43, 38, 34, 30, 27, 24)

Length.data <- data.frame(Length = c(rep(Tahoe_length, each = 8), rep(Tahoe_HL_length, each = 8)))
Static_Loft.data <- data.frame(Loft = c(rep(Tahoe_static_loft, each = 8), rep(Tahoe_HL_static_loft, each = 8)))

# Define Player intercept for 6 iron, needs to be changed to a 7 iron
player_speed_dif <- (speed - ggpredict(lm.Club.Speed.quad, terms = "Length[37]")$predicted)
player_attack_dif <- (attack - ggpredict(lm.Angle.Of.Attack.quad, terms = "Length[37]")$predicted)
player_pitch_dif <- (lean - ggpredict(lm.Pitch, terms = "Length[37]")$predicted)

#Define lateral and vertical impacts
set.seed(6)#14
x <- rnorm(8, 0, 1)
Lateral.Impact.GMM <- scales::rescale(x, to = c(-15, 15), from = range(x))

set.seed(14)
y <- rnorm(8, 0, 1)
Vertical.Impact.GMM <- scales::rescale(y, to = c(-20, 0), from = range(y))


new_data = c()
new_data <- data.frame(Player = rep("Population", nrow(Length.data)),
                       Model = rep(c("Tahoe Std", "Tahoe HL"), each = 48),
                       Length =c(rep(Tahoe_length, each = 8), rep(Tahoe_HL_length, each = 8)),
                       Static.Loft = c(rep(Tahoe_static_loft, each = 8), rep(Tahoe_HL_static_loft, each = 8)),
                       Club.Speed.quad = predict(lm.Club.Speed.quad, re.form= NA, newdata = Length.data) + player_speed_dif,
                       Angle.Of.Attack.quad = predict(lm.Angle.Of.Attack.quad, re.form= NA, newdata = Length.data) + player_attack_dif,
                       Face.To.Target.quad = 0,
                       Club.Path.quad = 0,
                       Face.To.Path.quad = 0,
                       Pitch = predict(lm.Pitch, re.form= NA, newdata = Length.data) + player_pitch_dif,
                       Lie.quad = rep(mean(na.omit(Data.Combined.train$Lie.quad)), 96),
                       Lateral.Face.quad = rep(Lateral.Impact.GMM, 12),
                       Vertical.Face.quad = rep(Vertical.Impact.GMM, 12))
 return(new_data)
}

#Predict dataframe

#predict_data <- data.frame(predict_data)


model_lc <- function(new_data) {
  
  predict_data = c()
  predict_data$Model = new_data$Model
  predict_data$Length = new_data$Length
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 35.75] <- "PW"
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 36.00] <- "9"
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 36.625] <- "8"
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 37.25] <- "7"
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 37.875] <- "6"
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 38.50] <- "5"
  #predict_data$Club[predict_data$Club == "Tahoe" & predict_data$Length== 39.125] <- "4"
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 35.75] <- "PW"
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 36.00] <- "9"
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 36.75] <- "8"
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 37.50] <- "7"
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 38.25] <- "6"
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 39.00] <- "5"
  #predict_data$Club[predict_data$Club == "Tahoe HL" & predict_data$Length== 39.750] <- "4"
  predict_data$Ball.Speed <- predict(lm.Ball.Speed.normalize, re.form= NA, newdata = new_data)
  predict_data$Launch.Angle <- predict(lm.Launch.Angle.normalize, re.form= NA, newdata = new_data)
  predict_data$Back.Spin <- predict(lm.Back.Spin.normalize, re.form= NA, newdata = new_data)
  predict_data$Side.Angle <- predict(lm.Side.Angle.normalize, re.form= NA, newdata = new_data)
  predict_data$Side.Spin <- predict(lm.Side.Spin.normalize, re.form= NA, newdata = new_data)
  
  predict_data <- within(predict_data, {
    Club <- factor(Club)
    Length <- factor(Length)
    Model <- factor(Model)
    })
  
  return(predict_data)
  
}

#print(predict_data)

#BELOW IS A TEMPLATE FOR THE SHINY APP
# This is a Shiny web application. You can run the application by clicking
#library(shiny)
#library(shinythemes)
#library(tableHTML)
#mycss <- ".irs-bar, .irs-bar-edge, .irs-single, .irs-grid-pol, .js-irs-0 .irs-to,.js-irs-0 .irs-from { background: red;  border-color: red;}"

# Define UI ----
ui <- navbarPage(theme = shinytheme("darkly"),
  title = h4(strong("Tahoe Iron Performance")),
  tabPanel(title = "Consumer",
           sidebarLayout(
             sidebarPanel(h4(strong("7 Iron Club Delivery")),
             sliderInput("speed",
                         label = strong(HTML('&nbsp;'), "Swing Speed"),
                         min = 50, max = 110, step = 5, value = c(90), width = '390px'),
             selectInput("attack", "Angle of Attack", choices = c("Steep", "Moderate", "Shallow")),
             selectInput("lean", "Shaft Lean", choices = c("Forward", "Neutral", "Backward")),
             actionButton("predict", "Submit to predict")
         ),
         mainPanel( 
         plotOutput("plottrajectory"),
         dataTableOutput("carrytable")
         ))),
  tabPanel(title = "Fitter",
           sidebarLayout(
             sidebarPanel(h4(strong("7 Iron Club Delivery")),
                          sliderInput("ballspeed",
                                      label = strong(HTML('&nbsp;'), "Ball Speed (mph)"),
                                      min = 50, max = 110, step = 5, value = c(90), width = '390px'),
                          sliderInput("launch",
                                      label = strong(HTML('&nbsp;'), "Launch Angle (deg)"),
                                      min = 5, max = 20, step = 5, value = c(90), width = '390px'),
                          sliderInput("backs",
                                      label = strong(HTML('&nbsp;'), "Backspin (rpm)"),
                                      min = 3000, max = 9000, step = 500, value = c(7000), width = '390px'),
                          actionButton("predict", "Submit to predict")
                          ),
           mainPanel())
           )
  )
  
  # fluidRow(
  #   dataTableOutput("trajectory")
  # ),
  
  # fluidRow(
  #   dataTableOutput("aero")
  # ),


source_python('python_ref.py')

# Define server logic ----
server <- function(input, output) {
  
  input_df <- reactive({
    
    if (input$attack == "Steep"){
      attack = -6.0
    } else if (input$attack == "Moderate"){
      attack = -3.0
    } else if (input$attack == "Shallow"){
      attack = 0.0
    }
    
    if (input$lean == "Forward"){
      lean = -4.0
    } else if (input$lean == "Neutral"){
      lean = -2.0
    } else if (input$lean == "Backward"){
      lean = 0.0
    }
    
    data.frame(
      speed = as.numeric(input$speed),
      attack = as.numeric(attack),
      lean = as.numeric(lean)
    )
  })
  

  launch_data <- eventReactive(input$predict, {
    
    test <- as.data.frame(new_data_create(input_df()$speed, input_df()$attack, input_df()$lean))
    launch_data <- model_lc(test)

    return(launch_data)
  })
  
  downrange_data <- reactive({

  launch_data <- as.data.frame(launch_data())
  launch_data <- launch_data %>% group_by(Model, Club) %>% summarise(Ball.Speed = mean(Ball.Speed),
                                                                     Launch.Angle = mean(Launch.Angle),
                                                                     Back.Spin = mean(Back.Spin),
                                                                     Side.Angle = mean(Side.Angle),
                                                                     Side.Spin = mean(Side.Spin))

  model_col_names <- c("Model", "Club", "Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
  colnames(launch_data) <- model_col_names
  aero_data <- aerotest(launch_data)
  downrange_data <- aero_data %>% select(carrydisp, carrydist)
  final_data <- cbind(launch_data, downrange_data)
  #print(final_data)

     return(final_data)
  })
  
  trajectory <- reactive({
    
    launch_data <- as.data.frame(launch_data())
    launch_data <- launch_data %>% group_by(Model, Club) %>% summarise(Ball.Speed = mean(Ball.Speed), 
                                                                       Launch.Angle = mean(Launch.Angle),
                                                                       Back.Spin = mean(Back.Spin), 
                                                                       Side.Angle = mean(Side.Angle),
                                                                       Side.Spin = mean(Side.Spin))
    
    model_col_names <- c("Model", "Club", "Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
    colnames(launch_data) <- model_col_names
    aero_data <- aerotest(launch_data)
    traj_X_data <- aero_data[["X yards"]]
    traj_X_data <- as.data.frame(do.call(cbind, traj_X_data))
    traj_X_data <- pivot_longer(traj_X_data, cols=everything()) %>% arrange(name)
    traj_X_data$Model[traj_X_data$name == "V1" | traj_X_data$name == "V2" | traj_X_data$name == "V3" | traj_X_data$name == "V4" | traj_X_data$name == "V5" | traj_X_data$name == "V6"] <- "Tahoe HL"
    traj_X_data$Model[traj_X_data$name == "V7" | traj_X_data$name == "V8" | traj_X_data$name == "V9" | traj_X_data$name == "V10" | traj_X_data$name == "V11" | traj_X_data$name == "V12"] <- "Tahoe Std"
    traj_X_data$Club[traj_X_data$name == "V1" | traj_X_data$name == "V7"] <- "5"
    traj_X_data$Club[traj_X_data$name == "V2" | traj_X_data$name == "V8"] <- "6"
    traj_X_data$Club[traj_X_data$name == "V3" | traj_X_data$name == "V9"] <- "7"
    traj_X_data$Club[traj_X_data$name == "V4" | traj_X_data$name == "V10"] <- "8"
    traj_X_data$Club[traj_X_data$name == "V5" | traj_X_data$name == "V11"] <- "9"
    traj_X_data$Club[traj_X_data$name == "V6" | traj_X_data$name == "V12"] <- "PW"
    
    traj_Z_data <- aero_data[["Z yards"]]
    traj_Z_data <- as.data.frame(do.call(cbind, traj_Z_data))
    traj_Z_data <- pivot_longer(traj_Z_data, cols=everything()) %>% arrange(name)
    
    Traj <- data.frame(Model = as.factor(traj_X_data$Model),
                       Club = as.factor(traj_X_data$Club),
                       X.yards = traj_X_data$value,
                       Z.yards = traj_Z_data$value)
                       
      
    return(Traj) 
  })
  
  # output$aero <- renderDataTable({
  # 
  #   data <- as.data.frame(model_lc())
  #   model_col_names <- c("Model", "Length", "Club", "Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
  #   colnames(data) <- as.character(model_col_names)
  # 
  #   aero_data <- data %>% select("Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
  #   #aero_data <- as.data.frame(aero_data)
  #   test <- aerotest(aero_data)
  #   
  #   
  #   #print(test)
  #   #test <- as.data.frame(reticulate::py$test)
  #   #print(test)
  # 
  #   return(datatable(test))
  #   #returnedText = testMethod()
  # 
  # })
  
  # output$plotdownrange <- renderPlot({
  #   
  #   ggplot(NULL,  aes(x = carrydisp.c, y = carrydist, color = Model, shape = Club)) + geom_point(data = downrange_data()) +
  #   stat_ellipse(data = downrange_data(), level = 0.85, type = "t")+ theme_classic(base_size = 18)+theme(legend.position = "right") + 
  #   ylab("Carry.predict (yds)") + xlab("Offline.predict (yds)") + xlim(-5, 5)
  #   
  #     })
  
  
  output$plottrajectory <- renderPlot({
    
    ggplot(NULL, aes(x = X.yards, y = Z.yards*3, color = Model, shape = Club)) +
      geom_point(data = trajectory()) + xlab("Distance (yds)") + ylab("Height (ft)") + theme_classic(base_size = 18)
    
    })
  
  output$carrytable <- renderDataTable({

    launch_data <- as.data.frame(launch_data())
    launch_data <- launch_data %>% group_by(Model, Club) %>% summarise(Ball.Speed = mean(Ball.Speed),
                                                                       Launch.Angle = mean(Launch.Angle),
                                                                       Back.Spin = mean(Back.Spin),
                                                                       Side.Angle = mean(Side.Angle),
                                                                       Side.Spin = mean(Side.Spin))
    
    model_col_names <- c("Model", "Club", "Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
    colnames(launch_data) <- model_col_names
    aero_data <- aerotest(launch_data)
    downrange_data <- aero_data %>% select(carrydisp, carrydist)
    final_data <- cbind(launch_data, downrange_data)
    final_data <- final_data %>% group_by(Club, Model) %>% summarise(Carry_Distance = round(mean(carrydist),1)) %>% rename(
        'Carry Distance (yds)' = Carry_Distance)
    
    #return(datatable(final_data))
    
    return(datatable(final_data, style = "bootstrap", options = list(pageLength = 15, lengthChange = FALSE, searching = FALSE),
                     rownames= FALSE))

    #   return(datatable(final_data,     options = list(
    #   initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'background-color' : 'white', 'color' : 'black', 'height' : '30px', 'font-size' : '15px', 'border-bottom' : 'none'});}"), dom = 't', ordering = F, columnDefs = list(list(className = 'dt-center', targets = 1:3))
    # ), rownames = FALSE, colnames=c("Club", "Model", "Carry Distance (yds)")) %>%
    #   formatStyle(columns = 1:3,  color = 'black', backgroundColor = 'white', fontSize = '15px'))

  })
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
