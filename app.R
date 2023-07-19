# This is a shiny web application that:
# 1) trains a model on player data through the bag
# 2) predicts launch conditions based on tahoe and tahoe hl specs
# 3) sends through Callaway aero code
# 4) plots results

# To do
# 1) include new data
# 2) check all models 
# 3) random crashes?
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

# to do
# size of each model 
# azure function with model inside code, javascript, tensorflow javascript, dg3s, learn how to do python tensorflow.

#to do
# Check bounds of launch conditions in Engage
# test with gamer on final model plot
# test with just modeling launch conditions 

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

#load swing TTB models
lm.Club.Speed.quad <- readRDS(file = "mem_speed_TTB_test.rda")
lm.Angle.Of.Attack.quad <- readRDS(file = "mem_attack_TTB.rda")
lm.Pitch <- readRDS(file = "mem_pitch_TTB.rda")

#load Tahoe OS and Tahoe HL LC models
lm.Ball.Speed.normalize <- readRDS(file = "mem_bs_tahoe.rda")
lm.Launch.Angle.normalize <- readRDS(file = "mem_la_tahoe.rda")
lm.Back.Spin.normalize <- readRDS(file = "mem_backs_tahoe.rda")
lm.Side.Angle.normalize <- readRDS(file = "mem_sa_tahoe.rda")
lm.Side.Spin.normalize <- readRDS(file = "mem_sides_tahoe.rda")

#load ball to swing inverse model
#nn_inverse <- readRDS(file = "nn_10_8_3_final.rda")
nn_inverse <- readRDS(file = "nn_12_8_5_paradym.rda")

swing_data_predict <- function(speed, attack, pitch) {
  
# Setup model test dataframes
Tahoe_length <- c(35.5, 35.75, 36.0, 36.625, 37.25, 37.875, 38.5, 39.125)
Tahoe_static_loft <- c(47, 42, 37, 33, 29, 26, 23, 20)
Tahoe_HL_length <- c(35.5, 35.75, 36.0, 36.75, 37.5, 38.25, 39.0, 39.75)
Tahoe_HL_static_loft <- c(48, 43, 38, 34, 30, 27, 24, 21)

Length.data <- data.frame(Length = c(rep(Tahoe_length, each = 8), rep(Tahoe_HL_length, each = 8)))
Static_Loft.data <- data.frame(Loft = c(rep(Tahoe_static_loft, each = 8), rep(Tahoe_HL_static_loft, each = 8)))

# Define Player intercept for 6 iron, needs to be changed to a 7 iron
player_speed_dif <- (speed - ggpredict(lm.Club.Speed.quad, terms = "Length[37]")$predicted)
print(player_speed_dif)
player_attack_dif <- (attack - ggpredict(lm.Angle.Of.Attack.quad, terms = "Length[37]")$predicted)
player_pitch_dif <- (pitch - ggpredict(lm.Pitch, terms = "Length[37]")$predicted)


#Define lateral and vertical impacts
set.seed(6)#14
x <- rnorm(8, 0, 1)
Lateral.Impact.GMM <- scales::rescale(x, to = c(-15, 15), from = range(x))

set.seed(14)
y <- rnorm(8, 0, 1)
Vertical.Impact.GMM <- scales::rescale(y, to = c(-20, 0), from = range(y))

new_data = c()
new_data <- data.frame(Player = rep("Population", nrow(Length.data)),
                       Model = rep(c("Tahoe Std", "Tahoe HL"), each = 64),
                       Length =c(rep(Tahoe_length, each = 8), rep(Tahoe_HL_length, each = 8)),
                       Static.Loft = c(rep(Tahoe_static_loft, each = 8), rep(Tahoe_HL_static_loft, each = 8)),
                       Club.Speed.quad = predict(lm.Club.Speed.quad, re.form= NA, newdata = Length.data) + player_speed_dif,
                       Angle.Of.Attack.quad = predict(lm.Angle.Of.Attack.quad, re.form= NA, newdata = Length.data) + player_attack_dif,
                       Face.To.Target.quad = 0,
                       Club.Path.quad = 0,
                       Face.To.Path.quad = 0,
                       Pitch = predict(lm.Pitch, re.form= NA, newdata = Length.data) + player_pitch_dif,
                       Lie.quad = rep(0, 128),
                       Lateral.Face.quad = rep(Lateral.Impact.GMM, 16),
                       Vertical.Face.quad = rep(Vertical.Impact.GMM, 16))
 return(new_data)
}

predict_tahoe_lc <- function(new_data, bs, la, backs, sa, sides) {
  
  predict_data = c()
  predict_data$Model = new_data$Model
  predict_data$Length = new_data$Length
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 35.5] <- "AW"
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 35.75] <- "PW"
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 36.00] <- "9"
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 36.625] <- "8"
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 37.25] <- "7"
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 37.875] <- "6"
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 38.50] <- "5"
  predict_data$Club[predict_data$Model == "Tahoe Std" & predict_data$Length== 39.125] <- "4"
  
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 35.5] <- "AW"
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 35.75] <- "PW"
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 36.00] <- "9"
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 36.75] <- "8"
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 37.50] <- "7"
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 38.25] <- "6"
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 39.00] <- "5"
  predict_data$Club[predict_data$Model == "Tahoe HL" & predict_data$Length== 39.75] <- "4"
  
  predict_data$Ball.Speed <- predict(lm.Ball.Speed.normalize, re.form= NA, newdata = new_data)
  predict_data$Launch.Angle <- predict(lm.Launch.Angle.normalize, re.form= NA, newdata = new_data)
  predict_data$Back.Spin <- predict(lm.Back.Spin.normalize, re.form= NA, newdata = new_data)
  predict_data$Side.Angle <- sa #predict(lm.Side.Angle.normalize, re.form= NA, newdata = new_data) #sa
  predict_data$Side.Spin <- sides #predict(lm.Side.Spin.normalize, re.form= NA, newdata = new_data) #sides
  
  predict_data <- within(predict_data, {
    Club <- factor(Club)
    Length <- factor(Length)
    Model <- factor(Model)
    })
  
  #print(predict_data)
  
  return(predict_data)
  
}

inverse_predict <- function(model, bs, la, backs, sa, sides) {
  
  
  if (model == "Apex MB 21"){
    
    club_data = data.frame(APEX_MB_21 = 1,
                           APEX_TCB_21 = 0,
                           APEX_PRO_21 = 0,
                           APEX_21 = 0,
                           APEX_DCB_21 = 0,
                           PARADYM_23 = 0,
                           PARADYM_X_23 = 0)
    
  } else if (model == "Apex TCB 21"){
    
    club_data = data.frame(APEX_MB_21 = 0,
                           APEX_TCB_21 = 1,
                           APEX_PRO_21 = 0,
                           APEX_21 = 0,
                           APEX_DCB_21 = 0,
                           PARADYM_23 = 0,
                           PARADYM_X_23 = 0
                           )
    
  } else if (model == "Apex Pro 21"){
    
    club_data = data.frame(APEX_MB_21 = 0,
                           APEX_TCB_21 = 0,
                           APEX_PRO_21 = 1,
                           APEX_21 = 0,
                           APEX_DCB_21 = 0,
                           PARADYM_23 = 0,
                           PARADYM_X_23 = 0)
    
  } else if (model == "Apex 21"){
    
    club_data = data.frame(APEX_MB_21 = 0,
                           APEX_TCB_21 = 0,
                           APEX_PRO_21 = 0,
                           APEX_21 = 1,
                           APEX_DCB_21 = 0,
                           PARADYM_23 = 0,
                           PARADYM_X_23 = 0)
    
  } else if (model == "Apex DCB 21"){
    
    club_data = data.frame(APEX_MB_21 = 0,
                           APEX_TCB_21 = 0,
                           APEX_PRO_21 = 0,
                           APEX_21 = 0,
                           APEX_DCB_21 = 1,
                           PARADYM_23 = 0,
                           PARADYM_X_23 = 0)
  } else if (model == "Paradym 23"){
    
    club_data = data.frame(APEX_MB_21 = 0,
                           APEX_TCB_21 = 0,
                           APEX_PRO_21 = 0,
                           APEX_21 = 0,
                           APEX_DCB_21 = 0,
                           PARADYM_23 = 1,
                           PARADYM_X_23 = 0)
  }else if (model == "Paradym X 23"){
    
    club_data = data.frame(APEX_MB_21 = 0,
                           APEX_TCB_21 = 0,
                           APEX_PRO_21 = 0,
                           APEX_21 = 0,
                           APEX_DCB_21 = 0,
                           PARADYM_23 = 0,
                           PARADYM_X_23 = 1)
  }

  predict_data = data.frame(BallSpeedMph = as.numeric(bs),
                            BallLaunchAngleDeg =  as.numeric(la),
                            BallSideAngleDeg =  as.numeric(sa),
                            BallBackSpinRpm =  as.numeric(backs),
                            BallSideSpinRpm =  as.numeric(sides))
  

  sds = data.frame(ImpactHeadSpeedMph = 8.4454662,
                   ImpactAttackAngleDeg = 2.5436867,
                   ImpactPitchAngleDeg  = 3.4537867,
                   ImpactYawAngleDeg = 3.5808855,
                   ImpactPathAngleDeg = 3.7887323,
                   ImpactHorizLocInch = 0.4393312,
                   ImpactVertLocInch = 0.2710816,
                   BallSpeedMph = 11.2668772,
                   BallLaunchAngleDeg = 3.1320913,
                   BallSideAngleDeg = 3.0279675,
                   BallBackSpinRpm = 1190.1103030,
                   BallSideSpinRpm = 764.2350612)

  means = data.frame(ImpactHeadSpeedMph = 83.2920008,
                   ImpactAttackAngleDeg = -3.6731912,
                   ImpactPitchAngleDeg  = 30.8064502,
                   ImpactYawAngleDeg = 0.6811612,
                   ImpactPathAngleDeg = 1.0280667,
                   ImpactHorizLocInch = -0.1694626,
                   ImpactVertLocInch = 0.2901663,
                   BallSpeedMph = 109.3495457,
                   BallLaunchAngleDeg = 19.1585356,
                   BallSideAngleDeg = 0.3574984,
                   BallBackSpinRpm = 5092.6397597,
                   BallSideSpinRpm = -11.3982497)

  scaled <- as.data.frame(cbind(scale(predict_data, center = means[8:12], scale = sds[8:12]), club_data))
  
  pr.nn <- compute(nn_inverse, scaled)
  
  
  #organize actual vs. predictions
  results <- data.frame(pr.nn$net.result[,1], pr.nn$net.result[,2], pr.nn$net.result[,3], pr.nn$net.result[,4], pr.nn$net.result[,5])
  colnames(results) <- c("pred_ImpactHeadSpeedMph", "pred_ImpactAttackAngleDeg", "pred_ImpactPitchAngleDeg", "pred_ImpactYawAngleDeg", "pred_ImpactPathAngleDeg")

  unscaled_ECPC <- data.frame(pred_ImpactHeadSpeedMph = results$pred_ImpactHeadSpeedMph * sds[['ImpactHeadSpeedMph']] + means[['ImpactHeadSpeedMph']],
                              pred_ImpactAttackAngleDeg = results$pred_ImpactAttackAngleDeg * sds[['ImpactAttackAngleDeg']] + means[['ImpactAttackAngleDeg']],
                              pred_ImpactPitchAngleDeg = results$pred_ImpactPitchAngleDeg * sds[['ImpactPitchAngleDeg']] + means[['ImpactPitchAngleDeg']] - 36,
                              pred_ImpactYawAngleDeg = results$pred_ImpactYawAngleDeg * sds[['ImpactYawAngleDeg']] + means[['ImpactYawAngleDeg']],
                              pred_ImpactPathAngleDeg = results$pred_ImpactPathAngleDeg * sds[['ImpactPathAngleDeg']] + means[['ImpactPathAngleDeg']]
                              )
  
  print(unscaled_ECPC)
  return(unscaled_ECPC)
  
}  


swing_data_predict_2 <- function(unscaled_ECPC) {
  
  # Setup model test dataframes
  Tahoe_length <- c(35.5, 35.75, 36.0, 36.625, 37.25, 37.875, 38.5, 39.125)
  Tahoe_static_loft <- c(47, 42, 37, 33, 29, 26, 23, 20)
  Tahoe_HL_length <- c(35.5, 35.75, 36.0, 36.75, 37.5, 38.25, 39.0, 39.75)
  Tahoe_HL_static_loft <- c(48, 43, 38, 34, 30, 27, 24, 21)
  
  Length.data <- data.frame(Length = c(rep(Tahoe_length, each = 8), rep(Tahoe_HL_length, each = 8)))
  Static_Loft.data <- data.frame(Loft = c(rep(Tahoe_static_loft, each = 8), rep(Tahoe_HL_static_loft, each = 8)))
  
  # Define Player intercept for 6 iron, needs to be changed to a 7 iron
  player_speed_dif <- (unscaled_ECPC$pred_ImpactHeadSpeedMph - ggpredict(lm.Club.Speed.quad, terms = "Length[37]")$predicted)
  player_attack_dif <- (unscaled_ECPC$pred_ImpactAttackAngleDeg - ggpredict(lm.Angle.Of.Attack.quad, terms = "Length[37]")$predicted)
  player_pitch_dif <- (unscaled_ECPC$pred_ImpactPitchAngleDeg - ggpredict(lm.Pitch, terms = "Length[37]")$predicted)
  
  #Define lateral and vertical impacts
  set.seed(6)#14
  x <- rnorm(8, 0, 1)
  Lateral.Impact.GMM <- scales::rescale(x, to = c(0, 0), from = range(x))
  
  set.seed(14)
  y <- rnorm(8, 0, 1)
  #Vertical.Impact.GMM <- scales::rescale(y, to = c(-20, 0), from = range(y))
  Vertical.Impact.GMM <- c(-12.785854	, -11.919672, -10.302431, -9.718269, -8.286890, -7.219095, -6.297345, -5.297321)
    
  new_data = c()
  new_data <- data.frame(Player = rep("Population", nrow(Length.data)),
                         Model = rep(c("Tahoe Std", "Tahoe HL"), each = 64),
                         Length =c(rep(Tahoe_length, each = 8), rep(Tahoe_HL_length, each = 8)),
                         Static.Loft = c(rep(Tahoe_static_loft, each = 8), rep(Tahoe_HL_static_loft, each = 8)),
                         Club.Speed.quad = predict(lm.Club.Speed.quad, re.form= NA, newdata = Length.data) + player_speed_dif,
                         Angle.Of.Attack.quad = predict(lm.Angle.Of.Attack.quad, re.form= NA, newdata = Length.data) + player_attack_dif,
                         Face.To.Target.quad = unscaled_ECPC$pred_ImpactYawAngleDeg,
                         Club.Path.quad = unscaled_ECPC$pred_ImpactPathAngleDeg,
                         Face.To.Path.quad = unscaled_ECPC$pred_ImpactYawAngleDeg - unscaled_ECPC$pred_ImpactPathAngleDeg,
                         Pitch = predict(lm.Pitch, re.form= NA, newdata = Length.data) + player_pitch_dif,
                         Lie.quad = rep(0, 128),
                         Lateral.Face.quad = rep(Lateral.Impact.GMM, 16),
                         Vertical.Face.quad = rep(rep(Vertical.Impact.GMM, each = 8), 2))
  print(new_data %>% group_by(Model, Length) %>% summarise(mean(Club.Speed.quad), mean(Angle.Of.Attack.quad),mean(Pitch), mean(Vertical.Face.quad), ))
  
  #print(new_data)
  return(new_data)
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
  

source_python('python_ref.py')

# Define server logic ----
server <- function(input, output) {
  
  
  #Consumer page functions
  swing_input_df <- reactive({
    
    if (input$attack == "Steep"){
      attack = -6.0
    } else if (input$attack == "Moderate"){
      attack = -3.0
    } else if (input$attack == "Shallow"){
      attack = 0.0
    }
    
    if (input$pitch == "Forward"){
      pitch = -3.0
    } else if (input$pitch == "Neutral"){
      pitch = 0.0
    }
    
    data.frame(
      speed = as.numeric(input$speed),
      attack = as.numeric(attack),
      pitch = as.numeric(pitch)
    )
  })
  
  swing_data <- eventReactive(input$predict, {
    
    test <- as.data.frame(swing_data_predict(swing_input_df()$speed, swing_input_df()$attack, swing_input_df()$pitch))
    swing_data <- predict_tahoe_lc(test, input$bs, input$la, input$backs, input$sa, input$sides)
    
    return(swing_data)
  })
  
  
  downrange_data <- reactive({
    
    swing_data <- as.data.frame(swing_data())
    swing_data <- swing_data %>% group_by(Model, Club) %>% summarise(Ball.Speed = mean(Ball.Speed),
                                                                     Launch.Angle = mean(Launch.Angle),
                                                                     Back.Spin = mean(Back.Spin),
                                                                     Side.Angle = mean(Side.Angle),
                                                                     Side.Spin = mean(Side.Spin))
    
    model_col_names <- c("Model", "Club", "Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
    colnames(swing_data) <- model_col_names
    #print(swing_data)
    aero_data <- aerotest(swing_data)
    downrange_data <- aero_data %>% select(carrydisp, carrydist)
    final_data <- cbind(swing_data, downrange_data)
    #print(final_data)
    
    return(final_data)
  })
  
  trajectory <- reactive({
    
    swing_data <- as.data.frame(swing_data())
    swing_data <- swing_data %>% group_by(Model, Club) %>% summarise(Ball.Speed = mean(Ball.Speed), 
                                                                     Launch.Angle = mean(Launch.Angle),
                                                                     Back.Spin = mean(Back.Spin), 
                                                                     Side.Angle = mean(Side.Angle),
                                                                     Side.Spin = mean(Side.Spin))
    
    model_col_names <- c("Model", "Club", "Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
    colnames(swing_data) <- model_col_names
    #print(swing_data)
    aero_data <- aerotest(swing_data)
    traj_X_data <- aero_data[["X yards"]]
    traj_X_data <- as.data.frame(do.call(cbind, traj_X_data))
    traj_X_data <- pivot_longer(traj_X_data, cols=everything()) %>% arrange(name)
    traj_X_data$Model[traj_X_data$name == "V1" | traj_X_data$name == "V2" | traj_X_data$name == "V3" | traj_X_data$name == "V4" | traj_X_data$name == "V5" | traj_X_data$name == "V6" | traj_X_data$name == "V7" | traj_X_data$name == "V8"] <- "Tahoe HL"
    traj_X_data$Model[traj_X_data$name == "V9" | traj_X_data$name == "V10" | traj_X_data$name == "V11" | traj_X_data$name == "V12" | traj_X_data$name == "V13" | traj_X_data$name == "V14" | traj_X_data$name == "V15" | traj_X_data$name == "V16"] <- "Tahoe Std"
    traj_X_data$Club[traj_X_data$name == "V1" | traj_X_data$name == "V9"] <- "4"
    traj_X_data$Club[traj_X_data$name == "V2" | traj_X_data$name == "V10"] <- "5"
    traj_X_data$Club[traj_X_data$name == "V3" | traj_X_data$name == "V11"] <- "6"
    traj_X_data$Club[traj_X_data$name == "V4" | traj_X_data$name == "V12"] <- "7"
    traj_X_data$Club[traj_X_data$name == "V5" | traj_X_data$name == "V13"] <- "8"
    traj_X_data$Club[traj_X_data$name == "V6" | traj_X_data$name == "V14"] <- "9"
    traj_X_data$Club[traj_X_data$name == "V7" | traj_X_data$name == "V15"] <- "PW"
    traj_X_data$Club[traj_X_data$name == "V8" | traj_X_data$name == "V16"] <- "AW"
    
    
    traj_Z_data <- aero_data[["Z yards"]]
    traj_Z_data <- as.data.frame(do.call(cbind, traj_Z_data))
    traj_Z_data <- pivot_longer(traj_Z_data, cols=everything()) %>% arrange(name)
    
    traj_Y_data <- aero_data[["Y yards"]]
    traj_Y_data <- as.data.frame(do.call(cbind, traj_Y_data))
    traj_Y_data <- pivot_longer(traj_Y_data, cols=everything()) %>% arrange(name)
    
    Traj <- data.frame(Model = as.factor(traj_X_data$Model),
                       Club = as.factor(traj_X_data$Club),
                       X.yards = traj_X_data$value,
                       Z.yards = traj_Z_data$value,
                       Y.yards = traj_Y_data$value)
    
    return(Traj) 
  })
  
  #Plots/Tables
  output$plottrajectory <- renderPlotly({
    
    axx <- list(
      title = "Distance (yds)"
    )
    
    axy <- list(
      nticks = 3,
      range = c(-10,10),
      title = "Dispersion (yds)"
    )
    
    axz <- list(
      title = "Height (ft)"
    )
    
    Data <- as.data.frame(trajectory())
    #print(range(Data$X.yards))
    #ggplot(NULL, aes(x = X.yards, y = Z.yards*3, color = Model, shape = Club)) +
    #  geom_point(data = trajectory()) + xlab("Distance (yds)") + ylab("Height (ft)") + theme_classic(base_size = 18)
    
    fig <- plot_ly(trajectory(), x = ~X.yards, y = ~Z.yards, color = ~Model, type = 'scatter', mode = 'lines', colors = c("red", "blue")) %>%
      layout(xaxis = list(title = 'Distance (yds)', showgrid = F, zeroline = F, range = c(0, 250)), 
             yaxis = list(title = '', showgrid = F, showticklabels = F, range = c(0, 66.66)))
    
    fig <- fig %>% add_lines(y = 0, x = range(Data$X.yards), line = list(color = "black", width = 2), inherit = FALSE, showlegend = FALSE)
    
    #fig <- plot_ly(trajectory(), x = ~X.yards)
    #fig <- fig %>% add_trace(y = ~Z.yards, name = 'test',mode = 'lines') 
    
    
    # layout(showlegend = FALSE,
    #        yaxis = list(showline= T, linewidth=2, linecolor='black', showticklabels = F)
    #layout(scene = list(xaxis=axx,yaxis=axz))
    
  })
  
  output$carrytable <- renderDataTable({
    
    swing_data <- as.data.frame(swing_data())
    swing_data <- swing_data %>% group_by(Model, Club) %>% summarise(Ball.Speed = mean(Ball.Speed),
                                                                     Launch.Angle = mean(Launch.Angle),
                                                                     Back.Spin = mean(Back.Spin),
                                                                     Side.Angle = mean(Side.Angle),
                                                                     Side.Spin = mean(Side.Spin))
    
    model_col_names <- c("Model", "Club", "Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
    colnames(swing_data) <- model_col_names
    aero_data <- aerotest(swing_data)
    downrange_data <- aero_data %>% select(carrydisp, carrydist)
    final_data <- cbind(swing_data, downrange_data)
    final_data.1 <- final_data %>% filter(Model == "Tahoe HL") %>% group_by(Model, Club) %>% summarise(Carry_Distance = round(mean(carrydist),0)) %>% arrange(desc(Carry_Distance)) %>% mutate(Gapping = round(Carry_Distance - lead(Carry_Distance, default = last(Carry_Distance)),0)) 
    final_data.2 <- final_data %>% filter(Model == "Tahoe Std") %>% group_by(Model, Club) %>% summarise(Carry_Distance = round(mean(carrydist),0)) %>% arrange(desc(Carry_Distance)) %>% mutate(Gapping = round(Carry_Distance - lead(Carry_Distance, default = last(Carry_Distance)),0)) 
    
    final_data <- cbind(final_data.1[2:4], final_data.2[3:4])
    colnames(final_data) <- c("Club", "Carry Distance (yds)", "Gapping (yds)", "Carry Distance (yds)", "Gapping (yds)")
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 1, ''),
          th(class = 'dt-center', colspan = 2, 'Tahoe HL'),
          th(class = 'dt-center', colspan = 2, 'Tahoe Std')
        ),
        tr(
          lapply(colnames(final_data), th)
        )
      )
    ))
    
    return(datatable(final_data, container = sketch, style = "bootstrap", options = list(pageLength = 15, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:4))), rownames= FALSE))
    
    #   return(datatable(final_data,     options = list(
    #   initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'background-color' : 'white', 'color' : 'black', 'height' : '30px', 'font-size' : '15px', 'border-bottom' : 'none'});}"), dom = 't', ordering = F, columnDefs = list(list(className = 'dt-center', targets = 1:3))
    # ), rownames = FALSE, colnames=c("Club", "Model", "Carry Distance (yds)")) %>%
    #   formatStyle(columns = 1:3,  color = 'black', backgroundColor = 'white', fontSize = '15px'))
    
  })
  
  
  #Fitter page functions
  launch_input_df <- reactive({
    
    data.frame(
      model = input$clubtype,
      bs = as.numeric(input$bs),
      la = as.numeric(input$la),
      backs = as.numeric(input$backs),
      sa = as.numeric(input$sa),
      sides = as.numeric(input$sides)
    )
  })

  
  launch_data <- eventReactive(input$predict2, {
    
    test <- as.data.frame(launch_input_df())

    return(test)
    
  })
  

  trajectory_7i <- reactive({
    
    lc <- as.data.frame(launch_data())
    model_col_names <- c("Model", "Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
    colnames(lc) <- model_col_names
    
    lc <- lc %>% group_by(Model) %>% summarise(Ballspeed = mean(Ballspeed), 
                                               LaunchAngle = mean(LaunchAngle),
                                               Backspin = mean(Backspin),
                                               SideAngle = mean(SideAngle),
                                               SideSpin = mean(SideSpin))
    
    aero_data <- aerotest(lc)
    traj_X_data <- aero_data[["X yards"]]
    traj_X_data <- as.data.frame(do.call(cbind, traj_X_data))
    traj_X_data <- pivot_longer(traj_X_data, cols=everything()) %>% arrange(name)
    traj_X_data$Model[traj_X_data$name == "V1"] <- "Gamer 7 Iron"
    traj_X_data$Club[traj_X_data$name == "V1" | traj_X_data$name == "V7"] <- "7"

    traj_Z_data <- aero_data[["Z yards"]]
    traj_Z_data <- as.data.frame(do.call(cbind, traj_Z_data))
    traj_Z_data <- pivot_longer(traj_Z_data, cols=everything()) %>% arrange(name)
    
    traj_Y_data <- aero_data[["Y yards"]]
    traj_Y_data <- as.data.frame(do.call(cbind, traj_Y_data))
    traj_Y_data <- pivot_longer(traj_Y_data, cols=everything()) %>% arrange(name)
    
    Traj <- data.frame(Model = as.factor(traj_X_data$Model),
                       Club = as.factor(traj_X_data$Club),
                       X.yards = traj_X_data$value,
                       Z.yards = traj_Z_data$value,
                       Y.yards = traj_Y_data$value)
    
    return(Traj) 
  })
  
  
  submit_tahoe <- eventReactive(input$predict3, {
    
    test <- as.data.frame(inverse_predict(input$clubtype, input$bs, input$la, input$backs, input$sa, input$sides))
    test_2 <- swing_data_predict_2(test)
    test_3 <- predict_tahoe_lc(test_2, input$bs, input$la, input$backs, input$sa, input$sides)
    
    return(test_3)
    
  })
  
  trajectory_test <- reactive({
    
    swing_data <- as.data.frame(submit_tahoe())
    swing_data <- swing_data %>% group_by(Model, Club) %>% summarise(Ball.Speed = mean(Ball.Speed), 
                                                                     Launch.Angle = mean(Launch.Angle),
                                                                     Back.Spin = mean(Back.Spin), 
                                                                     Side.Angle = mean(Side.Angle),
                                                                     Side.Spin = mean(Side.Spin))
    
    model_col_names <- c("Model", "Club", "Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
    colnames(swing_data) <- model_col_names
    #print(swing_data)
    aero_data <- aerotest(swing_data)
    traj_X_data <- aero_data[["X yards"]]
    traj_X_data <- as.data.frame(do.call(cbind, traj_X_data))
    traj_X_data <- pivot_longer(traj_X_data, cols=everything()) %>% arrange(name)
    traj_X_data$Model[traj_X_data$name == "V1" | traj_X_data$name == "V2" | traj_X_data$name == "V3" | traj_X_data$name == "V4" | traj_X_data$name == "V5" | traj_X_data$name == "V6" | traj_X_data$name == "V7" | traj_X_data$name == "V8"] <- "Tahoe HL"
    traj_X_data$Model[traj_X_data$name == "V9" | traj_X_data$name == "V10" | traj_X_data$name == "V11" | traj_X_data$name == "V12" | traj_X_data$name == "V13" | traj_X_data$name == "V14" | traj_X_data$name == "V15" | traj_X_data$name == "V16"] <- "Tahoe Std"
    traj_X_data$Club[traj_X_data$name == "V1" | traj_X_data$name == "V9"] <- "4"
    traj_X_data$Club[traj_X_data$name == "V2" | traj_X_data$name == "V10"] <- "5"
    traj_X_data$Club[traj_X_data$name == "V3" | traj_X_data$name == "V11"] <- "6"
    traj_X_data$Club[traj_X_data$name == "V4" | traj_X_data$name == "V12"] <- "7"
    traj_X_data$Club[traj_X_data$name == "V5" | traj_X_data$name == "V13"] <- "8"
    traj_X_data$Club[traj_X_data$name == "V6" | traj_X_data$name == "V14"] <- "9"
    traj_X_data$Club[traj_X_data$name == "V7" | traj_X_data$name == "V15"] <- "PW"
    traj_X_data$Club[traj_X_data$name == "V8" | traj_X_data$name == "V16"] <- "AW"
    
    traj_Z_data <- aero_data[["Z yards"]]
    traj_Z_data <- as.data.frame(do.call(cbind, traj_Z_data))
    traj_Z_data <- pivot_longer(traj_Z_data, cols=everything()) %>% arrange(name)
    
    traj_Y_data <- aero_data[["Y yards"]]
    traj_Y_data <- as.data.frame(do.call(cbind, traj_Y_data))
    traj_Y_data <- pivot_longer(traj_Y_data, cols=everything()) %>% arrange(name)
    
    Traj <- data.frame(Model = as.factor(traj_X_data$Model),
                       Club = as.factor(traj_X_data$Club),
                       X.yards = traj_X_data$value,
                       Z.yards = traj_Z_data$value,
                       Y.yards = traj_Y_data$value)
    
    return(Traj) 
  })
  
  
  output$testtable <- renderDataTable({
    
    swing_data <- as.data.frame(trajectory_test())
    
  })
  
  
  #Plots
  output$plottrajectory_gamer <- renderPlotly({
    
    
    scene = list(camera = list(eye = list(x = -.9, y = 1.75, z = 0)),
                 #aspectmode = "auto",
                 #aspectratio = list(x = 10),
                 xaxis = list(nticks = 10, title = "Distance (yds)", range = c(0, 250)),
                 yaxis = list(nticks = 3, range = c(max(trajectory_7i()$Y.yards) + 10,min(trajectory_7i()$Y.yards)  -10), title = "Dispersion (yds)"),
                 zaxis = list(title = "Height (yds)", range = c(0, 50))
                 )
                 
    Data <- as.data.frame(trajectory_7i())
    #print(range(Data$X.yards))
    #ggplot(NULL, aes(x = X.yards, y = Z.yards*3, color = Model, shape = Club)) +
    #  geom_point(data = trajectory()) + xlab("Distance (yds)") + ylab("Height (ft)") + theme_classic(base_size = 18)
    
    #fig <- plot_ly(trajectory_7i(), x = ~X.yards, y = ~Z.yards, color = ~Model, type = 'scatter', mode = 'lines', colors = c("red")) %>%
    #  layout(xaxis = list(title = 'Distance (yds)', showgrid = F, zeroline = T), 
    #         yaxis = list(title = 'Height (yds', showgrid = F, showticklabels = T))
    
    #fig <- fig %>% add_lines(y = 0, x = range(Data$X.yards), line = list(color = "black", width = 2), inherit = FALSE, showlegend = FALSE)
    
    fig <- plot_ly(trajectory_7i(), x = ~X.yards, y = ~Y.yards, z = ~Z.yards, type = 'scatter3d', mode = 'lines',
                   line = list(width = 6, colorscale = 'Accent'))
    
    fig <- fig %>% layout(scene = scene)

    # layout(showlegend = FALSE,
    #        yaxis = list(showline= T, linewidth=2, linecolor='black', showticklabels = F)
    #layout(scene = list(xaxis=axx,yaxis=axz))
    
  })
  
  output$plottrajectory_3dtest <- renderPlotly({
    
    
    scene = list(camera = list(eye = list(x = -1.85, y = 3.95, z = 1), center =  list(x = -.5, y = 0, z = 0)),
                 aspectmode = "manual", aspectratio = list(x=5,y=1,z=1),
                 #aspectratio = list(x = 10),
                 xaxis = list(nticks = 10, title = "Distance (yds)", range = c(0, 250), color = "white"),
                 yaxis = list(tickvals = as.list(seq(-10, 10, 10)), ticktext = as.list(c(-10, "", 10)), range = c(max(trajectory_test()$Y.yards) + 10,min(trajectory_test()$Y.yards)  -10), title = "Dispersion (yds)", color = "white", x = 1),
                 zaxis = list(tickvals = as.list(seq(0, 50, 50)), ticktext = as.list(c("", 50)), title = "Height (yds)", range = c(0, 50), color = "white"),
                 bgcolor = "black"
                 )
    
    Data <- unique(as.data.frame(trajectory_test()))
    print(Data %>% filter(Club == "4" | Club == "6", Model == "Tahoe HL"))
    
    #Data %>% group_by(Model, Club) %>% arrange(desc(X.Yards))
    #print(range(Data$X.yards))
    #ggplot(NULL, aes(x = X.yards, y = Z.yards*3, color = Model, shape = Club)) +
    #  geom_point(data = trajectory()) + xlab("Distance (yds)") + ylab("Height (ft)") + theme_classic(base_size = 18)
    
    #fig <- plot_ly(trajectory_7i(), x = ~X.yards, y = ~Z.yards, color = ~Model, type = 'scatter', mode = 'lines', colors = c("red")) %>%
    #  layout(xaxis = list(title = 'Distance (yds)', showgrid = F, zeroline = T), 
    #         yaxis = list(title = 'Height (yds', showgrid = F, showticklabels = T))
    
    #fig <- fig %>% add_lines(y = 0, x = range(Data$X.yards), line = list(color = "black", width = 2), inherit = FALSE, showlegend = FALSE)
    
    fig <- plot_ly(unique(trajectory_test()), x = ~X.yards, y = ~Y.yards, z = ~Z.yards, color = ~Model, colors = c("darkred", "blue"), type = 'scatter3d', mode = 'lines', split = ~Club,
                   line = list(width = 3))
    
    fig <- fig %>% layout(scene = scene, 
                          paper_bgcolor = "black", 
                          legend = list(
                            title = list(
                              text = "<b>Club Model</b>",
                              font = list(color = "white")),
                            borderwidth = 5,
                            font = list(
                              color = "white")
                            )
                          )
    
    # layout(showlegend = FALSE,
    #        yaxis = list(showline= T, linewidth=2, linecolor='black', showticklabels = F)
    #layout(scene = list(xaxis=axx,yaxis=axz))
    
  })
  
  
  output$plottrajectory_new <- renderPlotly({
    
    axx <- list(
      title = "Distance (yds)"
    )
    
    axy <- list(
      nticks = 3,
      range = c(-10,10),
      title = "Dispersion (yds)"
    )
    
    axz <- list(
      title = "Height (ft)"
    )
    
    Data <- as.data.frame(trajectory_test())
    #print(range(Data$X.yards))
    #ggplot(NULL, aes(x = X.yards, y = Z.yards*3, color = Model, shape = Club)) +
    #  geom_point(data = trajectory()) + xlab("Distance (yds)") + ylab("Height (ft)") + theme_classic(base_size = 18)
    
    fig <- plot_ly(trajectory_test(), x = ~X.yards, y = ~Z.yards, color = ~Model, type = 'scatter', mode = 'lines', colors = c("red", "blue")) %>%
      layout(xaxis = list(title = 'Distance (yds)', showgrid = F, zeroline = F, range = c(0, 250)), 
             yaxis = list(title = '', showgrid = F, showticklabels = F, range = c(0, 66.66)))
    
    fig <- fig %>% add_lines(y = 0, x = range(Data$X.yards), line = list(color = "black", width = 2), inherit = FALSE, showlegend = FALSE)
    
    #fig <- plot_ly(trajectory(), x = ~X.yards)
    #fig <- fig %>% add_trace(y = ~Z.yards, name = 'test',mode = 'lines') 
    
    
    # layout(showlegend = FALSE,
    #        yaxis = list(showline= T, linewidth=2, linecolor='black', showticklabels = F)
    #layout(scene = list(xaxis=axx,yaxis=axz))
    
  })
  
  output$carrytable_new <- renderDataTable({
    
    swing_data <- as.data.frame(submit_tahoe())
    swing_data <- swing_data %>% group_by(Model, Club) %>% summarise(Ball.Speed = mean(Ball.Speed),
                                                                     Launch.Angle = mean(Launch.Angle),
                                                                     Back.Spin = mean(Back.Spin),
                                                                     Side.Angle = mean(Side.Angle),
                                                                     Side.Spin = mean(Side.Spin))
    
    model_col_names <- c("Model", "Club", "Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
    colnames(swing_data) <- model_col_names
    aero_data <- aerotest(swing_data)
    downrange_data <- aero_data %>% select(carrydisp, carrydist)
    final_data <- cbind(swing_data, downrange_data)
    final_data.1 <- final_data %>% filter(Model == "Tahoe Std") %>% group_by(Model, Club) %>% summarise(Carry_Distance = round(mean(carrydist),0)) %>% arrange(desc(Carry_Distance)) %>% mutate(Gapping = round(Carry_Distance - lead(Carry_Distance, default = last(Carry_Distance)),0)) 
    final_data.2 <- final_data %>% filter(Model == "Tahoe HL") %>% group_by(Model, Club) %>% summarise(Carry_Distance = round(mean(carrydist),0)) %>% arrange(desc(Carry_Distance)) %>% mutate(Gapping = round(Carry_Distance - lead(Carry_Distance, default = last(Carry_Distance)),0)) 
    
    final_data <- cbind(final_data.1[2:4], final_data.2[3:4])
    colnames(final_data) <- c("Club", "Carry Distance (yds)", "Gapping (yds)", "Carry Distance (yds)", "Gapping (yds)")
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 1, ''),
          th(class = 'dt-center', colspan = 2, 'Tahoe Std'),
          th(class = 'dt-center', colspan = 2, 'Tahoe HL')
        ),
        tr(
          lapply(colnames(final_data), th)
        )
      )
    ))
    
    return(datatable(final_data, container = sketch, style = "bootstrap", options = list(pageLength = 15, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:4))), rownames= FALSE))
    
    #   return(datatable(final_data,     options = list(
    #   initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'background-color' : 'white', 'color' : 'black', 'height' : '30px', 'font-size' : '15px', 'border-bottom' : 'none'});}"), dom = 't', ordering = F, columnDefs = list(list(className = 'dt-center', targets = 1:3))
    # ), rownames = FALSE, colnames=c("Club", "Model", "Carry Distance (yds)")) %>%
    #   formatStyle(columns = 1:3,  color = 'black', backgroundColor = 'white', fontSize = '15px'))
    
  })
  
  output$lctable <- renderDataTable({
    
    swing_data <- as.data.frame(submit_tahoe())
    swing_data <- swing_data %>% group_by(Model, Club) %>% summarise(Ball.Speed = mean(Ball.Speed),
                                                                     Launch.Angle = mean(Launch.Angle),
                                                                     Back.Spin = mean(Back.Spin),
                                                                     Side.Angle = mean(Side.Angle),
                                                                     Side.Spin = mean(Side.Spin))
    
    model_col_names <- c("Model", "Club", "Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
    colnames(swing_data) <- model_col_names
    
    final_data.1 <- swing_data %>% filter(Model == "Tahoe Std") %>% group_by(Model, Club) %>% summarise(Ballspeed = round(mean(Ballspeed),1), LaunchAngle = round(mean(LaunchAngle), 1),Backspin = round(mean(Backspin), 1)) %>% arrange(desc(Ballspeed)) 
    final_data.2 <- swing_data %>% filter(Model == "Tahoe HL") %>% group_by(Model, Club) %>% summarise(Ballspeed = round(mean(Ballspeed),1), LaunchAngle = round(mean(LaunchAngle), 1),Backspin = round(mean(Backspin), 1)) %>% arrange(desc(Ballspeed)) 
    
    final_data <- cbind(final_data.1[2:5], final_data.2[3:5])
    colnames(final_data) <- c("Club", "Ball Speed", "Launch Angle", "Backspin", "Ball Speed", "Launch Angle", "Backspin")
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 1, ''),
          th(class = 'dt-center', colspan = 3, 'Tahoe Std'),
          th(class = 'dt-center', colspan = 3, 'Tahoe HL')
        ),
        tr(
          lapply(colnames(final_data), th)
        )
      )
    ))
    
    return(datatable(final_data, container = sketch, style = "bootstrap", options = list(pageLength = 15, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:6))), rownames= FALSE))
    
    #   return(datatable(final_data,     options = list(
    #   initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'background-color' : 'white', 'color' : 'black', 'height' : '30px', 'font-size' : '15px', 'border-bottom' : 'none'});}"), dom = 't', ordering = F, columnDefs = list(list(className = 'dt-center', targets = 1:3))
    # ), rownames = FALSE, colnames=c("Club", "Model", "Carry Distance (yds)")) %>%
    #   formatStyle(columns = 1:3,  color = 'black', backgroundColor = 'white', fontSize = '15px'))
    
  })
  
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)












# Extra functions

# output$aero <- renderDataTable({
# 
#   data <- as.data.frame(predict_tahoe_lc())
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
