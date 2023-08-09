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
#library("reticulate")
library('lme4')
library('DT')
library('ggeffects')
library('shiny')
library('shinyWidgets')
library('shinythemes')
library('plotly')
library('neuralnet')
#use_python("C:/Users/Quenten.hooker/AppData/Local/Programs/Python/Python39/python.exe", required=TRUE)
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
lm.Club.Speed.quad <- readRDS(file = "mem_speed_TTB.rda")
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
nn_inverse <- readRDS(file = "nn_12_8_5_7192023.rda")

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
  #print(player_speed_dif)
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

inverse_predict <- function(MonitorType, model, bs, la, backs, sa, sides, srt, sax) {
  
  
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
  
  if (MonitorType == "Foresight"){
    
  predict_data = data.frame(BallSpeedMph = as.numeric(bs),
                            BallLaunchAngleDeg =  as.numeric(la),
                            BallSideAngleDeg =  as.numeric(sa),
                            BallBackSpinRpm =  as.numeric(backs),
                            BallSideSpinRpm =  as.numeric(sides))}
  
  else {
  
  predict_data = data.frame(BallSpeedMph = as.numeric(bs),
                            BallLaunchAngleDeg =  as.numeric(la),
                            BallSideAngleDeg =  as.numeric(sa),
                            BallBackSpinRpm =  as.numeric(srt)*cos(((as.numeric(sax))*pi)/180),
                            BallSideSpinRpm =  as.numeric(srt)*sin(((as.numeric(sax))*pi)/180))}
  
  
  print(predict_data)
  
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
  
  #print(unscaled_ECPC)
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
  #Vertical.Impact.GMM <- c(-12.785854	, -11.919672, -10.302431, -9.718269, -8.286890, -7.219095, -6.297345, -5.297321)
  
  Vertical.Impact.GMM <- c(-18.785854	, -16.919672, -14.302431, -12.718269, -10.286890, -6.219095, -3.297345, 1.297321)
  
  
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
  
  #print(new_data %>% group_by(Model, Length) %>% summarise(mean(Club.Speed.quad), mean(Angle.Of.Attack.quad),mean(Pitch), mean(Vertical.Face.quad), ))
  
  #print(new_data)
  return(new_data)
}