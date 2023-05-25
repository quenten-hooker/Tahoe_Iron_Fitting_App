# This is a shiny web application that:
# 1) trains a model on player data through the bag
# 2) predicts launch conditions based on tahoe and tahoe hl specs
# 3) sends through Callaway aero code
# 4) plots results

# To do
# 1) go button?

# # Load Packages
library("ggplot2")
library("dplyr")
library("reticulate")
library('lme4')
library('DT')
library('ggeffects')
library('shiny')
library('shinyWidgets')
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

# Example input
player_pitch <- -5.0

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

print(mean(Lateral.Impact.GMM))

set.seed(14)
y <- rnorm(8, 0, 1)
Vertical.Impact.GMM <- scales::rescale(y, to = c(-20, 0), from = range(y))
print(mean(Vertical.Impact.GMM))


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
ui <- fluidPage(
  
  
  titlePanel(tagList(
    h1("Iron Fitting App - Beta",
      align = "center"
    )
  )),
  
  setSliderColor(c("darkred"), c(1)),
  sliderInput(
    "speed",
    label = strong(HTML('&nbsp;'), "Swing Speed"),
    min = 50,
    max = 110,
    value = c(80),
    width = '390px'
  ),
  selectInput("attack", "Angle of Attack", choices = c("Steep", "Moderate", "Shallow")),
  selectInput("lean", "Handle Lean", choices = c("Forward", "Neutral", "Backward")),
  
  # fluidRow(
  #   dataTableOutput("launch.conditions")
  # ),
  
  # fluidRow(
  #   dataTableOutput("aero")
  # ),
  
  fluidRow(
    plotOutput("plotdownrange")),
  
)

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
      lean = -5.0
    } else if (input$lean == "Neutral"){
      lean = 0.0
    } else if (input$lean == "Backward"){
      lean = 5.0
    }
    
    data.frame(
      speed = as.numeric(input$speed),
      attack = as.numeric(attack),
      lean = as.numeric(lean)
    )
  })
  
  
  
  
  downrange_data <- reactive({
    
    test <- as.data.frame(new_data_create(input_df()$speed, input_df()$attack, input_df()$lean))
    launch_data <- as.data.frame(model_lc(test))
    model_col_names <- c("Model", "Length", "Club", "Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
    colnames(launch_data) <- as.character(model_col_names)
    
    aero_data <- launch_data %>% select("Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
    aero_predict <- aerotest(aero_data)
    downrange_data <- aero_predict %>% select(carrydisp, carrydist)
    
    final_data <- cbind(launch_data, downrange_data)
    
    #print(final_data)
    
    return(final_data)

  })
  
  output$launch.conditions <- renderDataTable({

    data <- as.data.frame(model_lc())

    return(datatable(data))

    })

  output$aero <- renderDataTable({

    data <- as.data.frame(model_lc())
    model_col_names <- c("Model", "Length", "Club", "Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
    colnames(data) <- as.character(model_col_names)

    aero_data <- data %>% select("Ballspeed", "LaunchAngle", "Backspin", "SideAngle", "SideSpin")
    #aero_data <- as.data.frame(aero_data)
    test <- aerotest(aero_data)
    
    
    #print(test)
    #test <- as.data.frame(reticulate::py$test)
    #print(test)

    return(datatable(test))
    #returnedText = testMethod()

  })
  
  output$plotdownrange <- renderPlot({
    
    ggplot(NULL,  aes(x = carrydisp, y = carrydist, color = Model, shape = Club)) + geom_point(data = downrange_data()) +
    stat_ellipse(data = downrange_data(), level = 0.85, type = "t")+ theme_classic(base_size = 18)+theme(legend.position = "right") + 
    ylab("Carry.predict (yds)") + xlab("Offline.predict (yds)") + xlim(-5, 5)
    
      })
  
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)


