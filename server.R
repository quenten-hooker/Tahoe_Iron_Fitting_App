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
library("devtools")

#use_python("C:/Users/Quenten.hooker/AppData/Local/Programs/Python/Python39/python.exe", required=TRUE)
#pip install downrange --index-url https://pkgs.dev.azure.com/cgcRDClusterComputing/_packaging/callaway/pypi/simple/

# --- this works 
#virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')


# # Create virtual env and install dependencies
# reticulate::virtualenv_create(envname = virtualenv_dir, python = NULL)
# reticulate::virtualenv_install(virtualenv_dir, packages = c('pip', 'numpy'), ignore_installed=TRUE)
# reticulate::virtualenv_install("C:/Users/Quenten.hooker/AppData/Local/Programs/Python/Python39/Lib/site-packages/AeroCodePython",  envname = virtualenv_dir)
# #config <- reticulate::py_config()
# #system2(config$python, c("-m", "pip", "install", "--quiet", shQuote("C:/Users/Quenten.hooker/AppData/Local/Programs/Python/Python39/Lib/site-packages/AeroCodePython")))
# reticulate::use_virtualenv(virtualenv_dir, required = T)
# -- above works 


# # -- trying stuff, works too
# reticulate::virtualenv_create(envname = virtualenv_dir, python = NULL)
# reticulate::virtualenv_install(virtualenv_dir, packages = c('pip', 'numpy'), ignore_installed=TRUE)
# reticulate::virtualenv_install(virtualenv_dir, packages = c("-r", "requirements.txt"))
# reticulate::use_virtualenv(virtualenv_dir, required = T)
# # -- above works too

PYTHON_DEPENDENCIES = c('pip', 'numpy')
virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
python_path = Sys.getenv('PYTHON_PATH')

# Create virtual env and install dependencies
#reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
#reticulate::virtualenv_install(virtualenv_dir, packages =  PYTHON_DEPENDENCIES, ignore_installed=TRUE)
#reticulate::virtualenv_install(virtualenv_dir, packages = c("-r", "requirements.txt"))
reticulate::use_virtualenv(virtualenv_dir, required = T)

server <- function(input, output) {
  
  #pull in python functions
  reticulate::source_python('python_ref.py')
  
  output$monitor <- renderUI({
    if (input$Monitortype == "Foresight") {
      div(
        selectInput("clubtype", "Club", choices = c("Apex MB 21", "Apex TCB 21", "Apex Pro 21", "Paradym 23", "Paradym X 23")),
        #selectInput("ball", "Ball", choices = c("Chrome Soft X", "Chrome Soft X LS", "Chrome Soft")),
        sliderInput("bs",
                    label = strong(HTML('&nbsp;'), "Ball Speed (mph)"),
                    min = 80, max = 140, step = 5, value = c(120), width = '390px'),
        sliderInput("la",
                    label = strong(HTML('&nbsp;'), "Launch Angle (deg)"),
                    min = 5, max = 30, step = 1, value = c(16), width = '390px'),
        sliderInput("sa",
                    label = strong(HTML('&nbsp;'), "Side Angle (deg)"),
                    min = -5, max = 5, step = .5, value = c(0), width = '390px'),
        sliderInput("backs",
                    label = strong(HTML('&nbsp;'), "Backspin (rpm)"),
                    min = 4000, max = 9000, step = 500, value = c(7000), width = '390px'),
        sliderInput("sides",
                    label = strong(HTML('&nbsp;'), "Sidespin (rpm)"),
                    min = -2000, max = 2000, step = 250, value = c(0), width = '390px'),
        actionButton("predict3", "Predict Best Fit", width = '195px'),
        actionButton("predict.all", "Predict All Trajectories", width = '195px'),
        width = 5
      )
      }
    else {
      div(
        selectInput("clubtype", "Club", choices = c("Apex MB 21", "Apex TCB 21", "Apex Pro 21", "Apex DCB 21", "Paradym 23", "Paradym X 23")),
        #selectInput("ball", "Ball", choices = c("Chrome Soft X", "Chrome Soft X LS", "Chrome Soft")),
        sliderInput("bs",
                    label = strong(HTML('&nbsp;'), "Ball Speed (mph)"),
                    min = 80, max = 140, step = 5, value = c(120), width = '390px'),
        sliderInput("la",
                    label = strong(HTML('&nbsp;'), "Launch Angle (deg)"),
                    min = 5, max = 30, step = 1, value = c(16), width = '390px'),
        sliderInput("sa",
                    label = strong(HTML('&nbsp;'), "Side Angle (deg)"),
                    min = -5, max = 5, step = .5, value = c(0), width = '390px'),
        sliderInput("backs",
                    label = strong(HTML('&nbsp;'), "Spin Rate (rpm)"),
                    min = 4000, max = 9000, step = 500, value = c(7000), width = '390px'),
        sliderInput("sides",
                    label = strong(HTML('&nbsp;'), "Spin Axis (deg)"),
                    min = -10, max = 10, step = 1, value = c(0), width = '390px'),
        actionButton("predict3", "Predict Best Fit", width = '195px'),
        actionButton("predict.all", "Predict All Trajectories", width = '195px'),
        width = 5
      )
    }})
  
  
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
    swing_data <- predict_tahoe_lc(test, input$Monitortype, input$bs, input$la, input$backs, input$sa, input$sides)
    
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
    
    test <- as.data.frame(inverse_predict(input$Monitortype, input$clubtype, input$bs, input$la, input$backs, input$sa, input$sides))
    test_2 <- swing_data_predict_2(test)
    test_3 <- as.data.frame(predict_tahoe_lc(test_2, input$Monitortype, input$bs, input$la, input$backs, input$sa, input$sides))

    if (input$bs <= 100 | input$backs <= 5000 | input$la <= 12){
      test_3 <- test_3 %>% filter(Model == "Tahoe HL")
    } else if(input$bs >= 120 | input$backs >= 7500 | input$la >= 23) {
      test_3 <- test_3 %>% filter(Model == "Tahoe Std")
    }else{
      test_3 <- test_3
    }
    return(test_3)
    
  })
  
  submit_tahoe_best <- eventReactive(input$predict.best, {
    
    test <- as.data.frame(inverse_predict(input$Monitortype, input$clubtype, input$bs, input$la, input$backs, input$sa, input$sides))
    test_2 <- swing_data_predict_2(test)
    test_3 <- predict_tahoe_lc(test_2, input$Monitortype, input$bs, input$la, input$backs, input$sa, input$sides)
    
    return(test_3)
    
  })
  
  
  trajectory_test <- reactive({
    
    swing_data <- as.data.frame(submit_tahoe())
    swing_data <- within(swing_data, {
      Model <- factor(as.character(Model))
    })
    
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
    traj_X_data$Model[traj_X_data$name == "V1" | traj_X_data$name == "V2" | traj_X_data$name == "V3" | traj_X_data$name == "V4" | traj_X_data$name == "V5" | traj_X_data$name == "V6" | traj_X_data$name == "V7" | traj_X_data$name == "V8"] <- levels(swing_data$Model)[1]
    traj_X_data$Model[traj_X_data$name == "V9" | traj_X_data$name == "V10" | traj_X_data$name == "V11" | traj_X_data$name == "V12" | traj_X_data$name == "V13" | traj_X_data$name == "V14" | traj_X_data$name == "V15" | traj_X_data$name == "V16"] <- levels(swing_data$Model)[2]
    traj_X_data$Club[traj_X_data$name == "V1" | traj_X_data$name == "V9"] <- "4"
    traj_X_data$Club[traj_X_data$name == "V2" | traj_X_data$name == "V10"] <- "5"
    traj_X_data$Club[traj_X_data$name == "V3" | traj_X_data$name == "V11"] <- "6"
    traj_X_data$Club[traj_X_data$name == "V4" | traj_X_data$name == "V12"] <- "7"
    traj_X_data$Club[traj_X_data$name == "V5" | traj_X_data$name == "V13"] <- "8"
    traj_X_data$Club[traj_X_data$name == "V6" | traj_X_data$name == "V14"] <- "9"
    traj_X_data$Club[traj_X_data$name == "V7" | traj_X_data$name == "V15"] <- "AW"
    traj_X_data$Club[traj_X_data$name == "V8" | traj_X_data$name == "V16"] <- "PW"
    
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
    
    Traj$Club <- ordered(Traj$Club, levels = c("4", "5", "6", "7", "8", "9", "PW", "AW"))

    return(Traj) 
  })
  
  
  #Plots/Tables
  output$text_result <- renderText({
    
    if (length(unique(trajectory_test()$Model)) == 1){
      
      if (unique(trajectory_test()$Model) == "Tahoe HL"){
        "Based on your launch conditions, Tahoe HL is the best iron set for you"
      }    
      
      
      else if (unique(trajectory_test()$Model) == "Tahoe Std"){
        
        "Based on your launch conditions, Tahoe Std is the best iron set for you"

      }
    }
    else {
      
        "Based on your launch conditions, Tahoe Std and Tahoe HL could both be best for you"
      
    }
    
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
    
    #print(levels(trajectory_test()$Club))

    if (length(unique(trajectory_test()$Model)) == 1){
    
    if (unique(trajectory_test()$Model) == "Tahoe HL"){
      
      fig <- plot_ly()
      
      for (i in levels(trajectory_test()$Club)){
        
      plot_data <- unique(trajectory_test())%>% filter(Club == i)

      fig <- fig %>% add_trace(x = plot_data$X.yards, y = plot_data$Y.yards, z = plot_data$Z.yards, color = plot_data$Model, colors = c("darkred"), type = 'scatter3d', mode = 'lines', split = plot_data$Club,
                     line = list(width = 3)) 
      }
    }
      
      
      
    else if (unique(trajectory_test()$Model) == "Tahoe Std"){
      
      fig <- plot_ly()
      
      for (i in levels(trajectory_test()$Club)){
        
        plot_data <- unique(trajectory_test())%>% filter(Club == i)
        
        fig <- fig %>% add_trace(x = plot_data$X.yards, y = plot_data$Y.yards, z = plot_data$Z.yards, color = plot_data$Model, colors = c("blue"), type = 'scatter3d', mode = 'lines', split = plot_data$Club,
                                 line = list(width = 3)) 
      }
    }
    }
    else {
      
      fig <- plot_ly()
      
      for (i in levels(trajectory_test()$Model)){
        
        plot_data.1 <- unique(trajectory_test())%>% filter(Model == i)
      
      for (j in levels(trajectory_test()$Club)){
        
        plot_data <- unique(plot_data.1)%>% filter(Club == j)
        
        fig <- fig %>% add_trace(x = plot_data$X.yards, y = plot_data$Y.yards, z = plot_data$Z.yards, color = plot_data$Model, colors = c("darkred", "blue"), type = 'scatter3d', mode = 'lines', split = plot_data$Club,
                                 line = list(width = 3)) 
      }
      }
    }
    
    #print(apply(unique(trajectory_test()), 2, rev))
    
    #Data$Model <- factor(Data$Model, levels = c("4", "5", "6", "7", "8", "9", "PW", "AW"))
    #print(Data %>% filter(Club == "4" | Club == "6", Model == "Tahoe HL"))
    
    #Data %>% group_by(Model, Club) %>% arrange(desc(X.Yards))
    #print(range(Data$X.yards))
    #ggplot(NULL, aes(x = X.yards, y = Z.yards*3, color = Model, shape = Club)) +
    #  geom_point(data = trajectory()) + xlab("Distance (yds)") + ylab("Height (ft)") + theme_classic(base_size = 18)
    
    #fig <- plot_ly(trajectory_7i(), x = ~X.yards, y = ~Z.yards, color = ~Model, type = 'scatter', mode = 'lines', colors = c("red")) %>%
    #  layout(xaxis = list(title = 'Distance (yds)', showgrid = F, zeroline = T), 
    #         yaxis = list(title = 'Height (yds', showgrid = F, showticklabels = T))
    
    #fig <- fig %>% add_lines(y = 0, x = range(Data$X.yards), line = list(color = "black", width = 2), inherit = FALSE, showlegend = FALSE)
    

    
    fig <- fig %>% layout(scene = scene, 
                          paper_bgcolor = "black", 
                          legend = list(
                            title = list(
                              text = "<b>Club Model</b>",
                              font = list(color = "white")),
                            borderwidth = 5,
                            font = list(
                              color = "white")
                            #traceorder = "grouped"
                            #orientation = 'h'
                          )
    )
    
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
    downrange_data <- aero_data %>% select(carrydisp, carrydist, apexZft, landingangle)
    final_data <- cbind(swing_data, downrange_data)

    final_data.1 <- final_data %>% filter(Model == "Tahoe Std") %>% group_by(Model, Club) %>% summarise(Carry_Distance = round(mean(carrydist),0), Peak_Height = round(mean(apexZft)), Land_Angle = round(mean(landingangle))) %>% arrange(desc(Carry_Distance)) #%>% mutate(Gapping = round(Carry_Distance - lead(Carry_Distance, default = last(Carry_Distance)),0)) 
    final_data.2 <- final_data %>% filter(Model == "Tahoe HL") %>% group_by(Model, Club) %>% summarise(Carry_Distance = round(mean(carrydist),0), Peak_Height = round(mean(apexZft)), Land_Angle = round(mean(landingangle))) %>% arrange(desc(Carry_Distance)) #%>% mutate(Gapping = round(Carry_Distance - lead(Carry_Distance, default = last(Carry_Distance)),0)) 

    if (nrow(final_data.2) == 0){
      
      final_data <- final_data.1[2:5]
      colnames(final_data) <- c("Club", "Carry Distance (yds)", "Peak Height (ft)", "Land Angle (deg)")
      
      sketch = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(class = 'dt-center', colspan = 4, 'Tahoe Std')
          ),
          tr(
            lapply(colnames(final_data), th)
          )
        )
      ))
      return(datatable(final_data,container = sketch, style = "bootstrap", rownames= FALSE, options = list(lengthChange = FALSE, searching = FALSE, paging=FALSE)))
      
      
    }
    
    else if (nrow(final_data.1) == 0){
      
      final_data <- final_data.2[2:5]
      colnames(final_data) <- c("Club", "Carry Distance (yds)", "Peak Height (ft)", "Land Angle (deg)")
      
      print(final_data)
      
      sketch = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(class = 'dt-center', colspan = 4, 'Tahoe HL')
          ),
          tr(
            lapply(colnames(final_data), th)
          )
        )
      ))
      return(datatable(final_data, container = sketch, style = "bootstrap", rownames= FALSE, options = list(lengthChange = FALSE, searching = FALSE, paging=FALSE)))
      
    }
    
    else if (nrow(final_data.1) != 0 & nrow(final_data.1) != 0) {
      
      final_data <- cbind(final_data.1[2:5], final_data.2[3:5])
      colnames(final_data) <- c("Club", "Carry Distance (yds)", "Peak Height (ft)", "Land Angle (deg)", "Carry Distance (yds)", "Peak Height (ft)", "Lang Angle (deg)")
      
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
      return(datatable(final_data, container = sketch, style = "bootstrap", options = list(pageLength = 15, lengthChange = FALSE, searching = FALSE, paging=FALSE, columnDefs = list(list(className = 'dt-center', targets = 1:4))), rownames= FALSE))
      
    }
    

    #return(datatable(final_data, container = sketch, style = "bootstrap", options = list(pageLength = 15, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = 'dt-center', targets = 1:4))), rownames= FALSE))
    #return(datatable(final_data, rownames = NULL, style = "bootstrap", options = list(pageLength = 15, lengthChange = FALSE, searching = FALSE)))
    
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
  
  
  output$plottrajectory <- renderPlotly({
    
    scene = list(camera = list(eye = list(x = -1.85, y = 3.95, z = 1), center =  list(x = -.5, y = 0, z = 0)),
                 aspectmode = "manual", aspectratio = list(x=5,y=1,z=1),
                 #aspectratio = list(x = 10),
                 xaxis = list(nticks = 10, title = "Distance (yds)", range = c(0, 250), color = "white"),
                 yaxis = list(tickvals = as.list(seq(-10, 10, 10)), ticktext = as.list(c(-10, "", 10)), range = c(max(trajectory()$Y.yards) + 10,min(trajectory()$Y.yards)  -10), title = "Dispersion (yds)", color = "white", x = 1),
                 zaxis = list(tickvals = as.list(seq(0, 50, 50)), ticktext = as.list(c("", 50)), title = "Height (yds)", range = c(0, 50), color = "white"),
                 bgcolor = "black"
    )
    
    #print(apply(unique(trajectory()), 2, rev))
    
    #Data$Model <- factor(Data$Model, levels = c("4", "5", "6", "7", "8", "9", "PW", "AW"))
    #print(Data %>% filter(Club == "4" | Club == "6", Model == "Tahoe HL"))
    
    #Data %>% group_by(Model, Club) %>% arrange(desc(X.Yards))
    #print(range(Data$X.yards))
    #ggplot(NULL, aes(x = X.yards, y = Z.yards*3, color = Model, shape = Club)) +
    #  geom_point(data = trajectory()) + xlab("Distance (yds)") + ylab("Height (ft)") + theme_classic(base_size = 18)
    
    #fig <- plot_ly(trajectory_7i(), x = ~X.yards, y = ~Z.yards, color = ~Model, type = 'scatter', mode = 'lines', colors = c("red")) %>%
    #  layout(xaxis = list(title = 'Distance (yds)', showgrid = F, zeroline = T), 
    #         yaxis = list(title = 'Height (yds', showgrid = F, showticklabels = T))
    
    #fig <- fig %>% add_lines(y = 0, x = range(Data$X.yards), line = list(color = "black", width = 2), inherit = FALSE, showlegend = FALSE)
    
    fig <- plot_ly(unique(trajectory()), x = ~X.yards, y = ~Y.yards, z = ~Z.yards, color = ~Model, colors = c("darkred", "blue"), type = 'scatter3d', mode = 'lines', split = ~Club,
                   line = list(width = 3))
    
    fig <- fig %>% layout(scene = scene, 
                          paper_bgcolor = "black", 
                          legend = list(
                            title = list(
                              text = "<b>Club Model</b>",
                              font = list(color = "white")),
                            borderwidth = 5,
                            font = list(
                              color = "white"),
                            traceorder = "grouped"
                            #orientation = 'h'
                          )
    )
    
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
    
    downrange_data <- aero_data %>% select(carrydisp, carrydist, apexZft, landingangle)
    final_data <- cbind(swing_data, downrange_data)
    final_data.1 <- final_data %>% filter(Model == "Tahoe Std") %>% group_by(Model, Club) %>% summarise(Carry_Distance = round(mean(carrydist),0), Peak_Height = round(mean(apexZft)), Land_Angle = round(mean(landingangle))) %>% arrange(desc(Carry_Distance)) #%>% mutate(Gapping = round(Carry_Distance - lead(Carry_Distance, default = last(Carry_Distance)),0)) 
    final_data.2 <- final_data %>% filter(Model == "Tahoe HL") %>% group_by(Model, Club) %>% summarise(Carry_Distance = round(mean(carrydist),0), Peak_Height = round(mean(apexZft)), Land_Angle = round(mean(landingangle))) %>% arrange(desc(Carry_Distance)) #%>% mutate(Gapping = round(Carry_Distance - lead(Carry_Distance, default = last(Carry_Distance)),0)) 
    
    final_data <- cbind(final_data.1[2:5], final_data.2[3:5])
    colnames(final_data) <- c("Club", "Carry Distance (yds)", "Peak Height (ft)", "Land Angle (deg)", "Carry Distance (yds)", "Peak Height (ft)", "Lang Angle (deg)")
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(colspan = 1, ''),
          th(class = 'dt-center', colspan = 3, 'Tahoe HL'),
          th(class = 'dt-center', colspan = 3, 'Tahoe Std')
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
  
  
  
  
}

# Run the app ----
#shinyApp(ui = ui, server = server)





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


# output$plottrajectory_new <- renderPlotly({
#   
#   axx <- list(
#     title = "Distance (yds)"
#   )
#   
#   axy <- list(
#     nticks = 3,
#     range = c(-10,10),
#     title = "Dispersion (yds)"
#   )
#   
#   axz <- list(
#     title = "Height (ft)"
#   )
#   
#   Data <- as.data.frame(trajectory_test())
#   #print(range(Data$X.yards))
#   #ggplot(NULL, aes(x = X.yards, y = Z.yards*3, color = Model, shape = Club)) +
#   #  geom_point(data = trajectory()) + xlab("Distance (yds)") + ylab("Height (ft)") + theme_classic(base_size = 18)
#   
#   fig <- plot_ly(trajectory_test(), x = ~X.yards, y = ~Z.yards, color = ~Model, type = 'scatter', mode = 'lines', colors = c("red", "blue")) %>%
#     layout(xaxis = list(title = 'Distance (yds)', showgrid = F, zeroline = F, range = c(0, 250)), 
#            yaxis = list(title = '', showgrid = F, showticklabels = F, range = c(0, 66.66)))
#   
#   fig <- fig %>% add_lines(y = 0, x = range(Data$X.yards), line = list(color = "black", width = 2), inherit = FALSE, showlegend = FALSE)
#   
#   #fig <- plot_ly(trajectory(), x = ~X.yards)
#   #fig <- fig %>% add_trace(y = ~Z.yards, name = 'test',mode = 'lines') 
#   
#   
#   # layout(showlegend = FALSE,
#   #        yaxis = list(showline= T, linewidth=2, linecolor='black', showticklabels = F)
#   #layout(scene = list(xaxis=axx,yaxis=axz))
#   
# })
# output$plottrajectory_gamer <- renderPlotly({
#   
#   
#   scene = list(camera = list(eye = list(x = -.9, y = 1.75, z = 0)),
#                #aspectmode = "auto",
#                #aspectratio = list(x = 10),
#                xaxis = list(nticks = 10, title = "Distance (yds)", range = c(0, 250)),
#                yaxis = list(nticks = 3, range = c(max(trajectory_7i()$Y.yards) + 10,min(trajectory_7i()$Y.yards)  -10), title = "Dispersion (yds)"),
#                zaxis = list(title = "Height (yds)", range = c(0, 50))
#   )
#   
#   Data <- as.data.frame(trajectory_7i())
#   #print(range(Data$X.yards))
#   #ggplot(NULL, aes(x = X.yards, y = Z.yards*3, color = Model, shape = Club)) +
#   #  geom_point(data = trajectory()) + xlab("Distance (yds)") + ylab("Height (ft)") + theme_classic(base_size = 18)
#   
#   #fig <- plot_ly(trajectory_7i(), x = ~X.yards, y = ~Z.yards, color = ~Model, type = 'scatter', mode = 'lines', colors = c("red")) %>%
#   #  layout(xaxis = list(title = 'Distance (yds)', showgrid = F, zeroline = T), 
#   #         yaxis = list(title = 'Height (yds', showgrid = F, showticklabels = T))
#   
#   #fig <- fig %>% add_lines(y = 0, x = range(Data$X.yards), line = list(color = "black", width = 2), inherit = FALSE, showlegend = FALSE)
#   
#   fig <- plot_ly(trajectory_7i(), x = ~X.yards, y = ~Y.yards, z = ~Z.yards, type = 'scatter3d', mode = 'lines',
#                  line = list(width = 6, colorscale = 'Accent'))
#   
#   fig <- fig %>% layout(scene = scene)
#   
#   # layout(showlegend = FALSE,
#   #        yaxis = list(showline= T, linewidth=2, linecolor='black', showticklabels = F)
#   #layout(scene = list(xaxis=axx,yaxis=axz))
#   
# })