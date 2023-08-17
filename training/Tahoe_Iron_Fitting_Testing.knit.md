
<!-- rnb-text-begin -->

---
title: "Tahoe Iron Fitting App - Data format and test"
author: "Quenten Hooker"
date: "4/17/2023"
output: 
  pdf_document: default
  html_document:
    df_print: paged
  word_document: default
  html_notebook: default
urlcolor: blue
---

- Goal
- someone enter in a few swing parameters and give downrange and trajectory data for a few clubs


<!-- rnb-text-end -->



<!-- rnb-text-begin -->


#Glob and format to merge Core panel data

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuXG4jIFBhdGggPC0gXCJFOi80NzE2L1NoYXJlZCBEb2N1bWVudHMvOTEzNjIvMDUgLSBQbGF5ZXIgVGVzdC9cIlxuIyBcbiMgZGF0LmZpbGVzICA8LSBsaXN0LmZpbGVzKHBhdGg9IFBhdGgsXG4jICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICByZWN1cnNpdmU9VCxcbiMgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIHBhdHRlcm49YyhcIi5jc3ZcIiksXG4jICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICBmdWxsLm5hbWVzPVQpXG4jIFxuIyAjZnVuY3Rpb24gZm9yIHJlYWRpbmcgZWFjaCBjZy9tb2lcbiMgcmVhZERhdEZpbGUgPC0gZnVuY3Rpb24oZikge1xuIyAgICNkYXQuZmwgPC0gcmVhZF9leGNlbChwYXRoID0gZiwgc2hlZXQ9IDEsIGNvbF9uYW1lcyA9IFRSVUUsIHNraXAgPSAwKVxuIyAgIGRhdC5mbCA8LSByZWFkLmNzdihmLCBoZWFkZXIgPSBUUlVFLCBzZXA9XCIsXCIsIHNraXAgPSA0LCAgXG4jICAgICAgICAgICAgICAgIG5hLnN0cmluZ3M9YyhcIk5BXCIsXCJOYU5cIixcIiBcIixcIlwiKSlcbiMgfVxuIyBcbiMgRGF0YS5RdWFkIDwtIGMoKVxuIyAgIFxuIyAjTG9vayBmb3Igc2hlZXQgbmFtZXMgdG8gaGVscCBzY3JhcGVyIFxuIyBmb3IgKGkgaW4gMTpsZW5ndGgoZGF0LmZpbGVzKSkge1xuIyAgXG4jICAgdGVtcCA8LSByZWFkRGF0RmlsZShkYXQuZmlsZXNbaV0pXG4jICAgdGVtcCRQbGF5ZXIgPC0gc3ViKFwiLiovXCIsIFwiXCIsIGRhdC5maWxlc1tpXSlcbiMgICB0ZW1wJFBsYXllciA8LSAgc3Vic3RyKHRlbXAkUGxheWVyLCAxLCBuY2hhcih0ZW1wJFBsYXllciktNClcbiMgICBEYXRhLlF1YWQgPC0gcmJpbmQoRGF0YS5RdWFkLCB0ZW1wKVxuIyAgIFxuIyB9XG4jIFxuIyBEYXRhLlF1YWQgXG4jIFxuIyBEYXRhLlF1YWQkQ2x1YltEYXRhLlF1YWQkQ2x1Yi5JRCA9PSBcIkNsdWIgQVwiXSA8LSBcIkFXXCJcbiMgRGF0YS5RdWFkJENsdWJbRGF0YS5RdWFkJENsdWIuSUQgPT0gXCJDbHViIEJcIl0gPC0gXCI5IElyb25cIlxuIyBEYXRhLlF1YWQkQ2x1YltEYXRhLlF1YWQkQ2x1Yi5JRCA9PSBcIkNsdWIgQ1wiXSA8LSBcIkFXXCJcbiMgRGF0YS5RdWFkJENsdWJbRGF0YS5RdWFkJENsdWIuSUQgPT0gXCJDbHViIERcIl0gPC0gXCI5IElyb25cIlxuIyBcbiMgRGF0YS5RdWFkJE1vZGVsW0RhdGEuUXVhZCRDbHViLklEID09IFwiQ2x1YiBEXCIgfCBEYXRhLlF1YWQkQ2x1Yi5JRCA9PSBcIkNsdWIgQ1wiIF0gPC0gXCJUYWhvZSBITFwiXG4jIERhdGEuUXVhZCRNb2RlbFtEYXRhLlF1YWQkQ2x1Yi5JRCA9PSBcIkNsdWIgQVwiIHwgRGF0YS5RdWFkJENsdWIuSUQgPT0gXCJDbHViIEJcIiBdIDwtIFwiVGFob2UgU3RkXCJcbiMgXG4jICNEYXRhLlF1YWQuMDMgPC0gRGF0YS5RdWFkXG4jICNEYXRhLlF1YWQuMDQgPC0gRGF0YS5RdWFkXG4jICNEYXRhLlF1YWQuMDUgPC0gRGF0YS5RdWFkXG4jIFxuIyBEYXRhLkNvbWJpbmVkIDwtIHJiaW5kKERhdGEuUXVhZC4wMywgRGF0YS5RdWFkLjA0LCBEYXRhLlF1YWQuMDUpXG4jIERhdGEuQ29tYmluZWQkVGFncyA8LSBEYXRhLkNvbWJpbmVkJE1vZGVsXG4jIERhdGEuQ29tYmluZWQkRnVsbFRhZyA8LSBwYXN0ZShEYXRhLkNvbWJpbmVkJENsdWIsIERhdGEuQ29tYmluZWQkTW9kZWwsIHNlcCA9IFwiLCBcIiApXG4jIFxuIyBEYXRhLkNvbWJpbmVkIDwtIERhdGEuQ29tYmluZWQgJT4lIHJlbG9jYXRlKFBsYXllciwgQ2x1YiwgVGFncywgRnVsbFRhZykgJT4lIHJlbG9jYXRlKFRpbWUsIC5hZnRlciA9IFZlcnRpY2FsLkZhY2UuSW1wYWN0Li5tbS4uLi4uaXMudXAuKSAlPiUgcmVsb2NhdGUoU2hvdC5Db2xvci4uUi5HLkIuLCAuYWZ0ZXIgPSBUaW1lKVxuIyBcbiMgY29sX25hbWVzIDwtIGMoXCJQbGF5ZXJcIiwgXCJDbHViXCIsIFwiVGFnc1wiLCBcIkZ1bGxUYWdcIiwgXCJTaG90LklEXCIsIFwiU2VsZWN0ZWRcIiwgXCJEYXRlLnF1YWRcIiwgXCJDbHViLlR5cGVcIiwgXCJDbHViLklEXCIsIFwiUmFuZ2UuQmFsbC5xdWFkXCIsIFwiQmFsbC5TcGVlZC5xdWFkXCIsXCJMYXVuY2guQW5nbGUucXVhZFwiLCBcIlNpZGUuQW5nbGUucXVhZFwiLCBcIlNpZGUuU3Bpbi5xdWFkXCIsIFwiQmFjay5TcGluLnF1YWRcIiwgXCJUaWx0LkFuZ2xlLnF1YWRcIiwgXCJTcGluLlJhdGUucXVhZFwiLCBcIlBlYWsuSGVpZ2h0LnF1YWRcIiwgXCJDYXJyeS5xdWFkXCIsIFwiUmFuZ2UucXVhZFwiLCBcIk9mZmxpbmUucXVhZFwiLCBcIkRlc2NlbnQuQW5nbGUucXVhZFwiLCBcIkNsdWIuU3BlZWQucXVhZFwiICxcIkVmZmljaWVuY3kucXVhZFwiLCBcIkFuZ2xlLk9mLkF0dGFjay5xdWFkXCIsIFwiQ2x1Yi5QYXRoLnF1YWRcIiwgXCJGYWNlLlRvLlRhcmdldC5xdWFkXCIsIFwiRmFjZS5Uby5QYXRoLnF1YWRcIiwgXCJMaWUucXVhZFwiLCBcIkxvZnQucXVhZFwiLCBcIkNsb3NpbmcuUmF0ZS5xdWFkXCIsIFwiSW1wYWN0LlNwZWVkLnF1YWRcIiwgXCJMYXRlcmFsLkZhY2UucXVhZFwiLCBcIlZlcnRpY2FsLkZhY2UucXVhZFwiLCBcIlRpbWVcIiwgXCJTaG90LkNvbG9yXCIsIFwiTW9kZWxcIilcbiMgY29sbmFtZXMoRGF0YS5Db21iaW5lZCkgPC0gY29sX25hbWVzXG4jIFxuIyAjd3JpdGUuY3N2KERhdGEuQ29tYmluZWQsIFwiQ29yZV9QYW5lbF9HbG9iLmNzdlwiKVxuXG5gYGAifQ== -->

```r

# Path <- "E:/4716/Shared Documents/91362/05 - Player Test/"
# 
# dat.files  <- list.files(path= Path,
#                                recursive=T,
#                                pattern=c(".csv"),
#                                full.names=T)
# 
# #function for reading each cg/moi
# readDatFile <- function(f) {
#   #dat.fl <- read_excel(path = f, sheet= 1, col_names = TRUE, skip = 0)
#   dat.fl <- read.csv(f, header = TRUE, sep=",", skip = 4,  
#                na.strings=c("NA","NaN"," ",""))
# }
# 
# Data.Quad <- c()
#   
# #Look for sheet names to help scraper 
# for (i in 1:length(dat.files)) {
#  
#   temp <- readDatFile(dat.files[i])
#   temp$Player <- sub(".*/", "", dat.files[i])
#   temp$Player <-  substr(temp$Player, 1, nchar(temp$Player)-4)
#   Data.Quad <- rbind(Data.Quad, temp)
#   
# }
# 
# Data.Quad 
# 
# Data.Quad$Club[Data.Quad$Club.ID == "Club A"] <- "AW"
# Data.Quad$Club[Data.Quad$Club.ID == "Club B"] <- "9 Iron"
# Data.Quad$Club[Data.Quad$Club.ID == "Club C"] <- "AW"
# Data.Quad$Club[Data.Quad$Club.ID == "Club D"] <- "9 Iron"
# 
# Data.Quad$Model[Data.Quad$Club.ID == "Club D" | Data.Quad$Club.ID == "Club C" ] <- "Tahoe HL"
# Data.Quad$Model[Data.Quad$Club.ID == "Club A" | Data.Quad$Club.ID == "Club B" ] <- "Tahoe Std"
# 
# #Data.Quad.03 <- Data.Quad
# #Data.Quad.04 <- Data.Quad
# #Data.Quad.05 <- Data.Quad
# 
# Data.Combined <- rbind(Data.Quad.03, Data.Quad.04, Data.Quad.05)
# Data.Combined$Tags <- Data.Combined$Model
# Data.Combined$FullTag <- paste(Data.Combined$Club, Data.Combined$Model, sep = ", " )
# 
# Data.Combined <- Data.Combined %>% relocate(Player, Club, Tags, FullTag) %>% relocate(Time, .after = Vertical.Face.Impact..mm.....is.up.) %>% relocate(Shot.Color..R.G.B., .after = Time)
# 
# col_names <- c("Player", "Club", "Tags", "FullTag", "Shot.ID", "Selected", "Date.quad", "Club.Type", "Club.ID", "Range.Ball.quad", "Ball.Speed.quad","Launch.Angle.quad", "Side.Angle.quad", "Side.Spin.quad", "Back.Spin.quad", "Tilt.Angle.quad", "Spin.Rate.quad", "Peak.Height.quad", "Carry.quad", "Range.quad", "Offline.quad", "Descent.Angle.quad", "Club.Speed.quad" ,"Efficiency.quad", "Angle.Of.Attack.quad", "Club.Path.quad", "Face.To.Target.quad", "Face.To.Path.quad", "Lie.quad", "Loft.quad", "Closing.Rate.quad", "Impact.Speed.quad", "Lateral.Face.quad", "Vertical.Face.quad", "Time", "Shot.Color", "Model")
# colnames(Data.Combined) <- col_names
# 
# #write.csv(Data.Combined, "Core_Panel_Glob.csv")

```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Read data and reformat

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


#Creating length and static loft columns

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# Filtering functions

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Modeling dynamics

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->






<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->





<!-- rnb-text-end -->



<!-- rnb-text-begin -->


## Ball Speed Evaluation

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Launch Angle Evaluation

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Backspin Evaluation

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Side Angle Evaluation

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Sidespin Evaluation

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Prep for building test data frame

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Test data frame

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## predict data frame

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Aero code

<!-- rnb-text-end -->



<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->



<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




Downrange Plots
---------------------------------------------------------------------------------------------------

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->



<!-- rnb-text-begin -->




<!-- rnb-text-end -->



<!-- rnb-text-begin -->





<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# Sim test for distance from opening for randy

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Advanced model building



#### standard 80-20 train test evaluation, Ball Speed through set

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


   
#### standard 80-20 train test evaluation, Launch Angle through set

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

   
   
#### standard 80-20 train test evaluation, Backspin through set

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

           
#### standard 80-20 train test evaluation, club head speed

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



#### standard 80-20 train test evaluation, attack

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


#### standard 80-20 train test evaluation, loft

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->

  
  

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Person prediction testing - Me

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



## Random simulations generating testing

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Engage
## Load Packages

<!-- rnb-text-end -->



<!-- rnb-text-begin -->



## Establish Connection

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


## Example SQL to grab data

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-plot-begin eyJoZWlnaHQiOjQzMi42MzI5LCJ3aWR0aCI6NzAwLCJzaXplX2JlaGF2aW9yIjowLCJjb25kaXRpb25zIjpbXX0= -->

<img src="data:image/png;base64,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" />

<!-- rnb-plot-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


#### MEM and RF, club head speed

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# NN for our inverse prediction problem using neuralnet package

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# NN for our inverse prediction problem using neuralnet package

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# xgboost attempt - clubheadspeed

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# xgboost attempt - attack

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# xgboost attempt - pitch

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# NN multiple outputs

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



# NN multiple outputs - including more outputs

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




## Auto ML using H20

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->



<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


# Notes, model does a poor job predicting low pitch, we do not have any data in this area?


<!-- rnb-text-end -->

