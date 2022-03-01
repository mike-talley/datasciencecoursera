Week 1 Quiz: Issues with subsetting my data.

x <- hw1_data["Ozone" >31 & "Temp" >90, ]
  ## Does not work, just returns the entire df with no subsetting applied.

x <- hw1_data[hw1_data$Ozone > 31 & hw1_data$Temp > 90, ]
  ## This is a possible solution using brackets but it returns NAs.
x <- hw1_data[which(hw1_data$Ozone > 31 & hw1_data$Temp > 90), ]
  ## Wrapping it in which() solves it. 

x <- subset(hw1_data, Ozone > 31 & Temp > 90)
  ## Does work, returns a tbl of 10x6

-------------------------
  ## Returns NA value
mean(x["Solar.R"])
[1] NA
Warning message:
  In mean.default(x["Solar.R"]) :
  argument is not numeric or logical: returning NA

  ## Works correctly
mean(x$Solar.R)
[1] 212.8

-------------------------
  
x <- subset(hw1_data, Month == 5)
x <- hw1_data[hw1_data$Month == 5, ]

  ## This returns NA as the max. I would like like to find a streamlined way to write this, rather than
  ## the need for the multiple steps I took.
max(x$Ozone)
[1] NA

good <- complete.cases(x)
x[good, ]
  ## Tibble of 24x6, with NAs removed from Ozone.

y <- x[good, ]
max(y$Ozone)
[1] 115
