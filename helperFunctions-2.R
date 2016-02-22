# This script reads in the Time File and will output an elapsed time file

#genElapsedTime <- function(filename,offset_num){
genElapsedTime <- function(filename){
  df <- read.delim(filename)  
  time_index <- 1
  num_rows <- nrow(df)
  
  elapsed.time <- numeric(length=num_rows)
  
  for (i in 1:num_rows){
    if (i == 1) {
      elapsed.time[i] <- df[i,time_index]# + offset_num
    }
    else {
      elapsed.time[i] <- df[i,time_index] + elapsed.time[i-1]# + offset_num 
    }
  }
  
  return(elapsed.time)
  
}

extract_points <- function(df,i) {
  #value_at_time <- which(abs(df$time-i)==min(abs(df$time-i)))
  
  value_at_time <- which(abs(df$elapsedTimeSec-i)==min(abs(df$elapsedTimeSec-i)))
  
  stats <- df[value_at_time,]
  
  return(stats)
  
}



convertToSec <- function(df){
  #will convert the first column to seconds
  time_index <- 1
  num_rows <- nrow(df)
  
  convert.time <- numeric(length=num_rows)
  
  for (j in 1:num_rows){
    if (j == 1) {
      convert.time[j] <- 0
    }
    else {
      convert.time[j] <- (df[j,time_index] - df[j-1,time_index])*60*60*24 
    }
  }
  
  
  elapsed.time <- numeric(length=num_rows)
  
  for (i in 1:num_rows){
    if (i == 1) {
      elapsed.time[i] <- convert.time[i]
    }
    else {
      elapsed.time[i] <- convert.time[i] + elapsed.time[i-1] 
    }
  }
  
  return(elapsed.time)
  
}

extracting_points <- function(df,time_seq) {
  spacing <- NULL
  
  #df <- read.csv(filepath,stringsAsFactors = FALSE, skip=2)
  #df <- df[,c(3,4,5,6,7,8,9,10,11,13)] #removing excess columns
  
  #names(df) <- c("time", "chan1", "chan2", "chan3", "chan4", "amb1","amb2","temp", "rh","led_exposures")
  
  for (i in 1:length(time_seq)){
    data <- extract_points(df,time_seq[i])
    
    spacing <- rbind(spacing,data)
  }
  return (spacing)
  
}

extracting_points_offset <- function(df,time_seq,offset) {
  spacing <- NULL
  
  #df <- read.csv(filepath,stringsAsFactors = FALSE, skip=2)
  #df <- df[,c(3,4,5,6,7,8,9,10,11,13)] #removing excess columns
  
  #names(df) <- c("time", "chan1", "chan2", "chan3", "chan4", "amb1","amb2","temp", "rh","led_exposures")
  
  for (i in 1:length(time_seq)){
    data <- extract_points(df,(time_seq[i]+offset))
    
    spacing <- rbind(spacing,data)
  }
  return (spacing)
  
}

optosen <- function(df) {
  
  #  step_index=1    #;Do NOT use in column 7 for anything
  DEV=0.1        #;Typical deviation
  med=5          #;Data medium
  v=0.2          #;Minimum jump
  
  
  #cell(7,2) is a step counter
  #cell(7,1) is a standardDevCheck, named "threshold"
  #col(2  is the data where the data is
  
  n=nrow(df)
  
  col4 = NULL #standard dev
  col3 = NULL #mean
  col5 = NULL #time
  
  threshold = NULL
  
  for (i in 1:(n-med)){
    
    #threshold[i] = sd(df$Ph4[i:(i+med)])
    threshold[i] = sd(df$Ph4[i:(i+med)])
    
    
    #         if (threshold[i] < DEV){
    #           col4[step_index] = sd(df$Ph4[i:(i+med)]) 
    #           col3[step_index] = mean(df$Ph4[i:(i+med)])
    #           col5[step_index] = df$elapsedTimeHour[i]
    #           step_index = step_index + 1
    #         }    
    
    
    #    threshold[i]=sd(df$Ph4[i:end_i])
    
    if (threshold[i] < DEV){
      col4[i] = sd(df$Ph4[i:(i+med)]) 
      col3[i] = mean(df$Ph4[i:(i+med)])
      col5[i] = df$elapsedTimeHour[(i+med)]
      #not sure why you'd add in 25 seconds. 
      
    }
  }
  
  df2 <- cbind(threshold,col3,col4,col5)
  df3 <- data.frame(df2[complete.cases(df2),])
  
  #either have the NA's and then remove them
  # or have the step counter.
  # will opt to have the NA's and then remove them
  
  
  m=nrow(df3)
  
  col8 = NULL 
  col9 = NULL 
  col10 = NULL 
  
  for (j in 1:(m-1)){
    a = df3$col3[j] - df3$col3[j+1]
    
    if (a > v || a < (-1*v)){
      col8[j] = df3$col3[j]
      col9[j] = df3$col4[j]
      col10[j] = df3$col5[j]
    } 
    
  }  
  
  df4 <- cbind(col8,col9,col10)
  df5 <- data.frame(df4[complete.cases(df4),])
  
  
  
  
  return(df2)
  
  
  
}

fix_length <- function(df,length) {
  spacing <- NULL
  
  n = nrow(df)
  
  probeTime = "NA"
  probe.rh = "NA"
  probe.temp = "NA"
  extendrow = data.frame(probeTime,probe.rh,probe.temp)
  
  if (n >= length){
    return (df[1:length,])
  }
  if (n < length){
    
    grow = length - n
    for (i in 1:grow){
      df = rbind(df, extendrow)
    }
    
    return (df)
  }
  
  
}

