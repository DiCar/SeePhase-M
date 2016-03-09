genElapsedTime2 <- function(x){
  df <- read.delim(x)
  df
  time_index <- 1
  print(time_index)
  num_rows <- nrow(df)
  print(num_rows)
  
  elapsed.time <- numeric(length=num_rows)
  
  for (i in 1:num_rows){
    if (i == 1) {
      elapsed.time[i] <- df[i,time_index]# + offset_num
    }
    else {
      elapsed.time[i] <- df[i,time_index] + elapsed.time[i-1]# + offset_num 
    }
  }
  
  #return(elapsed.time)
  print(head(elapsed.time))
  
}

genElapsedTime2(vdsFilePath)
