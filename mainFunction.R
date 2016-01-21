#main file



#CO2 work

source('helperFunctions.R')

foo <- readLines('settings.txt')

for (i in 1:length(foo)){
  temp <- gsub("\\","/",foo[i],fixed=TRUE)
  eval(parse(text=temp))
}

#load VDS file
df <- read.delim(vdsFilePath) #tab delim format

#set number of columns to save
df <- df[1:num_columns_keep_vds] 
df <- df[!is.na(df[1]),]

elapsed.time <- genElapsedTime(vdsFilePath)
elapsed.time <- elapsed.time[!is.na(elapsed.time)]

df2 <- cbind(df, elapsed.time)

colnames(df2)[rh_vds] = "rh"

#read in the CO2 data
data1 <- read.csv(dataFilePath)

##humidity when working
#data12 <- read.delim("Part 1 humidity.txt", stringsAsFactors = FALSE)
#colnames(data12) = c("probeTime","probe.rh","probe.temp")
#data13 <- fix_length(data12, nrow(data1))


elapsedTimeSec <- convertToSec(data1)
elapsedTimeHour <- elapsedTimeSec / 3600
#data2 <- cbind(data1,elapsedTimeSec,elapsedTimeHour, data13)
data2 <- cbind(data1,elapsedTimeSec,elapsedTimeHour)

#create names for slicing later
n_time_data = colnames(data2)[time_data]
n_temp_data = colnames(data2)[temp_data]
n_phase_data = colnames(data2)[phase_data]

n_rh_vds = colnames(df2)[rh_vds]
n_temp_vds = colnames(df2)[temp_vds]
n_cCO2_vds = colnames(df2)[cCO2_vds]
n_cO2_vds = colnames(df2)[cO2_vds]
n_time_vds = colnames(df2)[time_vds]
n_include_vds = colnames(df2)[include_vds]

createPlot <- function(x,y,xmin,xmax,ymin,ymax,offset_value,phase_data,include_column){
  data35 <- extracting_points_offset(data2, elapsed.time, offset_value)
  
  plot(x,y,type='l',xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  points(data35$elapsedTimeHour,data35[,phase_data],col="red")
  data37 <- data35[df2[,include_column] == 0,]
  points(data37$elapsedTimeHour,data37[,phase_data],col="green")
}


require(manipulate)

offset_value_main = 0

manipulate(
  {createPlot(data2$elapsedTimeHour,data2[,phase_data],x.min,x.max,y.min,y.max,offset_value,phase_data,include_vds)
    offset_value_main <<- offset_value},
  x.min = slider(0,as.integer(data2$elapsedTimeHour[length(data2$elapsedTimeHour)]),initial=0),
  x.max = slider(0,as.integer(data2$elapsedTimeHour[length(data2$elapsedTimeHour)]),initial=as.integer(data2$elapsedTimeHour[length(data2$elapsedTimeHour)])),
  y.min = slider(0,as.integer(max(data2[,phase_data]))+10,initial=0),
  y.max = slider(0,as.integer(max(data2[,phase_data]))+10,initial=as.integer(max(data2[,phase_data]))+10),
  offset_value = slider(-3600,3600,initial=0)
  
)


cat ("Press [enter] to continue, 1 = stop")
line <- readline()
if (line == '1'){stop("user stopped script")}

print(offset_value_main)
data35 <- extracting_points_offset(data2, elapsed.time, offset_value_main)
data36 <- cbind(data35,df2)


#comparing the results to those from sigmaplot
#data4 <- read.csv("diff.csv")
#points(data4$hour,data4$ph3,col='blue')

#making the table

#garbage data, since I don't have the timestamps to line everything up


#tf2 <- data36[c("Ph4","AnalogB","include","temp","rh", "Concentration.of.Gas....")]
tf2 <- data36[c(n_phase_data,n_temp_data,n_include_vds,n_temp_vds,n_rh_vds,n_cCO2_vds)]

include_index <- grep(n_include_vds,colnames(tf2))

colnames(tf2)[6] = interest_variable
tf3 <- tf2[complete.cases(tf2),]
#tf4 <- tf3[tf3$include > 0,]
#tf4 <- tf3[tf3$include > 1,]


if (include_check == 0){
  tf4 <- tf3[tf3[,include_index] > 0,]
}
if (include_check > 1){
  tf4 <- tf3[tf3[,include_index] == include_check,]
  
}



temp_index <- grep(n_temp_vds,colnames(tf2))



require(reshape2)
require(ggplot2)

t <- Sys.time()
timeStamp <-  strftime(t,"%Y-%m-%d_%H-%M-%S")


dir.create(timeStamp)

tPath <- paste0(timeStamp,"/")




if (process_type == '1'){
  e1<- split(tf4, tf4[,temp_index])
  
  write("TablePlot",file=paste0(tPath,"table_averaged.csv"))
  write("---------",file=paste0(tPath,"table_averaged.csv"),append=TRUE)
  
  ttPath <- paste0(tPath,"pieces/")
  dir.create(ttPath)
  
  
  for (i in 1:length(e1)){
    print(i)
    
    x_text = "table_check <- dcast(e1[[i]], AAAA1111 ~ BBBB2222, length, value.var='CCCC3333')"
    x_text = sub("AAAA1111",interest_variable,x_text)
    x_text = sub("BBBB2222",n_rh_vds,x_text)
    x_text = sub("CCCC3333",n_phase_data,x_text)
    #table_check <- dcast(e1[[i]], concentration.co2 ~ rh, length, value.var="Ph4")
    eval(parse(text=x_text))
    
    print(table_check)
    write.csv(table_check,paste0(ttPath,"check_table",names(e1)[i],".csv"),row.names=FALSE)
    
    x_text2 = "table_averaged <- dcast(e1[[i]], AAAA1111 ~ BBBB2222, mean, value.var='CCCC3333')"
    x_text2 = sub("AAAA1111",interest_variable,x_text2)
    x_text2 = sub("BBBB2222",n_rh_vds,x_text2)
    x_text2 = sub("CCCC3333",n_phase_data,x_text2)
    #table_averaged <- dcast(e1[[i]], concentration.co2 ~ rh, mean, value.var="Ph4")
    eval(parse(text=x_text2))
    
    print(table_averaged)
    write.csv(table_averaged,paste0(ttPath,"averaged_table",names(e1)[i],".csv"),row.names=FALSE)
    
    write(paste0("Temperature",names(e1)[i]),file=paste0(tPath,"table_averaged.csv"),append=TRUE)
    write("---------",file=paste0(tPath,"table_averaged.csv"),append=TRUE)
    write.table(table_averaged,paste0(tPath,"table_averaged.csv"),sep=",",append=TRUE,row.names=FALSE)
    
    t_plot <- melt(table_averaged, id.vars = interest_variable, variable.name = 'series')
    number <- names(e1)[i]
    plotId <- paste0("CO2 vs RH vs Temperature Plot for ",number)
    g <- ggplot(t_plot, aes_string(interest_variable,"value")) + geom_point(aes(colour = series)) + ggtitle(plotId)+xlab('CO2 Concentration')+ylab('Phase')
    g
    print(g)
    ggplotname <- paste0(ttPath,"plot_forTemperature_",names(e1)[i],".png")
    ggsave(file=ggplotname)
  }
  
  file.copy('settings.txt',tPath)
  
  write(paste0("offset_value_main = ",offset_value_main," #used for this set of run"),file=paste0(tPath,"settings.txt"),append=TRUE)
  write(paste0("process_type = ",process_type," #used for this set of run"),file=paste0(tPath,"settings.txt"),append=TRUE)
  write.csv(tf4,paste0(tPath,"selectedpoints.csv"),col.names=FALSE)
  write.csv(data2,paste0(tPath,"rawdata.csv"),col.names=FALSE)
}



if (process_type == '2'){
  
  
  #e1<- split(tf4, tf4[,temp_index])
  
  #write("TablePlot",file=paste0(tPath,"table_averaged.csv"))
  #write("---------",file=paste0(tPath,"table_averaged.csv"),append=TRUE)
  
  #ttPath <- paste0(tPath,"pieces/")
  #dir.create(ttPath)
  
  
  x_text = "table_check <- dcast(tf4, AAAA1111 ~ BBBB2222, length, value.var='CCCC3333')"
  x_text = sub("AAAA1111",n_rh_vds,x_text)
  x_text = sub("BBBB2222",n_temp_vds,x_text)
  x_text = sub("CCCC3333",n_phase_data,x_text)
  #table_check <- dcast(e1[[i]], concentration.co2 ~ rh, length, value.var="Ph4")
  #table_check <- dcast(tf4, rh ~ temp, length, value.var="Ph4")
  eval(parse(text=x_text))
  
  print(table_check)
  write.csv(table_check,paste0(tPath,"check_table_RH.csv"),row.names=FALSE)
  
  x_text2 = "table_averaged <- dcast(tf4, AAAA1111 ~ BBBB2222, mean, value.var='CCCC3333')"
  x_text2 = sub("AAAA1111",n_rh_vds,x_text2)
  x_text2 = sub("BBBB2222",n_temp_vds,x_text2)
  x_text2 = sub("CCCC3333",n_phase_data,x_text2)
  #table_averaged <- dcast(e1[[i]], concentration.co2 ~ rh, mean, value.var="Ph4")
  #table_averaged <- dcast(tf4, rh ~ temp, mean, value.var="Ph4")
  eval(parse(text=x_text2))
  
  print(table_averaged)
  write.csv(table_averaged,paste0(tPath,"averaged_table_RH.csv"),row.names=FALSE)
  
#   write(paste0("Temperature",names(e1)[i]),file=paste0(tPath,"table_averaged.csv"),append=TRUE)
#   write("---------",file=paste0(tPath,"table_averaged.csv"),append=TRUE)
#   write.table(table_averaged,paste0(tPath,"table_averaged.csv"),sep=",",append=TRUE,row.names=FALSE)
  
  t_plot <- melt(table_averaged, id.vars = n_rh_vds, variable.name = 'series')
  #number <- names(e1)[i]
  plotId <- "RH vs Temperature Plot"
  g <- ggplot(t_plot, aes_string(n_rh_vds,"value")) + geom_point(aes(colour = series)) + ggtitle(plotId)+xlab('RH%')+ylab('Phase')
  g
  print(g)
  ggplotname <- paste0(tPath,"plot_forRHvsT.png")
  ggsave(file=ggplotname)

  
  
  file.copy('settings.txt',tPath)
  
  write(paste0("offset_value_main = ",offset_value_main," #used for this set of run"),file=paste0(tPath,"settings.txt"),append=TRUE)
  write(paste0("process_type = ",process_type," #used for this set of run"),file=paste0(tPath,"settings.txt"),append=TRUE)
  write.csv(tf4,paste0(tPath,"selectedpoints.csv"),col.names=FALSE)
  write.csv(data2,paste0(tPath,"rawdata.csv"),col.names=FALSE)
  
  
  
}



