# main file

# Styling notes:
# Any programming notes that are to be deleted once some function/feature is implemented
# are preceeded by three hash marks, eg. '### Find a way to combine RH probe data with seePhase data.'
# Code that is commented out because it is not currently working or needed is preceeded with a #and no space
# Permanent comments begin with a # and a space.
# Other styling choices are defined in Google's R Style Guide, https://google.github.io/styleguide/Rguide.xml

# Import required packages (PMD)
source('helperFunctions.R')
require(reshape2)
require(ggplot2)
require(manipulate) #allows the creation of an interactive plot

# Define global variables (PMD)
t <- Sys.time()
timeStamp <-  strftime(t,"%Y-%m-%d_%H-%M-%S")

# The Script
settings.lines <- readLines('settings.txt') #reads the lines of the settings file into a character vector (PMD)

for (i in 1:length(settings.lines)){
  temp <- gsub("\\","/",settings.lines[i],fixed=TRUE)
  eval(parse(text=temp))
}

# Load VDS file into a list, vds
vds <- read.delim(vdsFilePath)

#set number of columns to save from VDS file
vds <- vds[1:vds.num.columns.keep] #removes columns beyond vds.num.columns.keep, see settings file (PMD)
vds <- vds[!is.na(vds[1]),] #removes empty cells, which are imported as NA (PMD)

elapsed.time <- genElapsedTime(vdsFilePath) #stores elapsed time of VDS method as a double, using helper function genElapsedTime (PMD)
#line below can be moved to helperfunction genElapsedTime
elapsed.time <- elapsed.time[!is.na(elapsed.time)] #removes blank cells from elapsed.time (PMD)

vds2 <- cbind(vds, elapsed.time) #creates a new list, vds2, with elapsed time as a column (PMD)

colnames(vds2)[vds.rh] = "rh" #changes RH column name from 'RH....' to 'rh' in vds2 (PMD)

#read in the SeePhase data (as a list) (PMD)
seePhase <- read.csv(dataFilePath)

##humidity when working
#data12 <- read.delim("Part 1 humidity.txt", stringsAsFactors = FALSE)
#colnames(data12) = c("probeTime","probe.rh","probe.temp")
#data13 <- fix_length(data12, nrow(data1))


elapsedTimeSec <- convertToSec(seePhase)
elapsedTimeHour <- elapsedTimeSec / 3600
#seePhase2 <- cbind(data1,elapsedTimeSec,elapsedTimeHour, data13)
seePhase2 <- cbind(seePhase, elapsedTimeSec, elapsedTimeHour) #adds elapsedTimeSec and elapsedTimeHour columns to SeePhase data (PMD)

#If we add the vds data to the seePhase data here we can plot a Phase vs Time graph with Temperature series
#As it stands, the Phase information is removed before the Temperature series is glued in from the VDS file
#create names for slicing later
name.seePhase.time = colnames(seePhase2)[seePhase.time] #assigns name from time column to variable; column is assigned by seePhase.time variable in settings file (PMD)
name.seePhase.temperature = colnames(seePhase2)[seePhase.temperature] #assigns name from temperature column to variable; column is assigned by seePhase.temperature variable in settings file (PMD)
name.seePhase.phase = colnames(seePhase2)[seePhase.phase] #assigns name from phase column to variable; column is assigned by seePhase.phase variable in settings file (PMD)
name.vds.rh = colnames(vds2)[vds.rh] #should return 'rh' (PMD)
name.vds.temperature = colnames(vds2)[vds.temperature] #should return 'Temp' (PMD)
name.vds.cCO2 = colnames(vds2)[vds.cCO2] #returns 'Concentration.of.Gas...'(PMD)
name.vds.cO2 = colnames(vds2)[vds.cO2] #returns 'elapsed.time'...should return O2 concentration (PMD)
name.vds.time = colnames(vds2)[vds.time] #returns 'time.s.' (PMD)
name.vds.include = colnames(vds2)[vds.include] #returns 'Include' (PMD)

#Generate a plot from the raw SeePhase data
createPlot <- function(x,y,xmin,xmax,ymin,ymax,offset_value,phase_data,vds.include){
  data35 <- extracting_points_offset(seePhase2, elapsed.time, offset_value)
  
  plot(x,y,type='l',xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  points(data35$elapsedTimeHour,data35[,seePhase.phase],col="red")
  data37 <- data35[vds2[,vds.include] == 0,]
  points(data37$elapsedTimeHour,data37[,seePhase.phase],col="green")
}

offset_value_main = 0

#Generate the interactive plot for finding an acceptable offset value
manipulate(
  {createPlot(seePhase2$elapsedTimeHour,seePhase2[,seePhase.phase],x.min,x.max,y.min,y.max,offset_value,seePhase.phase,vds.include)
    offset_value_main <<- offset_value},
  x.min = slider(0,as.integer(seePhase2$elapsedTimeHour[length(seePhase2$elapsedTimeHour)]),initial=0),
  x.max = slider(0,as.integer(seePhase2$elapsedTimeHour[length(seePhase2$elapsedTimeHour)]),initial=as.integer(seePhase2$elapsedTimeHour[length(seePhase2$elapsedTimeHour)])),
  y.min = slider(0,as.integer(max(seePhase2[,seePhase.phase]))+10,initial=0),
  y.max = slider(0,as.integer(max(seePhase2[,seePhase.phase]))+10,initial=as.integer(max(seePhase2[,seePhase.phase]))+10),
  offset_value = slider(-3600,3600,initial=0)
  
)

#Opportunity to stop the script if an acceptable offset value cannot be found
cat ("Press [enter] to continue, 1 = stop")
line <- readline()
if (line == '1'){stop("user stopped script")}

print(offset_value_main)
data35 <- extracting_points_offset(seePhase2, elapsed.time, offset_value_main) #grabs only the values at the offset points on the graph, cuts out much of the data rows
data36 <- cbind(data35,vds2) #combines SeePhase and VDS data into one list (PMD)


#comparing the results to those from sigmaplot
#data4 <- read.csv("diff.csv")
#points(data4$hour,data4$ph3,col='blue')

#making the table

#garbage data, since I don't have the timestamps to line everything up


#tf2 <- data36[c("Ph4","AnalogB","include","temp","rh", "Concentration.of.Gas....")]
#Removes many of the columns from data36
tf2 <- data36[c(name.seePhase.phase,name.seePhase.temperature,name.vds.include,name.vds.temperature,name.vds.rh,name.vds.cO2)]

#Assign the column number of name.vds.include to a variable
include.index <- grep(name.vds.include,colnames(tf2))

#Assign a character string to the interest.variable
interest.variable <- colnames(tf2)[6]

#Remove any incomplete data (is this necessary?, likely caused problems for Marvin or wouldn't be in the code)
tf3 <- tf2[complete.cases(tf2),]
#tf4 <- tf3[tf3$include > 0,]
#tf4 <- tf3[tf3$include > 1,]

#Determine which rows to keep
if (include.check == 0){
  #Keep only rows where include.index value is > 0
  tf4 <- tf3[tf3[,include.index] > 0,]
}
if (include.check > 1){
  #Keep only rows where include.index value == include.check value in settings file
  tf4 <- tf3[tf3[,include.index] == include.check,]
}

#Assign the column number of name.vds.temperature to a variable
temp.index <- grep(name.vds.temperature,colnames(tf2))

#create directory to store output files, and define the path to that directory
dir.name <- paste(timeStamp, name.seePhase.phase)
dir.create(dir.name)
tPath <- paste0(dir.name,"/")

#runs this code if CO2 experiement (Haven't worked on this yet, PMD)
if (process_type == '1'){
  e1<- split(tf4, tf4[,temp_index])
  
  write("TablePlot",file=paste0(tPath,"table_averaged.csv"))
  write("---------",file=paste0(tPath,"table_averaged.csv"),append=TRUE)
  
  ttPath <- paste0(tPath,"pieces/")
  dir.create(ttPath)
  
  
  for (i in 1:length(e1)){
    print(i)
    
    x_text = "table_check <- dcast(e1[[i]], AAAA1111 ~ BBBB2222, length, value.var='CCCC3333')"
    x_text = sub("AAAA1111",interest.variable,x_text)
    x_text = sub("BBBB2222",name.vds.rh,x_text)
    x_text = sub("CCCC3333",name.seePhase.phase,x_text)
    #table_check <- dcast(e1[[i]], concentration.co2 ~ rh, length, value.var="Ph4")
    eval(parse(text=x_text))
    
    print(table_check)
    write.csv(table_check,paste0(ttPath,"check_table",names(e1)[i],".csv"),row.names=FALSE)
    
    x_text2 = "table_averaged <- dcast(e1[[i]], AAAA1111 ~ BBBB2222, mean, value.var='CCCC3333')"
    x_text2 = sub("AAAA1111",interest.variable,x_text2)
    x_text2 = sub("BBBB2222",name.vds.rh,x_text2)
    x_text2 = sub("CCCC3333",name.seePhase.phase,x_text2)
    #table_averaged <- dcast(e1[[i]], concentration.co2 ~ rh, mean, value.var="Ph4")
    eval(parse(text=x_text2))
    
    print(table_averaged)
    write.csv(table_averaged,paste0(ttPath,"averaged_table",names(e1)[i],".csv"),row.names=FALSE)
    
    write(paste0("Temperature",names(e1)[i]),file=paste0(tPath,"table_averaged.csv"),append=TRUE)
    write("---------",file=paste0(tPath,"table_averaged.csv"),append=TRUE)
    write.table(table_averaged,paste0(tPath,"table_averaged.csv"),sep=",",append=TRUE,row.names=FALSE)
    
    t_plot <- melt(table_averaged, id.vars = interest.variable, variable.name = 'series')
    number <- names(e1)[i]
    plotId <- paste0("CO2 vs RH vs Temperature Plot for ",number)
    g <- ggplot(t_plot, aes_string(interest.variable,"value")) + geom_point(aes(colour = series)) + ggtitle(plotId)+xlab('CO2 Concentration')+ylab('Phase')
    g
    print(g)
    ggplotname <- paste0(ttPath,"plot_forTemperature_",names(e1)[i],".png")
    ggsave(file=ggplotname)
  }
  
  file.copy('settings.txt',tPath)
  
  write(paste0("offset_value_main = ",offset_value_main," #used for this set of run"),file=paste0(tPath,"settings.txt"),append=TRUE)
  write(paste0("process_type = ",process_type," #used for this set of run"),file=paste0(tPath,"settings.txt"),append=TRUE)
  write.csv(tf4,paste0(tPath,"selectedpoints.csv"),col.names=FALSE)
  write.csv(seePhase2,paste0(tPath,"rawdata.csv"),col.names=FALSE)
}

if (process_type == '2'){

  #e1<- split(tf4, tf4[,temp_index])
  
  #write("TablePlot",file=paste0(tPath,"table_averaged.csv"))
  #write("---------",file=paste0(tPath,"table_averaged.csv"),append=TRUE)
  
  #ttPath <- paste0(tPath,"pieces/")
  #dir.create(ttPath)
  
  
  x_text = "table_check <- dcast(tf4, AAAA1111 ~ BBBB2222, length, value.var='CCCC3333')"
  x_text = sub("AAAA1111",name.vds.cO2,x_text) #CHANGED NAME.VDS.RH TO O2 HERE
  x_text = sub("BBBB2222",name.vds.temperature,x_text)
  x_text = sub("CCCC3333",name.seePhase.phase,x_text)
  #table_check <- dcast(e1[[i]], concentration.co2 ~ rh, length, value.var="Ph4")
  #table_check <- dcast(tf4, rh ~ temp, length, value.var="Ph4")
  eval(parse(text=x_text))
  
  print(table_check) #allows user to verify that the correct data is being used
  write.csv(table_check,paste0(tPath,"check_table_RH.csv"),row.names=FALSE)
  
  x_text2 = "table_averaged <- dcast(tf4, AAAA1111 ~ BBBB2222, mean, value.var='CCCC3333')"
  x_text2 = sub("AAAA1111",name.vds.cO2,x_text2) #CHANGED FROM RH FOR MY O2 FULL CALIBRATION
  x_text2 = sub("BBBB2222",name.vds.temperature,x_text2)
  x_text2 = sub("CCCC3333",name.seePhase.phase,x_text2)
  #table_averaged <- dcast(e1[[i]], concentration.co2 ~ rh, mean, value.var="Ph4")
  #table_averaged <- dcast(tf4, rh ~ temp, mean, value.var="Ph4")
  eval(parse(text=x_text2))
  
  print(table_averaged)
  write.csv(table_averaged,paste0(tPath,"averaged_table_RH.csv"),row.names=FALSE)
  
#   write(paste0("Temperature",names(e1)[i]),file=paste0(tPath,"table_averaged.csv"),append=TRUE)
#   write("---------",file=paste0(tPath,"table_averaged.csv"),append=TRUE)
#   write.table(table_averaged,paste0(tPath,"table_averaged.csv"),sep=",",append=TRUE,row.names=FALSE)
  
  t_plot <- melt(table_averaged, id.vars = name.vds.cO2, variable.name = 'series') #CHANGED ID.VARS HERE FOR O2 FULL CALIBRATION
  #number <- names(e1)[i]
  plotId <- paste(name.seePhase.phase, "vs RH with Temperature Series")
  variable.series.plot <- ggplot(t_plot, aes_string(name.vds.cO2,"value")) + #CHANGED RH TO O2 HERE
    geom_point(aes(colour = series)) + 
    ggtitle(plotId) + xlab('RH%') + ylab(name.seePhase.phase)
  print(variable.series.plot)
  ggplotname <- paste0(tPath,"plot_RHvsT.png")
  ggsave(file=ggplotname)

# Plot the data as a function of time
  ### Still need to add temperature series to the time.series
  ### Should be able to do this with aggregate(), but need seePhase and VDS data in one df
  time.series <- aggregate(Ph1 ~ elapsedTimeSec, data = seePhase2, sum)
  plotId <- paste(name.seePhase.phase, "vs Time with Temperature Series")
  #plot(time.series, type = 'l')
  time.series.plot <- ggplot(time.series, aes(elapsedTimeSec, Ph1)) +
    geom_line() + 
    ggtitle(plotId) + xlab('Time') + ylab(name.seePhase.phase)
  print(time.series.plot)
  ggplotname <- paste0(tPath,"plot_DataVsTime.png")
  ggsave(file=ggplotname)

  #Make a copy of the settings file and place in processed data directory
  file.copy('settings.txt',tPath)
  
  #add the offset value and process type used to the new settings file
  write(paste0("offset_value_main = ",offset_value_main," #used for this set of run"),file=paste0(tPath,"settings.txt"),append=TRUE)
  write(paste0("process_type = ",process_type," #used for this set of run"),file=paste0(tPath,"settings.txt"),append=TRUE)
  write.csv(tf4,paste0(tPath,"selectedpoints.csv"),col.names=FALSE)
  write.csv(seePhase2,paste0(tPath,"rawdata.csv"),col.names=FALSE)
  
  
  
}