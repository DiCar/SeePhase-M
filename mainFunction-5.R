# main file
# Styling notes:
# Any programming notes that are to be deleted once some function/feature is implemented
# are preceeded by three hash marks, eg. '### Find a way to combine RH probe data with seePhase data.'
# Code that is commented out because it is not currently working or needed is preceeded with a #and no space
# Permanent comments begin with a # and a space.
# Other styling choices are defined in Google's R Style Guide, https://google.github.io/styleguide/Rguide.xml

### Improvements to make:
### Take an average for selected points
### Plot graphs of each temperature series with Phase and Mag vs time

# Import required packages (PMD)
source('helperFunctions.R')
library(reshape2)
library(ggplot2)
library(manipulate) #allows the creation of an interactive plot

# Define global variables (PMD)
t <- Sys.time()
timeStamp <-  strftime(t,"%Y-%m-%d_%H-%M-%S")

# The Script
settings.lines <- readLines('settings.txt') #reads the lines of the settings file into a character vector (PMD)

for (i in 1:length(settings.lines)){
  temp <- gsub("\\","/",settings.lines[i],fixed=TRUE)
  eval(parse(text=temp))
}

#Set the interest variable
if (process.type == '1'){
  interest.variable <- "CO2"
}
if (process.type == '2'){
  interest.variable <- "O2"
}
if (process.type == '3'){
  interest.variable <- "RH"
}   

# Load VDS file into a list, vds
vds <- read.delim(vdsFilePath)

# set number of columns to save from VDS file
vds <- vds[1:vds.num.columns.keep] #removes columns beyond vds.num.columns.keep, see settings file (PMD)
vds <- vds[!is.na(vds[1]),] #removes empty cells, which are imported as NA (PMD)

elapsed.time <- genElapsedTime(vdsFilePath) #stores elapsed time of VDS method as a double, using helper function genElapsedTime (PMD)
#line below can be moved to helperfunction genElapsedTime
elapsed.time <- elapsed.time[!is.na(elapsed.time)] #removes blank cells from elapsed.time (PMD)

vds2 <- cbind(vds, elapsed.time) #creates a new list, vds2, with elapsed time as a column (PMD)

colnames(vds2)[vds.rh] = "rh" #changes RH column name from 'RH....' to 'rh' in vds2 (PMD)

# read in the SeePhase data (as a list) (PMD)
seePhase <- read.csv(dataFilePath)

# Read in Vaisala data
vaisala <- read.delim(vaisala.file.path, header = TRUE)
vaisala <- vaisala[complete.cases(vaisala),]
#data12 <- read.delim("Part 1 humidity.txt", stringsAsFactors = FALSE)
#colnames(data12) = c("probeTime","probe.rh","probe.temp")
#data13 <- fix_length(data12, nrow(data1))


elapsedTimeSec <- convertToSec(seePhase)
elapsedTimeHour <- elapsedTimeSec / 3600
#seePhase2 <- cbind(data1,elapsedTimeSec,elapsedTimeHour, data13)
seePhase2 <- cbind(seePhase, elapsedTimeSec, elapsedTimeHour) #adds elapsedTimeSec and elapsedTimeHour columns to SeePhase data (PMD)

#If we add the vds data to the seePhase data here we can plot a Phase vs Time graph with Temperature series
#As it stands, the Phase information is removed before the Temperature series is glued in from the VDS file

#Assign column names from data files to variables (for slicing later)
name.seePhase.time = colnames(seePhase2)[seePhase.time] #assigns name from time column to variable; column is assigned by seePhase.time variable in settings file (PMD)
name.seePhase.temperature = colnames(seePhase2)[seePhase.temperature] #assigns name from temperature column to variable; column is assigned by seePhase.temperature variable in settings file (PMD)
name.seePhase.phase = colnames(seePhase2)[seePhase.phase] #assigns name from phase column to variable; column is assigned by seePhase.phase variable in settings file (PMD)
name.vds.rh = colnames(vds2)[vds.rh] #should return 'rh' (PMD)
name.vds.temperature = colnames(vds2)[vds.temperature] #should return 'Temp' (PMD)
name.vds.cCO2 = colnames(vds2)[vds.cCO2] #returns 'Concentration.of.Gas...'(PMD)
name.vds.cO2 = colnames(vds2)[vds.cO2] #returns 'elapsed.time'...should return O2 concentration (PMD)
name.vds.time = colnames(vds2)[vds.time] #returns 'time.s.' (PMD)
name.vds.include = colnames(vds2)[vds.include] #returns 'Include' (PMD)
name.vds.gastype = colnames(vds2)[vds.gastype]
name.vds.flowrate = colnames(vds2)[vds.flow]
name.vaisala.RH = colnames(vaisala)[vaisala.RH]
name.vaisala.temperature = colnames(vaisala)[vaisala.temperature]
name.vaisala.pwater = colnames(vaisala)[vaisala.pwater]


#Generate a plot from the raw SeePhase data
createPlot <- function(x,y,xmin,xmax,ymin,ymax,offset_value,phase_data,vds.include){
  data35 <- extracting_points_offset(seePhase2, elapsed.time, offset_value)
  
  #This plots seePhase data as a line plot, and then overlays a point plot with just the extracted points plotted  
  plot(x,y,type='l',xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  points(data35$elapsedTimeHour,data35[,seePhase.phase],col="red") # this puts a red dot at every elapsed time point, offset value initially set a zero
  data37 <- data35[vds2[,vds.include] == 0,] # this just turns the vds.include 0 dots green
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
data355 <- extracting_points_offset(vaisala, elapsed.time, offset_value_main)
data36 <- cbind(data35,vds2) #combines SeePhase and VDS data into one list (delete line, PMD)
data365 <- cbind(data35,vds2, data355) #combines SeePhase, vaisala, and VDS data into one list (PMD)
###################TEST INTERACTIVE PLOT FOR VAISALA DATA###########################
# #need to generate a data frame where the time variable is changed by the offset value
# linegraph_offset2 <- function(variableRH, time, offset){
#   for (i in time){
#     time[i] <- time[i] + offset
#   }
# }
# 
# 
# #Define CreatePlot function
# createPlot <- function(x,y,xmin,xmax,ymin,ymax,offset_value,phase_data){
#   temp.plot <- linegraph_offset2(vaisala$RH..., vaisala$VaisalaTimeHours, offset_value)
#   
#   plot(x,y,type='l',xlim=c(xmin,xmax),ylim=c(ymin,ymax))
#   lines(vaisala$VaisalaTimeHours,vaisala[,2],col="red")
#   points(temp.plot[,1],temp.plot[,2],col="red")
# }
# 
# offset_value_main = 0
# 
# #Generate the interactive plot for finding an acceptable offset value
# #manipulate loops everytime a slider is changes, thus runs create plot each time
# manipulate(
#   {createPlot(seePhase2$elapsedTimeHour,seePhase2[,seePhase.phase],x.min,x.max,y.min,y.max,offset_value,2)
#     offset_value_main <- offset_value},
#   x.min = slider(0,as.integer(vaisala$VaisalaTimeHours[length(vaisala$VaisalaTimeHours)]),initial=0),
#   x.max = slider(0,as.integer(vaisala$VaisalaTimeHours[length(vaisala$VaisalaTimeHours)]),initial=as.integer(vaisala$VaisalaTimeHours[length(vaisala$VaisalaTimeHours)])),
#   y.min = slider(0,as.integer(max(vaisala[,2]))+10,initial=0),
#   y.max = slider(0,as.integer(max(vaisala[,2]))+10,initial=as.integer(max(vaisala[,2]))+10),
#   offset_value = slider(-3600,3600,initial=0)
#   
# )
# 
# #Opportunity to stop the script if an acceptable offset value cannot be found
# cat ("Press [enter] to continue, 1 = stop")
# line <- readline()
# if (line == '1'){stop("user stopped script")}
# 
# print(offset_value_main)
# data35 <- extracting_points_offset(seePhase2, elapsed.time, offset_value_main) #grabs only the values at the offset points on the graph, cuts out much of the data rows
# data36 <- cbind(data35,vds2) #combines SeePhase and VDS data into one list (PMD)

#####################################################################################

#Removes many of the columns from data36
#tf2 <- data36[c(name.seePhase.phase,name.seePhase.temperature,name.vds.include,name.vds.temperature,name.vds.rh,name.vds.cO2)]
tf2 <- data365[c(name.seePhase.phase,name.seePhase.temperature,name.vds.include,name.vds.temperature,name.vds.flowrate, name.vds.rh,name.vds.cO2, name.vds.gastype, name.vds.cCO2, name.vaisala.RH, name.vaisala.temperature, name.vaisala.pwater)]

#Assign the column number of name.vds.include to a variable
include.index <- grep(name.vds.include,colnames(tf2))

#Assign a character string to the interest.variable
#interest.variable <- colnames(tf2)[6] ###not needed?

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
vds.temperature.index <- grep(name.vds.temperature,colnames(tf2))
vaisala.temperature.index<- grep(name.vaisala.temperature,colnames(tf2))

#create directory to store output files, and define the path to that directory
dir.name <- paste(timeStamp, name.seePhase.phase)
dir.create(dir.name)
tPath <- paste0(dir.name,"/")

#########################################   WORKING AREA   #######################################################
##################################################################################################################
##################################################################################################################

#runs this code if CO2 experiement (Haven't worked on this yet, PMD)
if (process.type == '1'){
  # Split the table of selected points into separate tables based on temperature value
  e1<- split(tf4, tf4[,vds.temperature.index])
  
  write("TablePlot",file=paste0(tPath,"table_averaged.csv"))
  write("---------",file=paste0(tPath,"table_averaged.csv"),append=TRUE)
  
  ttPath <- paste0(tPath,"pieces/")
  dir.create(ttPath)
  
  # Loop through each table created by the split
  for (i in 1:length(e1)){
    print(i)
    
    x_text = "table_check <- dcast(e1[[i]], AAAA1111 ~ BBBB2222, length, value.var='CCCC3333')"
    x_text = sub("AAAA1111",name.vds.cCO2,x_text)
    x_text = sub("BBBB2222",name.vds.rh,x_text)
    x_text = sub("CCCC3333",name.seePhase.phase,x_text)
    #table_check2 <- dcast(e1[[i]], name.vds.cCO2 ~ name.vds.rh, length, value.var="Ph1")
    eval(parse(text=x_text))
    
    print(table_check)
    write.csv(table_check,paste0(ttPath,"check_table",names(e1)[i],".csv"),row.names=FALSE)
    
    x_text2 = "table_averaged <- dcast(e1[[i]], AAAA1111 ~ BBBB2222, mean, value.var='CCCC3333')"
    x_text2 = sub("AAAA1111",name.vds.cCO2,x_text2)
    x_text2 = sub("BBBB2222",name.vds.rh,x_text2)
    x_text2 = sub("CCCC3333",name.seePhase.phase,x_text2)
    #table_averaged <- dcast(e1[[i]], concentration.co2 ~ rh, mean, value.var="Ph4")
    eval(parse(text=x_text2))
    
    print(table_averaged)
    
    # Write the table to the "pieces" folder
    write.csv(table_averaged,paste0(ttPath,"averaged_table",names(e1)[i],".csv"),row.names=FALSE)
    
    # Write/append the table to the main table_averaged file, with a heading and spacer ------
    write(paste0("Temperature",names(e1)[i]),file=paste0(tPath,"table_averaged.csv"),append=TRUE) #soft code the splitting variable
    write("---------",file=paste0(tPath,"table_averaged.csv"),append=TRUE)
    write.table(table_averaged,paste0(tPath,"table_averaged.csv"),sep=",",append=TRUE,row.names=FALSE)
    
    t_plot <- melt(table_averaged, id.vars = name.vds.cCO2, variable.name = 'series')
    number <- names(e1)[i]
    plotId <- paste0("CO2 vs RH vs Temperature Plot for ",number)
    g <- ggplot(t_plot, aes_string(name.vds.cCO2,"value")) + 
      geom_point(aes(colour = series)) + 
      geom_line(aes(colour = series)) +
      ggtitle(plotId)+xlab('CO2 Concentration')+ylab('Phase')
    print(g)
    ggplotname <- paste0(ttPath,"plot_forTemperature_",names(e1)[i],".png")
    ggsave(file=ggplotname)
    
  }
  
  #Make a copy of the settings file and place in processed data directory
  file.copy('settings.txt',tPath)
  
  #add the offset value and process type used to the new settings file
  write(paste0("offset_value_main = ",offset_value_main," #used for this set of run"),file=paste0(tPath,"settings.txt"),append=TRUE)
  write(paste0("process.type = ",process.type," #used for this set of run"),file=paste0(tPath,"settings.txt"),append=TRUE)
  write.csv(tf4,paste0(tPath,"selectedpoints.csv"),col.names=FALSE)
  write.csv(seePhase2,paste0(tPath,"rawdata.csv"),col.names=FALSE)
  
  
}

#########################################   END WORKING AREA   #######################################################
##################################################################################################################
##################################################################################################################

if (process.type == '2' | process.type =='3'){
  
  if (process.type == '2'){
    calibration.gas <- name.vds.cO2
  }
  if (process.type == '3'){
    calibration.gas <- name.vds.rh
  }
  

###Works for nominal vds data###
  x_text = "table.check.vds <- dcast(tf4, AAAA1111 ~ BBBB2222, length, value.var='CCCC3333')"
  x_text = sub("AAAA1111",calibration.gas,x_text) #CHANGED NAME.VDS.RH TO O2 HERE ###NEED TO SOFT CODE THIS
  x_text = sub("BBBB2222",name.vds.temperature,x_text)
  x_text = sub("CCCC3333",name.seePhase.phase,x_text)
  eval(parse(text=x_text))
  
  
  print(table.check.vds) #allows user to verify that the correct data is being used
  write.csv(table.check.vds, paste0(tPath, interest.variable, "_check_table_nominal.csv"),row.names=FALSE)
  
  x_text2 = "table.selectedpoints.vds <- dcast(tf4, AAAA1111 ~ BBBB2222, mean, value.var='CCCC3333')"
  x_text2 = sub("AAAA1111",calibration.gas,x_text2) #CHANGED FROM RH FOR MY O2 FULL CALIBRATION ###NEED TO SOFT CODE THIS
  x_text2 = sub("BBBB2222",name.vds.temperature,x_text2)
  x_text2 = sub("CCCC3333",name.seePhase.phase,x_text2)
  eval(parse(text=x_text2))
  
  print(table.selectedpoints.vds)
  write.csv(table.selectedpoints.vds,paste0(tPath, interest.variable, "_selectedpoints_table_nominal.csv"),row.names=FALSE)
  
  t_plot <- melt(table.selectedpoints.vds, id.vars = calibration.gas, variable.name = 'series') #CHANGED ID.VARS HERE FOR O2 FULL CALIBRATION ###NEED TO SOFT CODE THIS
  t_plot <- t_plot[!is.na(t_plot[3]),] #removes rows with empty cells in column 3, i.e. if data wasn't collected for some gas concentration/humidity combination
  df <- t_plot
  x <- calibration.gas  ###NEED TO SOFT CODE THIS
  y <- "value"
  
  ratio.normalized <- (max(df[x])-min(df[x]))/(max(df[y])-min(df[y]))
  ratio.plot <- ratio.normalized/ratio.aspect

  plotId <- paste(name.seePhase.phase, "vs", interest.variable, "with Temperature Series\nNominal Variable Values")
  variable.series.plot <- ggplot(t_plot, aes_string(calibration.gas,"value")) + #CHANGED RH TO O2 HERE ###NEED TO SOFT CODE THIS
    geom_point(aes(colour = series)) + 
    ggtitle(plotId) + xlab(paste(interest.variable, '(%)')) + ylab(name.seePhase.phase) +
    coord_fixed(ratio.plot)
  print(variable.series.plot)
  ggplotname <- paste0(tPath,"plot ", interest.variable, "vsT_nominal-", name.seePhase.phase, ".png")
  ggsave(file=ggplotname, width=width, height=height, units = c("cm"))

#This is for real temperature (Analog) and RH (Vaisala) data  
  x_text = "table.check.probes <- aggregate(CCCC3333 ~ AAAA1111 + BBBB2222, data = tf4, length)"
  x_text = sub("AAAA1111",name.vaisala.RH,x_text) #CHANGED NAME.VDS.RH TO O2 HERE ###NEED TO SOFT CODE THIS
  x_text = sub("BBBB2222",name.seePhase.temperature,x_text)
  x_text = sub("CCCC3333",name.seePhase.phase,x_text)
  eval(parse(text=x_text))
  
  print(table.check.probes) #allows user to verify that the correct data is being used
  write.csv(table.check.probes, paste0(tPath, interest.variable, "_check_table_probes.csv"),row.names=FALSE)
  
  x_text2 = "table.selectedpoints.probes <- aggregate(CCCC3333 ~ BBBB2222 + AAAA1111 + Temperature, data = tf4, mean)"
  x_text2 = sub("AAAA1111",name.vaisala.RH,x_text2) #CHANGED FROM RH FOR MY O2 FULL CALIBRATION ###NEED TO SOFT CODE THIS
  x_text2 = sub("BBBB2222",name.seePhase.temperature,x_text2)
  x_text2 = sub("CCCC3333",name.seePhase.phase,x_text2)
  eval(parse(text=x_text2))
  
  print(table.selectedpoints.probes)
  write.csv(table.selectedpoints.probes, paste0(tPath, interest.variable, "_selectedpoints_table_probes.csv"),row.names=FALSE)
  
  df <- table.selectedpoints.probes
  x <- name.vaisala.RH
  y <- name.seePhase.phase
  
  ratio.normalized <- (max(df[x])-min(df[x]))/(max(df[y])-min(df[y]))
  ratio.plot <- ratio.normalized/ratio.aspect
 
  plotId <- paste(name.seePhase.phase, "vs", interest.variable, "with Temperature Series\nProbe Variable Values")
  variable.series.plot <- ggplot(df, aes_string(name.vaisala.RH, name.seePhase.phase)) + #CHANGED RH TO O2 HERE ###NEED TO SOFT CODE THIS
    geom_point(aes(colour = Analog)) +
    scale_colour_gradient(limits=c(3, 34), low="blue", high = "red") +
    ggtitle(plotId) + xlab(paste(interest.variable, '(%)')) + ylab(name.seePhase.phase) +
    coord_fixed(ratio.plot)
  print(variable.series.plot)
  ggplotname <- paste0(tPath,"plot ", interest.variable, "vsT_probes-", name.seePhase.phase, ".png")
  ggsave(file=ggplotname, width=width, height=height, units = c("cm"))
  
# Plot the data as a function of time
  ### Still need to add temperature series to the time.series
  ### Should be able to do this with aggregate(), but need seePhase and VDS data in one df
  time.series <- aggregate(Ph2 ~ elapsedTimeSec + Analog, data = seePhase2, sum)
  plotId <- paste(name.seePhase.phase, "vs Time with Temperature Series")
  #plot(time.series, type = 'l')
  time.series.plot <- ggplot(time.series, aes(elapsedTimeSec, Ph2)) +
    geom_line(aes(colour = Analog)) +
    scale_colour_gradient(limits=c(3, 34), low="blue", high = "red") +
    ggtitle(plotId) + xlab('Time') + ylab(name.seePhase.phase)
  print(time.series.plot)
  ggplotname <- paste0(tPath,"plot_DataVsTime.png")
  ggsave(file=ggplotname)

  #Make a copy of the settings file and place in processed data directory
  file.copy('settings.txt',tPath)
  
  #add the offset value and process type used to the new settings file
  write(paste0("offset_value_main = ",offset_value_main," #used for this set of run"),file=paste0(tPath,"settings.txt"),append=TRUE)
  write(paste0("process.type = ",process.type," #used for this set of run"),file=paste0(tPath,"settings.txt"),append=TRUE)
  write.csv(tf4,paste0(tPath,"selectedpoints.csv"),col.names=FALSE)
  write.csv(seePhase2,paste0(tPath,"rawdata.csv"),col.names=FALSE)
  
  
  
}