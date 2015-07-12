plot3 <- function(){
  # Get the working directory
  fileDir <- getwd();
  
  # Assume the file name is given so it was hardcoded as main purpose is to demo the graph
  filename <- paste(fileDir,"household_power_consumption.txt",collapse=NULL,sep="/")
  
  # store outputfile name in a vairble for later use
  outPlotfileName <- paste(fileDir,"plot3.png",collapse=NULL,sep="/")
  
  # read /load all the  data by using read table
  powerConsumptionData <- read.table(filename,na.strings="?",header=TRUE,sep=";")
  
  # Subset the data to get rows for Feb 1st & 2nd of 2007 and where Sub_metering_1 value is not NA.
  # as.Date is used for data comparision
  # three variables are used seperatly to make sure that each Sub_metering with NA is handled sepratly.
  # If we know data element is not having any NA for these 3 columns  ,only one variable can be used
  # for the safe side 3 variables are used to filter 3 subsets
  febEnergySubmeter1data <- subset(powerConsumptionData,is.na(powerConsumptionData$Sub_metering_1)==FALSE & as.Date(powerConsumptionData$Date,format='%d/%m/%Y')==as.Date("2007-02-01",format='%Y-%m-%d') | as.Date(powerConsumptionData$Date,format='%d/%m/%Y')==as.Date("2007-02-02",format='%Y-%m-%d'))
  febEnergySubmeter2data <- subset(powerConsumptionData,is.na(powerConsumptionData$Sub_metering_2)==FALSE & as.Date(powerConsumptionData$Date,format='%d/%m/%Y')==as.Date("2007-02-01",format='%Y-%m-%d') | as.Date(powerConsumptionData$Date,format='%d/%m/%Y')==as.Date("2007-02-02",format='%Y-%m-%d'))
  febEnergySubmeter3data <- subset(powerConsumptionData,is.na(powerConsumptionData$Sub_metering_3)==FALSE & as.Date(powerConsumptionData$Date,format='%d/%m/%Y')==as.Date("2007-02-01",format='%Y-%m-%d') | as.Date(powerConsumptionData$Date,format='%d/%m/%Y')==as.Date("2007-02-02",format='%Y-%m-%d'))
  
  # Open png device 
  png(outPlotfileName,height=480, width=480,bg="white")
  
  #Plot the line graph requested , use as.numeric to convertor factor to numeric vactor, as.date with strptime is used to construct x axis.
  plot(strptime(paste(febEnergySubmeter1data$Date, febEnergySubmeter1data$Time, sep=" "), format = "%d/%m/%Y %H:%M:%S") ,as.numeric(febEnergySubmeter1data$Sub_metering_1),xlab="",ylab="Energy Sub metering",type="l")
  lines(strptime(paste(febEnergySubmeter1data$Date, febEnergySubmeter1data$Time, sep=" "), format = "%d/%m/%Y %H:%M:%S"),as.numeric(febEnergySubmeter1data$Sub_metering_2),col="red")
  lines(strptime(paste(febEnergySubmeter3data$Date, febEnergySubmeter3data$Time, sep=" "), format = "%d/%m/%Y %H:%M:%S"),as.numeric(febEnergySubmeter3data$Sub_metering_3),col="blue")
  legend("topright",c("sub_metering_1","sub_metering_2","sub_metering_3"),lty=c(1,1,1),lwd=c(1.5,1.5,1.5),col=c("black","red","blue"))
  # Close the device off to save on to disk.
  dev.off()
}