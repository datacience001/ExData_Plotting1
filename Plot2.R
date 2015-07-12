plot2 <- function(){
  # Get the working directory
  fileDir <- getwd();
  
  # Assume the file name is given so it was hardcoded as main purpose is to demo the graph
  filename <- paste(fileDir,"household_power_consumption.txt",collapse=NULL,sep="/")
  
  # store outputfile name in a vairble for later use
  outPlotfileName <- paste(fileDir,"plot2.png",collapse=NULL,sep="/")
  
  # read /load all the  data by using read table
  powerConsumptionData <- read.table(filename,na.strings="?",header=TRUE,sep=";")
  
  # Subset the data to get rows for Feb 1st & 2nd of 2007 and where Global_active_power value is not NA.
  # as.Date is used for data comparision
  febPowerdata <- subset(powerConsumptionData,is.na(powerConsumptionData$Global_active_power)==FALSE & as.Date(powerConsumptionData$Date,format='%d/%m/%Y')==as.Date("2007-02-01",format='%Y-%m-%d') | as.Date(powerConsumptionData$Date,format='%d/%m/%Y')==as.Date("2007-02-02",format='%Y-%m-%d'))
 
   # Open png device 
  png(outPlotfileName,height=480, width=480,bg="white")
 
   #Plot the line graph requested , use as.numeric to convertor factor to numeric vactor, as.date with strptime is used to construct x axis.
  plot(strptime(paste(febPowerdata$Date, febPowerdata$Time, sep=" "), format = "%d/%m/%Y %H:%M:%S") ,as.numeric(febPowerdata$Global_active_power),ylab="Global Active Power(kilowatts)",xlab="", type="l",lwd="1.5")
  
   # Close the device off to save on to disk.
  dev.off()
}
