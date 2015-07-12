plot1 <- function(){
  # Get the working directory
  fileDir <- getwd();
  
  # Assume the file name is given so it was hardcoded as main purpose is to demo the graph
  filename <- paste(fileDir,"household_power_consumption.txt",collapse=NULL,sep="/")
  
  # store outputfile name in a vairble for later use
  outPlotfileName <- paste(fileDir,"plot1.png",collapse=NULL,sep="/")
  
  # read /load all the  data by using read table
  powerConsumptionData <- read.table(filename,na.strings="?",header=TRUE,sep=";")
  
  # Subset the data to get rows for Feb 1st & 2nd of 2007 and where Global_active_power value is not NA.
  # as.Date is used for data comparision
  # Since we know only two days , compariosin with logical OR is used, if it is more that that for Loop and more generic approach can be taken
  febPowerdata <- subset(powerConsumptionData,is.na(powerConsumptionData$Global_active_power)==FALSE & as.Date(powerConsumptionData$Date,format='%d/%m/%Y')==as.Date("2007-02-01",format='%Y-%m-%d') | as.Date(powerConsumptionData$Date,format='%d/%m/%Y')==as.Date("2007-02-02",format='%Y-%m-%d'))
  
  # Open pnf device 
  png(outPlotfileName,height=480, width=480,bg="white")
 
   #Plot histogram as requested , use as.numeric to convertor factor to numeric vactor
   hist(as.numeric(febPowerdata$Global_active_power),xlab="Global Active Power(kilowatts)",main="Global Active Power" ,col="red")
 
    # Close the device off to save on to disk.
   dev.off()
}


