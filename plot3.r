##### LOAD PACKAGES #####

library(dplyr)

##### INIT #####
 
#set language to show weeksdays in English
Sys.setlocale("LC_TIME", "English")

##### INIT FUNCTIONS #####

#function for importing data
import_dataset<-function(){
        
        #import data from internet
        temp <- tempfile()
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
        df <- read.csv(unz(temp, "household_power_consumption.txt"),sep = ';',stringsAsFactors = FALSE)
        unlink(temp)
        #transform column names
        names(df)<-tolower(names(df))
        
        #subet data based on date
        df<-df[df$date %in% c("1/2/2007","2/2/2007"),]
        
        #convert variable "global_active_power" to numeric data type
        df$global_active_power<-as.numeric(df$global_active_power)
        
        #convert variable "voltage" to numeric data type
        df$voltage<-as.numeric(df$voltage)
        
        #convert variable "global_reactive_power" to numeric data type
        df$global_reactive_power<-as.numeric(df$global_reactive_power)
        
        #get datetime variable
        df$datetime<-strptime(paste(df$date,df$time),"%d/%m/%Y %H:%M:%S")
        
        #get week of the week from datetime
        df$weekday<-weekdays(df$datetime)
        
        
        #return dataframe
        df
}

#write plot to png
plot_to_png<-function(filename="plot.png"){
        dev.copy(png,file=filename)
        dev.off()
}

##### CORE PROGRAM #####

#import data
data<-import_dataset()

#create graphic on screen 
with(data,plot(datetime,sub_metering_1,pin=c(480,480),type='n',xlab="",ylab="Energy sub metering"))
with(data,points(datetime,sub_metering_1,type='l',col="black"))
with(data,points(datetime,sub_metering_2,type='l',col="red"))
with(data,points(datetime,sub_metering_3,type='l',col="blue"))
legend("topright",lwd=1, col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))


plot_to_png("plot3.png")

