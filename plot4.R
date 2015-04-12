## Function requires household_power_consumption.txt file in working directory, 
## obtained from: 
## https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
##
## This function pulls the data from this file for 2 days (Feb 1-2, 2007), and
## creates a 4 charts showing power usage over these 2 days. The
## chart plot4.png is created in the working directory.
##
## Note: this function uses the data.table package in R. The fread function
## greatly improves the file reading time.

plot4 <- function() {
      
      #      install.packages("data.table")
      ## Insure the data.table package is in the library
      library(data.table)
      ## Create d1 data.frame using fread. (Converting numeric columns to 
      ## character to save time from possible automatic coersion when fread
      ## hits an NA value.)
      d1 <- fread("./household_power_consumption.txt", sep = ";"
                  , header = TRUE, na.strings = "?", stringsAsFactors = TRUE
                  , colClasses = list(character=3:9), data.table = FALSE)
      ## Create a subset of the data for the 2 days required
      s1 <- subset(d1, subset = (d1$Date == "1/2/2007" | d1$Date == "2/2/2007"))
      ## Convert required columns to numeric
      s1[,3:9] <- lapply(s1[,3:9], as.numeric)
      ## Create a new date+time column from the Date and Time columns
      s1$DT <- strptime(paste(s1$Date, s1$Time), format="%d/%m/%Y %H:%M:%S")
      
      ## Open the png graphic file device to create
      png(filename="./plot4.png", width = 480, height = 480, units = "px")
      # Plot FOUR charts into the png file
      par(mfrow = c(2,2))
      # Plot 1
      plot(s1$DT, s1$Global_active_power, type = "n", 
            ylab = "Global Active Power", xlab = "")
      lines(s1$DT, s1$Global_active_power)

      # Plot 2
      plot(s1$DT, s1$Voltage, type = "n", 
           ylab = "Voltage", xlab = "datetime")
      lines(s1$DT, s1$Voltage)

      # Plot 3
      plot(s1$DT, s1$Sub_metering_1, type = "n", 
           ylab = "Energy sub metering", xlab = "")
      lines(s1$DT, s1$Sub_metering_1, col = "black")
      lines(s1$DT, s1$Sub_metering_2, col = "red")
      lines(s1$DT, s1$Sub_metering_3, col = "blue")
      legend("topright", lty = 1, col = c("black", "red", "blue"), 
            legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
            bty = "n")

      # Plot 4
      plot(s1$DT, s1$Global_reactive_power, type = "n", 
           ylab = "Global_reactive_power", xlab = "datetime")
      lines(s1$DT, s1$Global_reactive_power)
      
      ## Close the png file
      dev.off()
}