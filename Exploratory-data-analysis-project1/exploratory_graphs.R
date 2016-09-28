ePowerData <- fread("household_power_consumption.txt", stringsAsFactors = FALSE,
                    colClasses = "Character")
l <- ePowerData[, Global_active_power != '?']
ePowerData <- ePowerData[l,] %>% 
        mutate( Datetime = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S"),
            Date = as.Date(Date, format= "%d/%m/%Y"),
            Global_active_power = as.numeric(Global_active_power),
            Global_reactive_power = as.numeric(Global_reactive_power),
            Voltage = as.numeric(Voltage),
            Global_intensity = as.numeric(Global_intensity),
            Sub_metering_1 = as.numeric(Sub_metering_1),
            Sub_metering_2 = as.numeric(Sub_metering_2),
            Sub_metering_3 = as.numeric(Sub_metering_3))

filteredData <- filter(ePowerData, Date == "2007-02-01" | Date == "2007-02-02")
#plot 1
png(filename = "plot1.png")
hist(filteredData$Global_active_power, col = "red", 
     xlab = "Global Active Power (kilowatts)", 
     main = "Global Active Power")
dev.off()
#plot 2
png(filename = "plot2.png")
plot(filteredData$Datetime, filteredData$Global_active_power, 
     type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
dev.off()

#plot 3
png(filename = "plot3.png")
plot(filteredData$Datetime, filteredData$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering")
points(filteredData$Datetime, filteredData$Sub_metering_1, type = "l")
points(filteredData$Datetime, filteredData$Sub_metering_2, type = "l", col = "red")
points(filteredData$Datetime, filteredData$Sub_metering_3, type = "l", col = "blue")
legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1,1,1))
dev.off()
#plot 4:

png(filename = "plot4.png")
par(mfrow = c(2,2))
plot(filteredData$Datetime, filteredData$Global_active_power, 
      type = "l", xlab = "", ylab = "Global Active Power")
plot(filteredData$Datetime, filteredData$Voltage, 
      type = "l", xlab = "datetime", ylab = "Voltage")
plot(filteredData$Datetime, filteredData$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering")
points(filteredData$Datetime, filteredData$Sub_metering_1, type = "l")
points(filteredData$Datetime, filteredData$Sub_metering_2, type = "l", col = "red")
points(filteredData$Datetime, filteredData$Sub_metering_3, type = "l", col = "blue")
legend("top", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = c(1,1,1), box.lwd = 0)
plot(filteredData$Datetime, filteredData$Global_reactive_power, 
     type = "l", xlab = "datetime", ylab = "Global_reactive_power")            
dev.off()
