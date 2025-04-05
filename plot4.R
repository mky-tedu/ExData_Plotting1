setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )

elecdata <- read.table("household_power_consumption.txt", sep=";", header= TRUE, na.strings = c("?"), colClasses = c("Date"="myDate"))
head(elecdata)
str(elecdata)
summary(elecdata)

# elecdata$Date <- as.Date(elecdata$Date, "%d/%m/%Y")
dstart <- as.Date("2007-02-01")
dfin <- as.Date("2007-02-02")

mydata <- elecdata[elecdata$Date>=dstart & elecdata$Date<=dfin,]

head(mydata)
str(mydata)
summary(mydata)

mydata$DT <- paste(strftime(mydata$Date), mydata$Time)
mydata$DT2 <- strptime(mydata$DT, "%Y-%m-%d %H:%M:%S")
mydata$WD <- weekdays(mydata$DT2, abbreviate = TRUE)

week_days <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
days <- intersect(mydata$WD,week_days)

lastday <- weekdays(mydata$DT2[length(mydata$DT2)], abbreviate = TRUE)
if (mydata$DT2[length(mydata$DT2)]$hour == 23 & mydata$DT2[length(mydata$DT2)]$min == 59) lastday = week_days[1+which(week_days == lastday)]

labelling = c()
for (i in 1:length(days) ) {
  labelling[i] = min(which(mydata$WD == days[i]))
}

labelling <- c(labelling, length(mydata$WD))

par(mfrow = c(2,2))

plot(mydata$Global_active_power, type="l", ylab = "Global Active Power (kilowatts)", xaxt = "n", xlab = "")
axis(1, at=labelling, labels = c(days,lastday))

plot(mydata$Voltage, type="l", ylab = "Voltage", xaxt = "n", xlab = "datetime")
axis(1, at=labelling, labels = c(days,lastday))

plot(mydata$Sub_metering_1, type="l", ylab = "Energy sub metering", xaxt = "n", xlab = "")
lines(mydata$Sub_metering_2, col = "red")
lines(mydata$Sub_metering_3, col = "blue")
axis(1, at=labelling, labels = c(days,lastday))
legend("topright", col = c("black","red","blue"), lty=c(rep(1,3)), colnames(mydata)[7:9])

plot(mydata$Global_reactive_power, type="l", ylab= "Global_reactive_power", xaxt = "n", xlab = "datetime")
axis(1, at=labelling, labels = c(days,lastday))

png(file="plot4.png")
par(mfrow = c(2,2))

plot(mydata$Global_active_power, type="l", ylab = "Global Active Power (kilowatts)", xaxt = "n", xlab = "")
axis(1, at=labelling, labels = c(days,lastday))

plot(mydata$Voltage, type="l", ylab = "Voltage", xaxt = "n", xlab = "datetime")
axis(1, at=labelling, labels = c(days,lastday))

plot(mydata$Sub_metering_1, type="l", ylab = "Energy sub metering", xaxt = "n", xlab = "")
lines(mydata$Sub_metering_2, col = "red")
lines(mydata$Sub_metering_3, col = "blue")
axis(1, at=labelling, labels = c(days,lastday))
legend("topright", col = c("black","red","blue"), lty=c(rep(1,3)), colnames(mydata)[7:9])

plot(mydata$Global_reactive_power, type="l", ylab= "Global_reactive_power", xaxt = "n", xlab = "datetime")
axis(1, at=labelling, labels = c(days,lastday))
dev.off()

