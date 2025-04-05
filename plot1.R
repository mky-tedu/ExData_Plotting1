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

hist(mydata$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")

png(file="plot1.png")
hist(mydata$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
dev.off()