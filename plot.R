extract <- function() {
  data <- read.table("Household_power_consumption.txt", header = TRUE, sep = ";")
  data1 <- subset(data, data$Date == "1/2/2007" | data$Date == "2/2/2007")
  write.table(data1, "subdata.txt", sep = ";")
}

plot1 <- function() {
  data <- read.table("subdata.txt", header = TRUE, sep = ";")
  png("plot1.png")
  hist(data$Global_active_power, col = "red", border = "black", 
       xlab = "Global Active Power (kilowatts)", ylab = "Frequency", 
       main = "Global Active Power")
  dev.off() 
}

plot2 <- function() {
  data <- read.table("subdata.txt", header = TRUE, sep = ";")
  png("plot2.png")
  n <- 2 * 24 * 60 - 1
  t <- 0:n
  t <- t * 60
  t <- as.POSIXct(t, origin = "2007-2-1")
  y <- data$Global_active_power
  xlim = as.POSIXct(c(0, n * 60 ), origin = "2007-2-1")
  plot(t, y, type = "l", xlab = "",
       xaxp =c(xlim, 2),
       ylab = "Global Active Power (kilowatts)")
  dev.off()
}

plot3 <- function() {
  data <- read.table("subdata.txt", header = TRUE, sep = ";")
  png("plot3.png")
  n <- 2 * 24 * 60 - 1
  t <- 0:n
  t <- t * 60
  t <- as.POSIXct(t, origin = "2007-2-1")
  y <- data$Sub_metering_1
  y1 <- data$Sub_metering_2
  y2 <- data$Sub_metering_3
  labels <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  cols <- c("black", "red", "blue")
  c1 <- is.na(y)
  y[c1] <- 0
  c2 <- is.na(y1)
  y1[c2] <- 0
  c3 <- is.na(y2)
  y2[c3] <- 0
  xlim = as.POSIXct(c(0, n * 60 ), origin = "2007-2-1")
  ylim = c(0, 40)
  yaxp = c(0, 40, 4)
  plot(t, y, type = "l", col = "black", 
     xlab = "", xaxp = c(xlim, 2), xlim = xlim,
     ylab = "",
     ylim = ylim, yaxp = yaxp)
  par(new=T)
  plot(t, y1, type = "l", col = "red", 
     xlab = "", xaxp = c(xlim, 2), xlim = xlim,
     ylab = "",
     ylim = ylim, yaxp = yaxp)
  par(new=T)
  plot(t, y2, type = "l", col = "blue", 
     xlab = "", xaxp = c(xlim, 2), xlim = xlim,
     ylab = "Energy sub metering",
     ylim = ylim, yaxp = yaxp)
  legend("topright", legend = labels, col = cols, lty = 1)
  dev.off()
}

plot4 <- function() {
  data <- read.table("subdata.txt", header = TRUE, sep = ";")
  png("plot4.png")
  par(mfrow = c(2, 2))
  n <- 2 * 24 * 60 - 1
  t <- 0:n
  t <- t * 60
  t <- as.POSIXct(t, origin = "2007-2-1")
  y <- data$Global_active_power
  xlim = as.POSIXct(c(0, n * 60 ), origin = "2007-2-1")
  plot(t, y, type = "l", xlab = "",
       xaxp =c(xlim, 2),
       ylab = "Global Active Power (kilowatts)")
  y <- data$Voltage
  plot(t, y, type = "l", xlab = "datetime",
       xaxp =c(xlim, 2),
       ylab = "Voltage")

  y <- data$Sub_metering_1
  y1 <- data$Sub_metering_2
  y2 <- data$Sub_metering_3
  labels <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  cols <- c("black", "red", "blue")
  c1 <- is.na(y)
  y[c1] <- 0
  c2 <- is.na(y1)
  y1[c2] <- 0
  c3 <- is.na(y2)
  y2[c3] <- 0
  ylim = c(0, 40)
  yaxp = c(0, 40, 4)
  plot(t, y, type = "l", col = "black", 
       xlab = "", xaxp = c(xlim, 2), xlim = xlim,
       ylab = "",
       ylim = ylim, yaxp = yaxp)
  par(new=T)
  plot(t, y1, type = "l", col = "red", 
       xlab = "", xaxp = c(xlim, 2), xlim = xlim,
       ylab = "",
       ylim = ylim, yaxp = yaxp)
  par(new=T)
  plot(t, y2, type = "l", col = "blue", 
       xlab = "", xaxp = c(xlim, 2), xlim = xlim,
       ylab = "",
       ylim = ylim, yaxp = yaxp)
  legend("topright", legend = labels, col = cols, lty = 1, box.col = "white")
  par(new=T)
  tt <- c(0)
  yy <- c(0)
  plot(t, y, type = "l", col = "blue", 
       xlab = "", xaxp = c(xlim, 2), xlim = xlim,
       ylab = "Energy sub metering",
       ylim = ylim, yaxp = yaxp)  

  y <- data$Global_reactive_power
  plot(t, y, type = "l", xlab = "datetime",
       xaxp =c(xlim, 2),
       ylab = "Global_reactive_power")
  dev.off()
}
