# exploratory data analysis - course project 2 # 

library(ggplot2)

# plot 1
png(file = "plot1.png")
plot(plot1$year, plot1$`sum(Emissions)`, 
     type = "l",
     xlab = "year",
     ylab = "PM2.5 emissions", 
     main = "PM2.5 emissions in the US")
dev.off()

# plot 2
png(file = "plot2.png")
plot(plot2$year, plot2$`sum(Emissions)`, 
     type = "l",
     xlab = "year",
     ylab = "PM2.5 emissions", 
     main = "PM2.5 emissions in Baltimore City")
dev.off()

# plot 3
png(file = "plot3.png")
g3 <- ggplot(plot3, aes(x = year, y = `sum(Emissions)`, color = type))
g3 + geom_line(size = 1.2) +
  labs(title = "PM2.5 emissions in Baltimore City - by type", x = "year", y = "PM2.5 emissions")
dev.off()

# plot 4
png(file = "plot4.png")
g4 <- ggplot(plot4, aes(x = year, y = `sum(Emissions)`))
g4 + geom_line(size = 1.2, color = "red") +
  labs(title = "PM2.5 emissions in the US related to coal combustion sources", x = "year", y = "PM2.5 emissions")
dev.off()

# plot 5
png(file = "plot5.png")
g5 <- ggplot(plot5, aes(x = year, y = `sum(Emissions)`, color = fuel))
g5 + geom_line(size = 1.2) +
  labs(title = "motor vehicle PM2.5 emissions in Baltimore City - by fuel type", x = "year", y = "PM2.5 emissions")
dev.off()

# plot 6
png(file = "plot6.png", width = 680, height = 380)
g6 <- ggplot(plot6, aes(x = year, y = emis, color = fuel)) + facet_grid(. ~ city)
g6 + geom_line(size = 1.2) + 
  labs(title = "motor vehicle PM2.5 emissions in LA and Baltimore - by fuel type", 
       x = "year", 
       y = "PM2.5 emissions - as % of the base year (1999 = 1)")
dev.off()