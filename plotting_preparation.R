# exploratory data analysis - course project 2 # 

setwd("~/R/exdata/exdata-project2/")
library(dplyr)
library(tidyr)

download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "data/exdata_2.zip", method = "wget")
unzip("data/exdata_2.zip", exdir = "data")

## read data ###This first line will likely take a few seconds. Be patient!
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# str(NEI) # str(SCC)
# unique(NEI$SCC) # unique(SCC$SCC.Level.One)

# data for 1st plot
plot1 <- NEI %>% group_by(year) %>% 
  summarise(sum(Emissions))

# data for 2nd plot
plot2 <- NEI %>% 
  filter(fips == 24510) %>%
  group_by(year) %>% 
  summarise(sum(Emissions))

# data for 3rd plot
plot3 <- NEI %>% 
filter(fips == 24510) %>%
  group_by(year, type) %>% 
  summarise(sum(Emissions))

# data for 4th plot - ###coal### based on grepping SCC.Level.Four

coal_SCC <- SCC$SCC[grepl('coal', SCC$SCC.Level.Four, ignore.case = TRUE)]
coal_SCC <- as.character(coal_SCC)

coal <- filter(NEI, SCC %in% coal_SCC)
#unique(NEI$SCC[NEI$SCC %in% coal_SCC]) # check

plot4 <- coal %>%
  group_by(year) %>% 
  summarise(sum(Emissions))


# data for 5th plot - ###motor vehicles### based on grepping Short.Name 

  # check grepping ### SCC$Short.Name[grepl('highway veh - diesel', SCC$Short.Name, ignore.case = TRUE)]

  # 2 types - gasoline/diesel # You can also use the value = TRUE option and get the matching elements directly.
gasoline_SCC <- SCC$SCC[grepl('highway veh - gasoline', SCC$Short.Name, ignore.case = TRUE)]
diesel_SCC <- SCC$SCC[grepl('highway veh - diesel', SCC$Short.Name, ignore.case = TRUE)]

gasoline_SCC <- as.character(gasoline_SCC)
diesel_SCC <- as.character(diesel_SCC)

gasoline <- filter(NEI, SCC %in% gasoline_SCC) %>%
  mutate(fuel="gasoline")
diesel <- filter(NEI, SCC %in% diesel_SCC) %>%
  mutate(fuel="diesel")

MV <- rbind(gasoline,diesel)

plot5 <- MV %>%
  filter(fips == 24510) %>%
  group_by(year, fuel) %>% 
  summarise(sum(Emissions))


# data for 6th plot - similiar to 5th plot, but % changes

temp <- MV %>%
  filter(fips == "24510" | fips == "06037") %>%
  group_by(year, fuel, fips) %>% 
  summarise(emis = sum(Emissions))

  # 1999 as baseline for percent change
r1 <- temp$emis[temp$fips == "24510" & temp$fuel == "gasoline"] / temp$emis[temp$fips == "24510" & temp$year == 1999 & temp$fuel == "gasoline"]
r2 <- temp$emis[temp$fips == "24510" & temp$fuel == "diesel"] / temp$emis[temp$fips == "24510" & temp$year == 1999 & temp$fuel == "diesel"]
r3 <- temp$emis[temp$fips == "06037" & temp$fuel == "gasoline"] / temp$emis[temp$fips == "06037" & temp$year == 1999 & temp$fuel == "gasoline"]
r4 <- temp$emis[temp$fips == "06037" & temp$fuel == "diesel"] / temp$emis[temp$fips == "06037" & temp$year == 1999 & temp$fuel == "diesel"]

  # tidying
rs <- data.frame(Baltimore.gasoline = r1, 
                 Baltimore.diesel = r2,
                 LA.gasoline = r3,
                 LA.diesel = r4,
                 year = c(1999,2002,2005,2008))


plot6 <- gather(rs, key = city, value = emis, 1:4) %>%
  separate(city, into = c("city", "fuel"))



