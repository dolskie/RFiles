################


# Read the source file which WHO.csv file; The table can be checked in the Environment Tab

WHO <- read.csv('WHO.csv')

# For you to be able to understand your dataset you may run names() to check column titles
names(WHO)

#Assignment 1;
#####1. WHO Dataset
#d. Country with the latest lowest literacy
    #Step 1. Find the lowest rate under column Literacy Rate
lit_min <- min(WHO$LiteracyRate, na.rm = TRUE)
print(lit_min)
    #Step 2. Find tha row with lowest literacy rate

lowest_lit_country <- subset(WHO, LiteracyRate == lit_min)
print(lowest_lit_country)
  #Print the country that has the lowest literacy rate
print(lowest_lit_country$Country)
#ANSWER:
#[1] Mali

#e. Richest country on the Europe based on GNI
  #Step 1. Set another data frame for Europe only using subset

europe_reg <- subset(WHO, Region == 'Europe')
print(europe_reg)


  #Step 2. Check the max value on GNI column

gni_euro <- max(europe_reg$GNI, na.rm = TRUE)
print(gni_euro)

  #Step 3. Find the row which has the max value on GNI

row_max <- subset(europe_reg, GNI == gni_euro)
print(row_max)
  #Step 4. Select the country with the max value in GNI
print(row_max$Country)

#ANSWER: [1] Luxembourg


#f. The Mean Life Expectancy of Africa
  #Step 1. Subset the Region of Africa

africa_reg <- subset(WHO, Region == 'Africa')

  #Step 2. Get the average life expectancy of Arica
africa_mean <- mean(africa_reg$LifeExpectancy)
print(africa_mean)

#ANSWER: [1] 57.95652

#g. Number of countries with Population greater than 10M
  #Step 1. Subset a over 10M value under Population Column
country_pop <- subset(WHO, Population > 10000)
print(country_pop)

  #Step 2. Count the total number of Countries which can be repsented by total number of rows

nrow(country_pop)
#ANSWER: [1] 86

#h. Top 5 of the countries in the Americas with the highest child mortality
  #Step 1. Subset the Americas from Region Column
americas_reg <- subset(WHO, Region == 'Americas')

  #Step 2. Set in order from highest to lowest 

americas_3index <- order(americas_reg$ChildMortality, decreasing = TRUE)
print(americas_3index)

  #Step 3. Get the order of the ChildMortality Rate
americas_3order <- americas_reg[americas_3index,]
print(americas_3order)
  #Step 4. Identity the top 5 highest Mortality Rate
americas_3top5 <- head(americas_3order,5)
print(americas_3top5)
  #Step 5. To narrow down the closest answer get the matrix
americas_3top5_index <- americas_3top5[1:5]
print(americas_3top5_index)

#Answer: HAITI, Bolivia, Guyana, Gautemala, Dominican Republic

####### NBA Dataset

##Run readxl package
library('readxl')

## Name the file and store in to a dataframe
nba_historyset <-read_excel("Historical NBA Performance.xlsx")

## Check column names
names(nba_historyset)

#a. The year bulls has the highest winning percentage
  #Step 1. Subset the team Bulls
bulls_team <- subset(nba_historyset, Team == 'Bulls')
  #Step 2. Get the max of Winnig Percentage
bulls_winprcnt <- max(bulls_team$`Winning Percentage`)
print(bulls_winprcnt)
  #Step 3. Find the row with the highest winning perentage
bulls_winprcnt_row <- subset(bulls_team, `Winning Percentage` == bulls_winprcnt)
print(bulls_winprcnt_row)
  # Print the year of where bulls got the highest percentage
print(bulls_winprcnt_row$Year)

#ANSWER: 1995-96

#b. Teams with an even win loss record in a year

even_WL <- subset(nba_historyset, `Winning Percentage` == 0.5)
print(even_WL)

#ANSWER: There are 53 teams (please see table for details)

######### Season Stats

## Read, name a file and store in to a dataframe
stat_season <- read.csv("Seasons_Stats.csv")

# Check for the column names
names(stat_season)

#a. Player the highest 3-pt attempt rate in a season

  # Get the highest 3pt attempt from column X3PAR
high_x3ptAR <- max(stat_season$X3P., na.rm =TRUE)
print(high_x3ptAR)

  # Get the players with the highest 3pt attempt rate (players who played more than 1 team in a season)

players_highest_x3pAR <- subset(stat_season, X3P. == high_x3ptAR)
print(players_highest_x3pAR)

#b. Players with the highest rate in a season

ftrhow_high <- max(stat_season$FT., na.rm = TRUE) 
print(ftrhow_high)

fthrow_sub <- subset(stat_season, FT. == ftrhow_high)
print(fthrow_sub)


#c. Season Lebron Scored the highest
  #Subset the LeBron James player
Lebron <- subset(stat_season, Player == 'LeBron James')
print(Lebron)
  #Get the max pts of LeBron
Lebron_max<- max(Lebron$PTS, na.rm = TRUE)
print(Lebron_max)
  # Match the nmax pts to the year he got the max pts
Lebron_pts <- subset(Lebron, PTS == Lebron_max)
print(Lebron_pts$Year)

#ANSWER: 2006

# d. Season Jordan scored the highest (Jordan with *)
  # Subset as Michael Jordan*
Jordan_M <- subset(stat_season, Player == 'Michael Jordan*')
print(Jordan_M)
  # Get the Max points/season
Jordan_max <- max(Jordan_M$PTS, na.rm = TRUE)
print(Jordan_max)
  # Subset points equal to max point to get the Year
Jordan_pts<- subset(Jordan_M, PTS == Jordan_max)
print(Jordan_pts)
print(Jordan_pts$Year)

#ANSWER: 1987

  #e. Bryan effiency rating in the year where his MP is lowest

KB <- subset(stat_season, Player =='Kobe Bryant')
print(KB)

print(subset(KB, MP == min(KB$MP))$PER)

#ANSWER: 10.7

####### National University Rankings
  #Name and read the file
ustat <- read.csv("National Universities Rankings.csv")
print(ustat)
#Check the column names
names(ustat)

# a. Universities with most number of undergrad.
#which.max can be used on this problem and eleminitaing non numeric characters

ustat$Undergrad.Enrollment <- gsub('\\$',"",ustat$Undergrad.Enrollment)
print(ustat$Undergrad.Enrollment)
ustat$Undergrad.Enrollment <- gsub('\\"',"",ustat$Undergrad.Enrollment)
print(ustat$Undergrad.Enrollment)
ustat$Undergrad.Enrollment <- gsub('\\,',"",ustat$Undergrad.Enrollment)
print(ustat$Undergrad.Enrollment)
ustat$Undergrad.Enrollment <- as.numeric(ustat$Undergrad.Enrollment)
print(ustat$Undergrad.Enrollment)
print(ustat[which.max(ustat$Undergradenrolment),] $Undergrad.Enrollment)
print(ustat[which.max(ustat$Undergrad.Enrollment),] $Name)

#ANSWER: University of Central Florida


# b. Average Tuition in the Top 10 University

  #Eliminate the non numeric first and Get the order of the universities by rank and average tuition fee

ustat$Tuition.and.fees <- gsub('\\$',"",ustat$Tuition.and.fees)
print(ustat$Tuition.and.fees)
ustat$Tuition.and.fees <- gsub('\\"',"",ustat$Tuition.and.fees)
print(ustat$Tuition.and.fees)
ustat$Tuition.and.fees <- gsub('\\,',"",ustat$Tuition.and.fees)
print(ustat$Tuition.and.fees)
ustat$Tuition.and.fees <- as.numeric(ustat$Tuition.and.fees)
print(ustat$Tuition.and.fees)
urank <- ustat[order(ustat$Rank,decreasing = FALSE),]
print(urank)
urank <-urank[1:10,]
print(mean(urank$Tuition.and.fees))
print(urank$Name)


#ANSWER: 49895.2

#END
