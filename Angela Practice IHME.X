# This script is intended to demo the map package & use real data for 
# obesity/diabetes


# **************************************************************************** #
# ***************                Time Stamp                    *************** #
# **************************************************************************** #
# Time/date stamps to help with labeling results
now=Sys.Date()
today=format(now, format="%d%b%y")


# Clear R memory 
rm(list=ls())

# Data Links 
#Data: http://www.healthmetricsandevaluation.org/publications/summaries/prevalence-physical-activity-and-obesity-us-counties-2001-2011-road-map-actio#/data-methods
#Interactive Map: http://viz.healthmetricsandevaluation.org/us-health-map/

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Set Working Directory
setwd("C:/Users/lemasd/Dropbox/Undergrad_Research/Angela/Project1")
#setwd("C:/Users/name/Dropbox/Undergrad_Research/Angela/Project1")

# What files are in this directory?
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #
# These are the libraries/packages that we need
# Need to figure out how to install these packages first!

#install.packages("devtools")
#install.packages("maps")
#install_github("choroplethr", "trulia")
install.packages("stringr")
library(devtools)
library(maps)
library(choroplethr)





# **************************************************************************** #
# ***************                Input Data                    *************** #
# **************************************************************************** #

# Import FIPS data
data(county.fips)
county.fips=as.data.frame(county.fips)
names(county.fips);head(county.fips);str(county.fips)

#Turn Fibs data into a file
write.table(county.fips, file="county.fips.csv", sep=",", row.names=F,na="NA")
# Example Data
data(choroplethr)



# **************************************************************************** #
# ***************                Run Code                      *************** #
# **************************************************************************** #


choroplethr(df_pop_county, "county", title="2012 County Population Estimates")
write.table(df_pop_county, file="county_map_data_27Mar14.csv", sep=",", row.names=F,na="NA")




#**************IHME Data**********************

# Import the data- IHME Obesity Data- Female only
IHME <- read.table(file="IHME_obesity_county_female.csv", sep = ",", header = T)
head(IHME)
dim(IHME)
IHME1 <- IHME[,1:6]
head(IHME1)
names(IHME1)

colnames(IHME1)[3]=(c("2001"))
colnames(IHME1)[4]=(c("2009"))
colnames(IHME1)[5]=(c("2011"))
colnames(IHME1)[6]=(c("Diff_09_11"))

#Make County names lowercase
IHME1$County <- tolower(IHME1$County)
IHME1$County <- tolower(IHME1$County)
head(IHME1)

# split up to first word in county name for IHME female data "IHME"
IHME1$newCounty <- lapply(strsplit(as.character(IHME1$County), " "), "[", 1)
head(IHME1)

#make these two newcounty's make them character strings
IHME1$State=as.character(IHME1$State)
IHME1$newCounty=as.character(IHME1$newCounty)
#Make State names lowercase
IHME1$State <- tolower(IHME1$State)
head(IHME1)
str(IHME1)

#**************Fips Conversion Data**********************


# Import the data sheet that has both county names and fip codes. Howver, county names say "county"

conv <- read.table(file="county.fips.csv", sep = ",", header = T)
str(conv)
which(conv$fips==6027) # 171 (Inyo County)
which(conv$fips==6071) # 193 (S.B. County)
head(conv)
conv1 <- conv[,]
head(conv1)
# rename columns
colnames(conv1)=c("fips","County")


# split up to first word in county name for conversion "conv"
# split county.name variable to create two new variables
conv1$State <- lapply(strsplit(as.character(conv1$County), ","), "[", 1)
conv1$newCounty <- lapply(strsplit(as.character(conv1$County), ","), "[", 2)
head(conv1)





#What kinds of things are the two newCountys
str(IHME1$newCounty)
str(conv1$newCounty)
#make these two newcounty's make them character strings
IHME1$newCounty=as.character(IHME1$newCounty)
conv1$newCounty=as.character(conv1$newCounty)

#What kinds of things are the two "State"s
str(IHME1)
str(conv1)
#make these two "State"s make them character strings
IHME1$State=as.character(IHME1$State)
conv1$State=as.character(conv1$State)
########################################
##############Troubleshooting###########
which(conv1$fips==6027) # 171 (Inyo County)
which(conv1$fips==6071) # 193 (S.B. County)

#*************merging the two*****************
# Check merge features
# States
length(unique(conv1$State)) # 49
length(unique(IHME1$State)) # 52
# Counties
length(unique(conv1$newCounty)) # 1788
length(unique(IHME1$newCounty)) # 1740
# What do pre-merge df's look like?
head(conv1)
head(IHME1)

#merge Convertion (Conv) and date (IHME1)
IHME.n<- merge(conv1, IHME1, by=c("State","newCounty"))
head(IHME.n)
str(IHME.n)

which(IHME1$newCounty=="san bernadino") # 171 (Inyo County)
which(conv1$fips==6071) # 193 (S.B. County)
which(conv1$fips==6027) # 171 (Inyo County)
which(conv1$fips==6071) # 193 (S.B. County)
which(IHME.n$fips==6027) # 171 (Inyo County)
which(IHME.n$fips==6071) # 193 (S.B. County)
##############Troubleshooting###########
########################################


IHME.n$fips=as.character(IHME.n$fips)
IHME.n$'2001'=as.numeric(IHME.n$'2001')
IHME.n$"2009"=as.numeric(IHME.n$"2009")
IHME.n$"2011"=as.numeric(IHME.n$"2011")

#Eliminate repeats in Fips

#Measure difference in lengths of total and unique lists of fips
length(IHME.n$fips)
length(unique(IHME.n$fips))

IHME.n$duplicated=duplicated(IHME.n$fips)
IHME.n <- subset(IHME.n, duplicated=="FALSE")
length(IHME.n$fips)



##*****************2001**********************#

#trim data frame to colums for Fip and Prevelance 2001
IHME.2001=IHME.n[,c(3,6)]
head(IHME.2001)
which(IHME.2001$region==6027) # 160 (Inyo county)
which(IHME.2001$region==6071) # not present (S.B county)


# rename columns
colnames(IHME.2001)=c("region","value")


#Map of IMHE 2001 Female Obesity Prevalence  data
choroplethr(IHME.2001, "county", title="Prevalnce of Female Obesity by County 2001")


##*****************2009**********************#

#trim data frame to colums for Fip and Prevelance 2001
IHME.2009=IHME.n[,c(3,7)]
head(IHME.2009)

# rename columns
colnames(IHME.2009)=c("region","value")

#Map of IMHE 2001 Female Obesity Prevalence  data
choroplethr(IHME.2009, "county", title="Prevalnce of Female Obesity by County 2009")

##*****************2011**********************#
#trim data frame to colums for Fip and Prevelance 2011
IHME.2011=IHME.n[,c(3,8)]
head(IHME.2011)

# rename columns
colnames(IHME.2011)=c("region","value")

#Map of IMHE 2001 Female Obesity Prevalence  data
choroplethr(IHME.2011, "county", title="Prevalnce of Female Obesity by County 2011")

#check Stucture
str(IHME.n)
IHME.n$newState=as.character(IHME.n$newState)

#turning IHME.n to file
write.table(IHME.n, file="IHME.n.csv",sep=",",row.names=F)
