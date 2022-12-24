# Importing the libraries

library(tidyverse)
library(ggplot2)
library(tidyr)
library(caret)
library(sf)
library(mapview)


Fire_Inspection<- read.csv("C:/Users/shikh/Downloads/fire_inspections.csv")

# Summary Statistics for each dataframe 

summary(Fire_Inspection)

# To view the dataframe
View(Fire_Inspection)

# To see null values in the dataframe 
lapply(Fire_Inspection,function(x) { length(which(is.na(x)))})
# There are no null values 

# To find the number of rows 
nrow(Fire_Inspection)

# To find the number of columns
ncol(Fire_Inspection)


colnames(Fire_Inspection)


colnames(Fire_Inspection)[1] <- "ApplicationNumber"


nearZeroVar(Fire_Inspection)

dim(Fire_Inspection)

#### Univariate Analysis

ggplot(Fire_Inspection,aes(x = fct_infreq(ApplicationType), fill=ApplicationType))+ 
  geom_bar(stat ='count')+
  ggtitle("Countplot for Application Type")+
  labs(x = "Application Type")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))

# As we can see from the plot above FPCP LIC application type has the highest number of count 
# It is the Fire Prevention Code Permit which allow the permit applicant to handle, store or use substances or devices regulated by the fire department.


ggplot(Fire_Inspection,aes(x = fct_infreq(ApplicationStatus), fill=ApplicationStatus))+ 
  geom_bar(stat ='count')+
  ggtitle("Countplot for Application Status")+
  labs(x = "Application Status")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))

# we Can see that majority of the application Status are closed.


# Address Key, Application Key, Inspection Key, Address Key are system generated key hence they are not important for our analysis


ggplot(Fire_Inspection,aes(y = fct_infreq(InspectionType), fill=InspectionType))+ 
  geom_bar(stat ='count')+
  ggtitle("Countplot for Inspection Type")+
  labs(x = "Inspection Type")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))


sort(table(Fire_Inspection$InspectionType))

# By looking at the output of this we can say that most of Applications are F POSTOCC and F 13 SYS
# F POSTOCC is the inspection after occupancy in the building. So most of the fire inspections are done after occupnacy in the building to see if they are or not maintaining all the fire codes.


# We are not doing the time series forecasting. Therefore, we are not using the CompletedDateTime Column

t<-as_factor(Fire_Inspection$StatusId)

options(scipen=999)

ggplot(Fire_Inspection,aes(y= fct_infreq(t), fill=t))+ 
  geom_bar(stat ='count')+
  ggtitle("Countplot for Status ID")+
  labs(y = "Status ID")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))


# Flag indicating the status of the inspection. 0 = None, 1 = Passed, 2 = Failed, 3 = Cancelled, 4 = Hold, 5 = Closed, 8 = Pending
# most of the applications are passed as they might have followed the rules and regulations.

table(Fire_Inspection$Addr_type)


ggplot(Fire_Inspection,aes(y= fct_infreq(Addr_type), fill=Addr_type))+ 
  geom_bar(stat ='count')+
  ggtitle("Countplot for Addr_type")+
  labs(y = "Addr_type")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))

# We can se that most of the addr_type is point address because it gives geographic point for an address and it is easy to locate an location

p<-ggplot(Fire_Inspection, aes(x= Miles_To_Nearest_Station,color=Miles_To_Nearest_Station)) +
  geom_boxplot()+
  ggtitle("Boxplot for Miles to Nearest Station")+
  theme(plot.title = element_text(hjust = 0.5))
p

# We can see there are some outliers which is very limited and very far from the nearest station



p<-ggplot(Fire_Inspection, aes(x= Miles_To_Nearest_Office,color=Miles_To_Nearest_Office)) +
  geom_boxplot()+
  ggtitle("Boxplot for Miles to Nearest Office")+
  theme(plot.title = element_text(hjust = 0.5))
p

# We can see there are some outliers which is very limited and very far from the nearest office


table(Fire_Inspection$Facility_Name)

ggplot(Fire_Inspection,aes(y= fct_infreq(Facility_Name), fill=Facility_Name))+ 
  geom_bar(stat ='count')+
  ggtitle("Countplot for Facility_Name")+
  labs(y = "Facility_Name")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(Fire_Inspection,aes(y= fct_infreq(Jurisdiction), fill=Jurisdiction))+ 
  geom_bar(stat ='count')+
  ggtitle("Countplot for Jurisdiction")+
  labs(y = "Jurisdiction")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))

# In this data most of the jurisdiction responsible for the inspection or permit processing is the Fairfax County

ggplot(Fire_Inspection,aes(y= fct_infreq(Office), fill=Office))+ 
  geom_bar(stat ='count')+
  ggtitle("Countplot for Office")+
  labs(y = "Jurisdiction")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))+ 
  theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))





# Bivariate Analysis


p<-ggplot(Fire_Inspection, aes(x=StatusName, y=Miles_To_Nearest_Office, color= StatusName)) +
  geom_boxplot()
p

# We can see that almost all Status Names have around same Miles_To_Nearest_Office


p<-ggplot(Fire_Inspection, aes(x=StatusName, y=Miles_To_Nearest_Station, color= StatusName)) +
  geom_boxplot()
p

# We can see that almost all Status Names have around same Miles_To_Nearest_Station

# But passed has 2 major outliers but they have already passed so not a big risk. and we can ignore these two variables




dat <- data.frame(table(Fire_Inspection$StatusName,Fire_Inspection$Jurisdiction))
names(dat) <- c("StatusName","Jurisdiction","Count")

ggplot(data=dat, aes(x=StatusName, y=Count, fill=Jurisdiction)) + geom_bar(stat="identity")

# By the plot above we can see that City of Fairfax and Fairfax county failed almost equal amount permits or applications




dat <- data.frame(table(Fire_Inspection$Jurisdiction,Fire_Inspection$Office))
names(dat) <- c("Jurisdiction","Office","Count")


cor(Fire_Inspection$Miles_To_Nearest_Station,Fire_Inspection$Miles_To_Nearest_Office)
# We can see that there is no High corr between them

cor(Fire_Inspection$Miles_To_Nearest_Station,Fire_Inspection$TypeNumber)
# We can see that there is no High corr between them

cor(Fire_Inspection$Miles_To_Nearest_Office,Fire_Inspection$TypeNumber)
# We can see that there is no High corr between them


locations_sf <- st_as_sf(Fire_Inspection, coords = c("X", "Y"), crs = 4326)


p=Fire_Inspection[Fire_Inspection$ApplicationStatus == "O", ]
locations_sf <- st_as_sf(p, coords = c("X", "Y"), crs = 4326)
mapview(locations_sf)

#  These are the locations of the property where the application status is open

q=Fire_Inspection[Fire_Inspection$StatusName == "Failed", ]
locations_sf <- st_as_sf(q, coords = c("X", "Y"), crs = 4326)
mapview(locations_sf)

# These are the locations where the inspection is failed

r=Fire_Inspection[Fire_Inspection$TypeNumber >5, ]
locations_sf <- st_as_sf(r, coords = c("X", "Y"), crs = 4326)
mapview(locations_sf)

# These are the locations where inspection more than 5 times happened

s=r[r$StatusName >"Failed", ]
locations_sf <- st_as_sf(s, coords = c("X", "Y"), crs = 4326)
mapview(locations_sf)

# These are the locations where inspection has happened more than 5 times and still the statusName is failed

# Fire Incidents

df_incidents <- read_csv("fire_incidents.csv")

#Zipcode wise count
df_incidents %>% group_by(ZipCode) %>% 
  summarise(count = n()) %>% arrange(desc(count)) %>% head(10) %>% 
  ggplot(aes(fct_inorder(ZipCode), count)) + geom_col() +
  labs(x = "Zipcode" , y = "Count", title = "Top 10 places with most fire incidents")

#Street type wise count
df_incidents %>% group_by(StreetType) %>% 
  summarise(count = n()) %>% arrange(desc(count)) %>% head(10) %>% 
  ggplot(aes(fct_inorder(StreetType), count)) + geom_col() +
  labs(x = "Street type" , y = "Count", title = "Top 10 Street types with most fire incidents")

clean_data_final <- read_csv("clean_data.csv")

clean_data_final %>% head() %>% view()

clean_data_final %>% ggplot(aes(Miles_To_Nearest_Office)) + geom_boxplot() +
  facet_wrap(~Office) +
  coord_flip() + 
  ggtitle("Box plot showing the distribution of Miles to Nearest Office based on the Office Location") +
  theme(plot.title = element_text(hjust = 0.5))




