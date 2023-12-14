# Data-mining assignment 1
HRdata<-read.csv("Set1_assignment1_HRdata",header= TRUE) # To call 1st dataset

#1.
require(tidyverse) #call package to use separate function

separate_data<- separate(HRdata,deptid_gender, into = c("deptid","gender"),sep = ";") #to separate deptid and gender into 2 columns


#2.
HRdata1<-unite(separate_data, col = 'birthdate', c('birthyear','birthmonth','birthday'), sep = '/') #to combine birthyear,birthmonth and birthday into new column called birthdate

##. Yes, there is 2 (288 & 3927) rows of missing values. 


#3.
require(lubridate) #call package to manipulate dates

FDnew<-format(as.Date(HRdata1$birthdate), "%Y/%m/%d") 

FDnew2= HRdata1%>%mutate(birthdate= as.Date(FDnew)) #to format birthdate into date format

misdata<-which(is.na(FDnew2$birthdate)) #find which column has NA [288, 3927]

## The data is missing because the birthday were on 29th but the birthyear wernt on a leap year.


#4.
NewHR<-FDnew2[complete.cases(FDnew2$birthdate),] #to only remove NA rows of data in birthdate

misdata<-which(is.na(NewHR$birthdate)) #to check back if NA is properly removed or not


#5.
str(NewHR) #to check format of other date columns

NewHR2<-format(as.Date(NewHR$enterdate,format = "%d/%m/%Y") ,) #to change enterdate column into date format
Endate= NewHR%>%mutate(enterdate= as.Date(NewHR2))

Exdate<-format(as.Date(Endate$exitdate,format = "%d/%m/%Y") ,) #to change exitdate column into date format
Exdate2<-as.Date(Exdate)
Exdate3= Endate%>%mutate(exitdate= as.Date(Exdate2))


#6.
Agedata<-Exdate3 %>% mutate(age=floor(as.numeric(difftime(as.Date("2022-10-31"), birthdate, unit = "days"))/ 365.25)) #to add new column for age using birthdate column and calculating from 31/10/2022 and divide by 365.25days


#7.
Actexit<-Agedata %>% mutate(Status = ifelse(is.na(exitdate) | exitdate >=as.Date("2022-10-31"), "active", "inactive")) #to add new column for active employees


#8.
Losdate<-Actexit %>% 
  mutate(
    Lengthofservice = ifelse(
      is.na(exitdate) | exitdate >= as.Date("2022-10-31"),
      floor(as.numeric(difftime(as.Date("2022-10-31"),enterdate, units = "days"))/365.25),
      floor(as.numeric(difftime(exitdate, enterdate, units = "days"))/ 365.25)
    )
  ) # to calculate the length of service as of 31 october 2022 and to add new column for length of service


#9.
Losdate$Losgroup<- cut(Losdate$Lengthofservice, breaks = c(-Inf, 13.5, 25.5, 46.6), labels = c("[0,13.5]", "(13.5,25.5]", "(25.5,46.6]"), include.lowest = TRUE, ordered_result = TRUE) #to add new column for losgroup


#10.
Saldata<-read.csv("Set1_assignment1_FINdata", header=TRUE) #to open and read new salary dataset

Latestsal <- Saldata %>%
  group_by(staffid) %>%
  arrange(staffid, desc(yearid)) %>%
  slice(1) %>%
  select(staffid, salary) #to take only latest salary by year

Newsal <- merge(Losdate, Latestsal, by = "staffid", all.x = TRUE) #to merge old data with new data


#11.
Subdata <- Newsal %>%
  filter(Status == "active") %>%
  select(staffid, deptid, Status, gender, namefirst, namelast, Losgroup, salary) #to take only staffid,deptid,status,gender,1st name and last name,losgroup and salary and put in new table column


#12.
par(mfrow = c(1, 2)) #to look at histogram side by side

hist(Subdata$salary, breaks = seq(0, 70000, by = 10000), main = "Relative Frequency Histogram (Breaks: 10000)", xlab = "Salary", ylab = "Relative Frequency", col = "darkred", probability = TRUE) #histogram break:10000 for Relative Frequency and Salary

hist(Subdata$salary, breaks = seq(0, 70000, by = 5000), main = "Relative Frequency Histogram (Breaks: 5000)", xlab = "Salary", ylab = "Relative Frequency", col = "darkgreen", probability = TRUE) #histogram break:5000 for Relative Frequency and Salary

## Comparing both histograms of 5000 and 10000, both of the histogram have good features but the histogram of break: 5000 is better because it shows the sparse and concentrated data and also highlights the outliers, while the histogram of break 10000 covers alot of the needed value however it has smoother overview of distribution and smoother look of histogram.


#13.
boxplot(Subdata$salary ~ Subdata$Losgroup, main = "Boxplots of Salary by Length of Service", xlab = "Length of Service Group", ylab = "Salary", col = "yellow") #to boxplot salary and length of service
