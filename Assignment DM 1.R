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

## From the boxplot, it shows that in x: Length of service group has 3 different class [0,13.5], (13.5,25.5] & (25.5,46.6]. While y; shows the range of salaries obtained from these class.

## In class [0,13.5], it shows that the boxplot has multiple outlier more lower, than higher values. Its boxplot is symetrically skewed. This is probably because the employees in this group are relatively new and havent worked a certain amount of years to achieve a higher paying salary.

## In class (13.5,25.5], it shows the lower bound value is around 1000, Q1 is within 8000,Q2(median) is 9000, Q3 is around 12000 and upper bound value is 16500. The boxplot of this class is skewed to the right and contains outlier on the right side. This shows that the minimum salary of this group of employees have salaries that are not much different from the 1st group.It also indicates that most of the employees in this group has a salary within the 8000 to 12000 range and working a certain amount does not guarentee a higher income. 

## In class (25.5,46.6], it shows the lower bound value is under 4000, Q1 is within 22000, Q2(median) is around 26000, Q3 is within 36000 and upper bound value is 53000. The boxplot for this class is right skewed and it does contain an outlier on the right side of the whisker. This boxplot's group has a diverse range of salary. From the lower bound, it shows that the salary is almost the same as the other groups. However, the box range from Q1 to Q3 is relatively higher than other groups and the amount is almost more than double for the 2nd group. This shows that the most of the employees in this group have a senior or upper management position.

## Comparing class classes together, it shows that class (13.5,26.6] is where the most value of salary is due to the box being bigger in size, while the class [0,13.5] has a little to none box. The class [0,13.5] has multiple outliers. These anomalies make the boxplot possibly have abnormal values. The group (25.5,46.6] have the most diverse range of salary. The range of all groups shows that length of service may not increase the amount of salary obtained.

