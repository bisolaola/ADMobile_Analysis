##############################################################################################
##################install required packages##################################################
intall.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
install.packages("skimr")
install.packages("tidyr")
install.packages("RColorBrewer")
install.packages("gridExtra")

#############calling required libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(skimr) 
library(tidyr)
library(RColorBrewer)
library(gridExtra)

####set working directory 
setwd("/Users/bisolaolagoke/Desktop/Data Management/R Scripts")

#####import data sets for analyses
data1 <- read_excel("DDM22_56_customer.xlsx") ####7920 observations and 12 variables
data2 <- read_excel("DDM22_56_score.xlsx") ####2235 observations and 2 variables
str(data1)
str(data2)


##################################################################################################
#########################first data set(data1 only)################################################

####################################################################################################
#####data assessment for outliers and missing values

######################missing values
skim(data1) 
#########there are 42 values missing in variable citizenship
########there are 125 values missing in variable contract_terminated
#########there are 35 values missing in variable tenure

####################outliers 
table(data1$gender) ########there are 32 values called "other" in variable gender
table(data1$citizenship)########there are two outliers in variable citizenship named "-1" and "0"
table(data1$data_allowance)#####there is an outlier called "drunkard" in variable data_allowance


########################################################################################################
##################################cleaning data########################################################## 
##1. dropping outliers
#######a.drop the outlier "drunkard" in variable data_allowance 
data1 <- filter(data1, data_allowance == "1 GB" | data_allowance == "10 GB" | data_allowance == "5 GB" |
                              data_allowance == "no data"| data_allowance == "unlimited") 

#######b.drop the outlier "-1", "0" in variable citizenship 
data1 <- filter(data1, citizenship == "British"| citizenship == "Irish/ NI"|
                              citizenship == "other"| citizenship == "residence permit")

#######c.drop the value "other" in variable gender
data1 <- filter(data1, gender == "female"| gender == "male")

#################2. dropping missing values
#################removing missing values in variable contract_terminated, tenure and citizenship
data1 <- filter(data1, contract_terminated !="", tenure !="", citizenship !="")
#################data1 has been cut down to 7360 observations and 12 variables after dropping 
################outliers and missing values


####################################################################################################
##############re-coding data#######################################################################
############1. create a new variable that consist of tenure in years and rename the original tenure to indicate that it is in months 
data1 <- data1 %>% mutate(tenure_years = tenure/12) %>% rename(tenure_months = tenure)
data1$tenure_years <- round(data1$tenure_years, digits = 0) #########this is to round the new variable "tenure_years" in whole numbers
##########after changing tenure to years, an outlier was found detected. This reveal that there
##########customers with tenure 46years but in age, are younger than 46years.
head(arrange(data1, -tenure_years))
###########the outliers were therefore dropped. To drop them, this outliers were firstly turned into NAs before they were dropped
data1 <- data1 %>% mutate(tenure_years = na_if(tenure_years, 46)) %>% filter(tenure_years !="")

####################data1 is therefore down to 7312 observations and 13variables#####################


########2. re-code "no" in variable number_transferred to "not requested"
data1$number_transferred <- recode(data1$number_transferred, no = "no request")

########3. re-code "no" and "yes" in variable contract_terminated to "active" and "terminated" respectively
data1$contract_terminated <-  recode_factor(data1$contract_terminated, no = "Active", yes = "Terminated")

#######4. re-arranging the values in variable contract_type
data1$contract_type <- data1$contract_type %>% factor (levels = c("pay-as-you-go", "one year", "two years"))

#############################################################################################
#########################second data set(join data1 and data2 )###############################

data3 <-  inner_join(data1, data2, by = c("cid"))

############re-cording score 
##########1. re-code and rename variable satisfaction_score from numerical to categorical variable 
data3 <- data3 %>%
  mutate(satisfaction_score = ifelse(score <= 2, "Very Unsatisfied",
                              ifelse(score > 2 & score <= 4, "Unsatisfied",
                                     ifelse(score > 4 & score <= 6, "Neutral",
                                            ifelse(score > 6 & score <= 8, "Satisfied", "Very Satisfied")))))

#######2. re-arranging the values in variable satisfaction_score
data3$satisfaction_score <- data3$satisfaction_score %>% factor(levels = c("Very Unsatisfied", "Unsatisfied", "Neutral",
                                                                           "Satisfied", "Very Satisfied"))


#######3. re-arranging the values in variable contract_type
data3$contract_type <- data3$contract_type %>% factor (levels = c("pay-as-you-go", "one year", "two years"))


########data3 is with a total of 2064 observations and 15 variables########################

################################################################################################################
################################################################################################################
################################analysis and visualization#####################################################

#figure1 
figure1 <- ggplot(data = data1, mapping = aes(x = ..count../1, y= contract_terminated, fill = contract_terminated))+ 
  geom_bar() + labs( x= "Number of Customers", y = "Contract Terminated", 
                     title = "Contract Termination Status") + 
  scale_fill_manual(labels = c("Active", "Terminated"), values = c("#051436","#B0DAFF"))  + 
  theme(legend.title = element_blank(), legend.position = "none") 

#figure2
figure2 <- ggplot(data = data1, mapping = aes(x =contract_terminated, fill = number_transferred)) + 
  geom_bar(position = "fill") + labs( x= "Contract Terminated", y = "Proportion of Terminated Consumers(%)", 
                                      title = "Request for Termination versus Terminated Consumers", 
                                      label = "Number Transferred") + 
  scale_fill_manual(labels = c("no request", "requested"), values = c("#450404","#FDA9A9"), name = "Number Transferred") 

  
#figure3
figure3 <- ggplot(data = data3, mapping = aes(x = satisfaction_score, fill = contract_terminated)) + 
  geom_bar(position = "fill") + labs( x= "Satisfaction Score", y = "Proportion (%)", 
                                      title = "Does Customer Satisfaction Influence Retention?") + 
  scale_fill_manual(labels = c("Active", "Terminated"), values = c("#051436","#B0DAFF")) +
  theme(legend.title = element_blank(), legend.position = "bottom")


#figure4
figure4 <- ggplot(data = data1, mapping = aes(x = ..count../1, y = contract_type, fill = contract_terminated)) + 
  geom_bar(position = "fill") + labs( x= "Proportion (%)", y = "Contract Type", 
                                      title = "Proportion of Contract Terminated by Contract Type") + 
  scale_fill_manual(labels = c("Active", "Terminated"), values = c("#051436","#B0DAFF")) +
  theme(legend.title = element_blank(), legend.position = "bottom")

#figure4b
ggplot(data = data3, mapping = aes(x= satisfaction_score, fill = contract_terminated)) + 
  geom_bar(position = "fill") + facet_wrap(~ contract_type) + 
  labs( x= "Satisfaction Score", y = "Proportion (%)", title = "Contract Type Satisfaction Ratings and Retention") + 
  scale_fill_manual(labels = c("Active", "Terminated"), values = c("#051436","#B0DAFF")) +
  theme(legend.title = element_blank(), legend.position = "bottom")

#######dashboard
grid.arrange(figure1, figure2, figure3, figure4)

