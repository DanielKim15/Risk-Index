#######Data Modification (08/18/2022)
install.packages("xlsx")
install.packages("pscl")
packageVersion("dplyr")
install.packages("caret")
library(tidyverse)
library(janitor) #lets you clear all spaces in column headers, cons: chisq.test and fisher.test will be masked
library(foreign)
library(nnet)
library(reshape2)
library(car) #has all of the diagnositc test for linear regressions
library(MASS) 
library(leaps) #contains regsubest and help find AIC/BIC to determine best variable
library(bestNormalize)
library(GGally)
detach(package:GGally,unload=TRUE)
library(lmtest) #used to find robust standard errors
library(sandwich) #used to find robust standard errors for the consultant funding test
options(scipen = 999)
library(forcats)
library(fastDummies) #helps you create dummy variables in factors
library(nlme) #contains the gls function (helps with regressions with unequal variance), masks dplyr collapse function
library(robustbase) #has lmrob, robust regression
library(ggstatsplot)
library(lubridate) #lets you control date/time (don't use)
library(anytime) #lets you control date/time 
library(readr) #writes dataframe into csv file faster
library(xlsx) #writes dataframe into excel file
library(scales)
library(pscl)
library(caret)
data("ToothGrowth")

###Part 1: Load dataset
Data <- read_csv("C:/Users/dkim/Downloads/Form_471.csv")
str(Data)
length(unique(Data$billed_entity_number_ben))
dim(Data) #101737 rows, 82 columns origianlly

###Part 2: Cleaning Data
Data$`Invoicing Method` #note to self: use that one technique from pmc dataset to clear up the spaces in the column names

Data <- clean_names(Data)
Data$filing_window

#changing all characters to factors
Data[sapply(Data, is.character)] <- lapply(Data[sapply(Data, is.character)], as.factor)
str(Data)

#changing some num to factors
Data$billed_entity_number_ben <- as.factor(Data$billed_entity_number_ben)

#Replacing 0 with NA on FCC Registration Number Column
Data$fcc_registration_number <- replace(Data$fcc_registration_number, Data$fcc_registration_number == 0, NA)
sum(is.na(Data$fcc_registration_number))
sum(Data$filing_window == "ECF Window 3")

#Creating a column that says if a consultant is there or not
Data <- mutate(Data, Consultant_Yes_No = ifelse(consulting_firm == "{",0,1))
Data$Consultant_Yes_No[is.na(Data$Consultant_Yes_No)] = 0
Data$Consultant_Yes_No <- as.factor(Data$Consultant_Yes_No)
str(Data$Consultant_Yes_No)
Data$Consultant_Yes_No
sum(Data$Consultant_Yes_No == 1) #its correct
str(Data$Consultant_Yes_No)
str(Data)

#Creating a column that states if there are two or more consultants involved or not
Data$consulting_firm <- as.character(Data$consulting_firm)
z <- nchar(Data$consulting_firm)
sum(ifelse(z>=57,1,0) == 1, na.rm = TRUE) #I lose 200 applications, (8327 out of 8533 is found here)

z <- nchar(Data$consulting_firm)
Data$two_or_more_consultant <- ifelse(z>=57,1,0)
str(Data$two_or_more_consultant)
Data$two_or_more_consultant <- as.factor(Data$two_or_more_consultant)
dim(Data)

Data$two_or_more_consultant[is.na(Data$two_or_more_consultant)] <- 0
Data$two_or_more_consultant

#Creating columns to show if the application appeared in this window or not
#Data$funding_request_total_student_per_capita <- Data$total_funding_commitment_request_amount/Data$total_student_count
#Data$funding_request_total_student_per_capita
#Data$funding_request_total_student_per_capita[is.nan(Data$funding_request_total_student_per_capita)] <- NA 
#Data$funding_request_total_student_per_capita[is.infinite(Data$funding_request_total_student_per_capita)] <- NA

Data$Window1_Yes_No <- ifelse(Data$filing_window == "ECF Window 1", 1,0)
sum(Data$Window1_Yes_No == 1)

Data$Window2_Yes_No <- ifelse(Data$filing_window == "ECF Window 2", 1,0)
sum(Data$Window2_Yes_No == 1) #problem: it only counts individual applications (frn line item level) as ecf, so instead of seeing BEN in ecf window 1 and 2, we just see individual applciations saying 1.

Data$Window3_Yes_No <- ifelse(Data$filing_window == "ECF Window 3", 1,0)
sum(Data$Window3_Yes_No == 1)
############################
multiple_window <- Data

#potential solution: develop a for loop and do the if else statement

ben_experiment <- filter(Data, billed_entity_number_ben == 359)
dim(ben_experiment)
ben_experiment$duplicate <- ifelse(sum(ben_experiment$Window1_Yes_No == 1) > 1 & sum(ben_experiment$Window3_Yes_No == 1) > 1 & duplicated(ben_experiment$billed_entity_number_ben),1,0)
view(ben_experiment[,c(85,86,87,120)])
names(ben_experiment)

ifelse(sum(ben_experiment$Window1_Yes_No == 1) > 1,1,0)

for(i in 1:length(ben_experiment)){
  if ben_experiment$billed_entity_number_ben
}



sum(ifelse(multiple_window$Window1_Yes_No == 1 & multiple_window$Window2_Yes_No == 1 & duplicated(multiple_window$billed_entity_number_ben), 1,0) == 1)
ifelse(multiple_window$Window1_Yes_No == 1, "Hi",0)


sum(ifelse(sum(multiple_window$Window1_Yes_No == 1) > 1 & sum(multiple_window$Window2_Yes_No == 1) > 1  & duplicated(multiple_window$billed_entity_number_ben), 1,0) == 1)

sum(ifelse(sum(multiple_window$Window1_Yes_No == 1 & multiple_window$Window2_Yes_No == 1) > 1  & duplicated(multiple_window$billed_entity_number_ben), 1,0) == 1)

ifelse(multiple_window$filing_window %in% "ECF Window 1" & multiple_window %in% "ECF Window 2" & multiple_window$billed_entity_number_ben, 1,0)

names(multiple_window)
view(multiple_window[,c(13,85,86,87,120)])

multiple_window$which_window_duplicate <- ifelse(sum(multiple_window$Window1_Yes_No == 1) > 1 & sum(multiple_window$Window2_Yes_No == 1) > 1  & duplicated(multiple_window$billed_entity_number_ben), 1,0)
############################
view(multiple_window[,120])
sum(ifelse(multiple_window$Window1_Yes_No %in% 1 & multiple_window$Window2_Yes_No %in% 1, "Window1_Window2",0) == "Window1_Window2",na.rm = TRUE)
sum(ifelse(multiple_window$Window1_Yes_No == 1 & multiple_window$Window3_Yes_No == 1, "Window1_Window3",0) == "Window1_Window3", na.rm = TRUE)

sum(ifelse(multiple_window$Window1_Yes_No == 1 & multiple_window$Window2_Yes_No == 1, "HELLO",0) == "HELLO", na.rm = TRUE)

#Creating Public/Private column
Data$Public_or_Private <- Data$applicant_subtype #Best one
Data$Public_or_Private

Data$Public_or_Private <- fct_collapse(Data$Public_or_Private, Public = c("Adult Education,Juvenile Justice,Public School","Head Start","Charter School","Charter School,Pre-K","Head Start,Pre-K,Public School","Head Start,Public School","Adult Education,Pre-K,Public School","Adult Education,Public School","Adult Education,Head Start,Pre-K,Public School","Adult Education,ESA School,Head Start,Pre-K,Public School","ESA School,ESA School District with no Schools,Public School","Dormitory,Public School","ESA School,Public School","Juvenile Justice,Public School","Public School","Pre-K","Pre-K,Public School", "New Construction School,Public School","Dormitory,Public School,Tribal School","Public School,Tribal School","Adult Education,Dormitory,Pre-K,Public School,Tribal School","BIE,Dormitory,Public School,Tribal School","BIE,Public School,Tribal School","Charter School,Pre-K,Public School,Tribal School","Charter School,Public School,Tribal School","Adult Education,Charter School,Pre-K,Public School","Adult Education,Charter School,Public School","BIE,Charter School,Public School","Charter School,Dormitory,Public School","Charter School,ESA School,Public School","Charter School,Head Start,Pre-K,Public School","Charter School,New Construction School,Public School","Charter School,Pre-K,Public School","Charter School,Public School", "Main Branch,Public Library,Tribal Library","Main Branch,Tribal Library","New Construction Library,Public Library","Public Library,Tribal Library","Public Library,State Library Agency - Library","Public Library,Research,Tribal Library","Public Library,Research","Public Library System","Public Library","Main Branch,Public Library,Research","Main Branch,Public Library","Main Branch,New Construction Library,Public Library","Bookmobile,Public Library","Bookmobile,Main Branch,Public Library","Academic,Public Library,Research","Academic,Main Branch,Public Library,Research","Academic,Bookmobile,Main Branch,New Construction Library,Public Library,Research,State Library Agency - Library","Academic,Bookmobile,Kiosk,Main Branch,New Construction Library,Public Library,Research","ESA School District,Public School District","Public School District","ESA School District,Public School District","ESA School District with no Schools,Public School","Charter School District,ESA School District,Public School District","Charter School District,Public School District","ESA Consortium,State Education Agency,State-wide","Non-Profit Purchasing Group,State-wide","Other State Agency,State-wide","State-wide","State Education Agency,State-wide","State Library Agency - Consortium,State-wide"),
                                          Private = c( "Charter School,Pre-K,Private School","Private School,Tribal School","Dormitory,Private School,Tribal School","Head Start,Pre-K,Private School","Head Start,Private School","Adult Education,Pre-K,Private School","Adult Education,Private School","Adult Education,Juvenile Justice,Private School","ESA School District with no Schools,Private School","Dormitory,Juvenile Justice,Private School","Dormitory,Pre-K,Private School","Dormitory,Private School", "General-Use School,Pre-K,Private School","General-Use School,Private School","Juvenile Justice,Private School", "New Construction School,Private School" ,"Private School","Pre-K,Private School","Charter School,Private School,Tribal School","Charter School,Private School","Academic,Private Library","Private Library,Research,Tribal Library","Private Library,Research", "Private Library System","Private Library", "Academic,Private Library,Research", "Main Branch,Private Library","Academic,Main Branch,Private Library,Research", "Private School District" ),
                                          Other = c("ESA Consortium"))

Data$Public_or_Private <- replace(Data$Public_or_Private, Data$Public_or_Private == "Other",NA)
Data$Public_or_Private <- droplevels(Data$Public_or_Private)

str(Data$Public_or_Private)
levels(Data$Public_or_Private)
sum(Data$Public_or_Private == "Public", na.rm = TRUE)
sum(Data$Public_or_Private == "Private", na.rm = TRUE)
sum(is.na(Data$Public_or_Private))

#Creating the Tribal Column
Data$Tribal_Yes_No <- as.integer(Data$applicant_subtype %in% c("Dormitory,Public School,Tribal School","Public School,Tribal School","Adult Education,Dormitory,Pre-K,Public School,Tribal School","BIE,Dormitory,Public School,Tribal School","BIE,Public School,Tribal School","Charter School,Pre-K,Public School,Tribal School","Charter School,Public School,Tribal School", "Main Branch,Public Library,Tribal Library","Main Branch,Tribal Library","New Construction Library,Public Library","Public Library,Tribal Library","Public Library,State Library Agency - Library","Public Library,Research,Tribal Library","Public Library,Research","Private School,Tribal School","Dormitory,Private School,Tribal School","Charter School,Private School,Tribal School","Private Library,Research,Tribal Library","Private Library,Research"))
Data$Tribal_Yes_No <- as.factor(Data$Tribal_Yes_No)
sum(Data$Tribal_Yes_No == 1)

sum(Data$Tribal_Yes_No == 1)
str(Data$Tribal_Yes_No)

#Creating Charter Column
Data$Charter_Yes_No <- as.integer(Data$applicant_subtype %in% c("Adult Education,Charter School,Pre-K,Public School","Adult Education,Charter School,Public School","BIE,Charter School,Public School","Charter School","Charter School,Dormitory,Public School","Charter School,ESA School,Public School","Charter School,Head Start,Pre-K,Public School","Charter School,New Construction School,Public School","Charter School,Pre-K","Charter School,Pre-K,Private School","Charter School,Pre-K,Public School","Charter School,Private School","Charter School,Public School","Charter School District,ESA School District,Public School District","Charter School District,Public School District","Charter School,Pre-K,Public School,Tribal School","Charter School,Private School,Tribal School","Charter School,Public School,Tribal School"))
Data$Charter_Yes_No <- as.factor(Data$Charter_Yes_No)
sum(Data$Charter_Yes_No == 1)
str(Data$Charter_Yes_No)

#Creating Pre-K column
levels(Data$applicant_subtype)
Data$PreK_Yes_No <- as.integer(Data$applicant_subtype %in% c("Adult Education,Charter School,Pre-K,Public School", "Adult Education,Dormitory,Pre-K,Public School,Tribal School", "Adult Education,ESA School,Head Start,Pre-K,Public School",  "Adult Education,Head Start,Pre-K,Public School","Adult Education,Pre-K,Private School","Adult Education,Pre-K,Public School","Charter School,Head Start,Pre-K,Public School","Charter School,Pre-K","Charter School,Pre-K,Private School","Charter School,Pre-K,Public School","Charter School,Pre-K,Public School,Tribal School","Dormitory,Pre-K,Private School","General-Use School,Pre-K,Private School","Head Start,Pre-K,Private School","Head Start,Pre-K,Public School","Pre-K","Pre-K,Private School" ,"Pre-K,Public School"))
Data$PreK_Yes_No <- as.factor(Data$PreK_Yes_No)
sum(Data$PreK_Yes_No == 1)
str(Data$PreK_Yes_No)

#Creating Head Start
levels(Data$applicant_subtype)
Data$HeadStart_Yes_No <- as.integer(Data$applicant_subtype %in% c("Adult Education,ESA School,Head Start,Pre-K,Public School","Adult Education,Head Start,Pre-K,Public School","Charter School,Head Start,Pre-K,Public School","Head Start", "Head Start,Pre-K,Private School", "Head Start,Pre-K,Public School", "Head Start,Private School", "Head Start,Public School"))
Data$HeadStart_Yes_No <- as.factor(Data$HeadStart_Yes_No)
sum(Data$HeadStart_Yes_No == 1)
str(Data$HeadStart_Yes_No)

#Creating ESA filter column
levels(Data$applicant_subtype)
Data$ESA_Yes_No <- as.integer(Data$applicant_subtype %in% c("Adult Education,ESA School,Head Start,Pre-K,Public School","Charter School District,ESA School District,Public School District","Charter School,ESA School,Public School","ESA Consortium","ESA Consortium,State Education Agency,State-wide","ESA School District with no Schools,Private School","ESA School District with no Schools,Public School","ESA School District,Public School District","ESA School,ESA School District with no Schools,Public School","ESA School,Public School"))
Data$ESA_Yes_No <- as.factor(Data$ESA_Yes_No)
sum(Data$ESA_Yes_No == 1)
str(Data$ESA_Yes_No)


#creating new column that shows different levels of applicant subtype (ex. large school is 1000 or more, small is 0-250)
Data <- mutate(Data, All_School_Size = case_when(applicant_type == "School" & total_student_count > 0 & total_student_count <= 133 ~ "Small_All_School",
                                                          applicant_type == "School" & total_student_count >= 134 & total_student_count <= 441 ~ "Medium_All_School",
                                                          applicant_type == "School" & total_student_count >= 442 & total_student_count <= 728 ~ "Large_All_School",
                                                          applicant_type == "School" & total_student_count > 729 ~ "Outliers_All_School"))

Data <- mutate(Data, Public_School_Size = case_when(applicant_type == "School" & Public_or_Private == "Public" & total_student_count > 0 & total_student_count <= 179 ~ "Small_Public_School",
                                                          applicant_type == "School" & Public_or_Private == "Public" & total_student_count >= 180 & total_student_count <= 550 ~ "Medium_Public_School",
                                                          applicant_type == "School" & Public_or_Private == "Public" & total_student_count >= 551 & total_student_count <= 903 ~ "Large_Public_School",
                                                          applicant_type == "School" & Public_or_Private == "Public" & total_student_count > 904 ~ "Outliers_Public_School"))

Data <- mutate(Data, Private_School_Size = case_when(applicant_type == "School" & Public_or_Private == "Private" & total_student_count > 0 & total_student_count <= 115 ~ "Small_Private_School",
                                                           applicant_type == "School" & Public_or_Private == "Private" & total_student_count >= 116 & total_student_count <= 363 ~ "Medium_Private_School",
                                                           applicant_type == "School" & Public_or_Private == "Private" & total_student_count >= 364 & total_student_count <= 599 ~ "Large_Private_School",
                                                           applicant_type == "School" & Public_or_Private == "Private" & total_student_count > 600 ~ "Outliers_Private_School")) 

Data <- mutate(Data, Charter_School_Size = case_when(applicant_type == "School"  & Charter_Yes_No == 1 & total_student_count > 0 & total_student_count <= 207 ~ "Small_Charter_School",
                                                           applicant_type == "School"  & Charter_Yes_No == 1 & total_student_count >= 208 & total_student_count <= 575 ~ "Medium_Charter_School",
                                                           applicant_type == "School"  & Charter_Yes_No == 1 & total_student_count >= 576 & total_student_count <= 916 ~ "Large_Charter_School",
                                                           applicant_type == "School"  & Charter_Yes_No == 1 & total_student_count > 917 ~ "Outliers_Charter_School")) 

Data <- mutate(Data, Tribal_School_Size = case_when(applicant_type == "School"  & Tribal_Yes_No == 1 & total_student_count > 0 & total_student_count <= 135 ~ "Small_Tribal_School",
                                                          applicant_type == "School"  & Tribal_Yes_No == 1 & total_student_count >= 136 & total_student_count <= 260 ~ "Medium_Tribal_School",
                                                          applicant_type == "School"  & Tribal_Yes_No == 1 & total_student_count >= 261 & total_student_count <= 439 ~ "Large_Tribal_School",
                                                          applicant_type == "School"  & Tribal_Yes_No == 1 & total_student_count > 440 ~ "Outlier_Tribal_School")) 


Data <- mutate(Data, PreK_School_Size = case_when(applicant_type == "School"  & PreK_Yes_No == 1 & total_student_count > 0 & total_student_count <= 137 ~ "Small_PreK_School",
                                                          applicant_type == "School"  & PreK_Yes_No == 1 & total_student_count >= 138 & total_student_count <=374 ~ "Medium_PreK_School",
                                                          applicant_type == "School"  & PreK_Yes_No == 1 & total_student_count >= 375 & total_student_count <=580 ~ "Large_PreK_School",
                                                          applicant_type == "School"  & PreK_Yes_No == 1 & total_student_count > 581 ~ "Outlier_PreK_School")) 


Data <- mutate(Data, Headstart_School_Size = case_when(applicant_type == "School"  & HeadStart_Yes_No == 1 & total_student_count > 0 & total_student_count <= 93 ~ "Small_Headstart_School",
                                                            applicant_type == "School" & HeadStart_Yes_No == 1 & total_student_count >= 94 & total_student_count <= 412 ~ "Medium_Headstart_School",
                                                            applicant_type == "School" & HeadStart_Yes_No == 1 & total_student_count >= 413 & total_student_count <= 1266 ~ "Large_Headstart_School",
                                                            applicant_type == "School" &  HeadStart_Yes_No == 1 & total_student_count > 1267 ~ "Outliers_Headstart_School")) 

Data <- mutate(Data, All_District_Size = case_when(applicant_type == "School District" & total_student_count > 0 & total_student_count <=576 ~ "Small_All_District",
                                                            applicant_type == "School District" & total_student_count >= 577 & total_student_count <=3642 ~ "Medium_All_District",
                                                            applicant_type == "School District" & total_student_count >= 3643 & total_student_count <=9469 ~ "Large_All_District",
                                                            applicant_type == "School District" & total_student_count > 9470 ~ "Outliers_All_District")) 

Data <- mutate(Data, Public_District_Size = case_when(applicant_type == "School District" & Public_or_Private == "Public" & total_student_count > 0 & total_student_count <=601 ~ "Small_Public_District",
                                                            applicant_type == "School District" & Public_or_Private == "Public" & total_student_count >= 602 & total_student_count <=3756 ~ "Medium_Public_District",
                                                            applicant_type == "School District" & Public_or_Private == "Public" & total_student_count >= 3757 & total_student_count <=9674 ~ "Large_Public_District",
                                                            applicant_type == "School District" & Public_or_Private == "Public" & total_student_count > 9675 ~ "Outliers_Public_District")) 

Data <- mutate(Data, Private_District_Size = case_when(applicant_type == "School District" & Public_or_Private == "Private" & total_student_count > 0 & total_student_count <=220 ~ "Small_Private_District",
                                                             applicant_type == "School District" & Public_or_Private == "Private" & total_student_count >= 221 & total_student_count <=934 ~ "Medium_Private_District",
                                                             applicant_type == "School District" & Public_or_Private == "Private" & total_student_count >= 935 & total_student_count <=1705 ~ "Large_Private_District",
                                                             applicant_type == "School District" & Public_or_Private == "Private" & total_student_count > 1706 ~ "Outliers_Private_District")) 

Data <- mutate(Data, Charter_District_Size = case_when(applicant_type == "School District" & Charter_Yes_No == 1 & total_student_count > 0 & total_student_count <=615 ~ "Small_Charter_District",
                                                             applicant_type == "School District" & Charter_Yes_No == 1 & total_student_count >= 616 & total_student_count <=3153 ~ "Medium_Charter_District",
                                                             applicant_type == "School District" & Charter_Yes_No == 1 & total_student_count >= 3154 & total_student_count <=10738 ~ "Large_Charter_District",
                                                             applicant_type == "School District" & Charter_Yes_No == 1 & total_student_count > 10739 ~ "Outliers_Charter_District")) 

Data <- mutate(Data, ESA_District_Size = case_when(applicant_type == "School District" & ESA_Yes_No == 1 & total_student_count > 0 & total_student_count <=179 ~ "Small_ESA_District",
                                                             applicant_type == "School District" & ESA_Yes_No == 1 & total_student_count >= 180 & total_student_count <=1023 ~ "Medium_ESA_District",
                                                             applicant_type == "School District" & ESA_Yes_No == 1 & total_student_count >= 1024 & total_student_count <=2569 ~ "Large_ESA_District",
                                                             applicant_type == "School District" & ESA_Yes_No == 1 & total_student_count > 2570 ~ "Outliers_ESA_District")) 

Data <- mutate(Data, Consortium_Size = case_when(applicant_type == "Consortium" & total_student_count > 0 & total_student_count <=231 ~ "Small_Consortium",
                                                       applicant_type == "Consortium" & total_student_count >= 232 & total_student_count <=3098 ~ "Medium_Consortium",
                                                       applicant_type == "Consortium" & total_student_count >= 3099 & total_student_count <=24869 ~ "Large_Consortium",
                                                       applicant_type == "Consortium" & total_student_count > 24870 ~ "Outliers_Consortium")) 

Data <- mutate(Data, Library_Type = case_when(applicant_type == "Library" & Public_or_Private == "Private" ~ "Private_Library",
                                                    applicant_type == "Library" & Public_or_Private == "Public" ~ "Public_Library",
                                                    applicant_type == "Library System" & Public_or_Private == "Private" ~ "Private_Library_System",
                                                    applicant_type == "Library System" & Public_or_Private == "Public" ~ "Public_Library_System"))


#replacing all na with 0
Data$All_School_Size <- replace(Data$All_School_Size, is.na(Data$All_School_Size), 0)
Data$Public_School_Size <- replace(Data$Public_School_Size, is.na(Data$Public_School_Size), 0)
Data$Private_School_Size <- replace(Data$Private_School_Size, is.na(Data$Private_School_Size), 0)
Data$Charter_School_Size <- replace(Data$Charter_School_Size, is.na(Data$Charter_School_Size), 0) 
Data$Tribal_School_Size <- replace(Data$Tribal_School_Size, is.na(Data$Tribal_School_Size), 0) 
Data$All_District_Size <- replace(Data$All_District_Size, is.na(Data$All_District_Size), 0) 
Data$Public_District_Size <- replace(Data$Public_District_Size, is.na(Data$Public_District_Size), 0) 
Data$Charter_District_Size <- replace(Data$Charter_District_Size, is.na(Data$Charter_District_Size), 0) 
Data$Consortium_Size <- replace(Data$Consortium_Size, is.na(Data$Consortium_Size), 0)
Data$Private_District_Size <- replace(Data$Private_District_Size, is.na(Data$Private_District_Size),0)
Data$Library_Type <- replace(Data$Library_Type, is.na(Data$Library_Type),0)
Data$PreK_School_Size <- replace(Data$PreK_School_Size, is.na(Data$PreK_School_Size),0)
Data$Headstart_School_Size <- replace(Data$Headstart_School_Size, is.na(Data$Headstart_School_Size),0)

#Changing reference point
Data[sapply(Data, is.character)] <- lapply(Data[sapply(Data, is.character)], as.factor) #changing the reference level requires that its all factors 
str(Data)
Data$All_School_Size <- relevel(Data$All_School_Size, ref = 4)
Data$Public_School_Size <- relevel(Data$Public_School_Size, ref = 4) 
Data$Private_School_Size <- relevel(Data$Private_School_Size, ref = 4) 
Data$Charter_School_Size <- relevel(Data$Charter_School_Size, ref = 4) 
Data$Tribal_School_Size <- relevel(Data$Tribal_School_Size, ref = 3)
Data$All_District_Size <- relevel(Data$All_District_Size, ref = 4)
Data$Public_District_Size <- relevel(Data$Public_District_Size, ref = 4) 
Data$Charter_District_Size <- relevel(Data$Charter_District_Size, ref = 4) 
Data$Consortium_Size <- relevel(Data$Consortium_Size, ref = 4) 
Data$Private_District_Size <- relevel(Data$Private_District_Size, ref = 4)
str(Data$Library_Type)

#Creating a column that shows regions in USA
levels(Data$billed_entity_state)
Data$regions <- fct_collapse(Data$billed_entity_state, Southeast = c("GA","FL","SC","NC","VA","WV","KY","TN","AL","MS","AR","LA"), 
                                Southwest = c("AZ","NM","TX","OK"), 
                                Northeast = c("DE","NJ","MD","PA","NY","CT","MA","VT","NH","RI","ME", "DC"),
                                Midwest = c("ND","SD","NE","KS","MN","WI","IA","MO","IL","IN","MI","OH"),
                                West = c("CA","NV","OR","WA","MT","ID","WY","UT","CO"),
                                Other = c("HI","GU","AK","PR","AS","MP","VI"))
levels(Data$regions)
sum(is.na(Data$regions))

#Creating a column that shows what month this thing was accepted for date
#Creating two columns from certified_date_time into time of day and month
Data$certified_date_time <- as.character(Data$certified_date_time)
Data$certified_date_time <- mdy_hms(Data$certified_date_time) #THIS WORKED
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
labels <- c("Night", "Morning", "Afternoon", "Evening")

Data$Time_of_day_certified <- cut(x=hour(Data$certified_date_time), breaks = breaks, labels = labels, include.lowest=TRUE)
Data$Time_of_day_certified #creates column that has time of day
View(Data) #Success
str(Data$certified_date_time)
table(Data$Time_of_day_certified)/sum(table(Data$Time_of_day_certified))

Data <- transform(Data,certified_month = format(certified_date_time, "%m")) #creates column of months
Data$certified_month <- as.factor(Data$certified_month)
str(Data$certified_month)
levels(Data$certified_month) #the months are created
levels(Data$certified_month) <- c("January","February","March","April","May","June","July","August","September","October","November","December")
(table(Data$certified_month)/sum(table(Data$certified_month)))*100
#create two column: one for months (january, february, etc), the other for day time (morning, afternoon, evening, night). Possibly third column? The week its at


#Adding the column where it shows the duplicates of windows across
Data$row_id <- 1:nrow(Data)
Data$row_id
window_duplicates <- read_csv("C:/Users/dkim/Downloads/Window_Duplicates.csv")
window_duplicates <- clean_names(window_duplicates)
colnames(window_duplicates)[colnames(window_duplicates) == "ben"] <- "billed_entity_number_ben"
window_duplicates_2 <- window_duplicates %>% group_by(billed_entity_number_ben)
window_duplicates_2$keep_or_delete <- ifelse(duplicated(window_duplicates_2$billed_entity_number_ben),0,1)
window_duplicates_2 <- window_duplicates_2[window_duplicates_2$keep_or_delete == 1,]
view(window_duplicates_2)
dim(window_duplicates_2)

window_duplicates_2[,2] <- lapply(window_duplicates_2[,2], as.integer) #7/25/2022 so far so good, now add it to the main Data
window_duplicates_2$billed_entity_number_ben <- as.factor(window_duplicates_2$billed_entity_number_ben)
Data$billed_entity_number_ben <- as.factor(Data$billed_entity_number_ben)

Data <- left_join(Data,window_duplicates_2, by = "billed_entity_number_ben")
dim(Data)

Data <- Data[!duplicated(Data$row_id),]
Data$duplicate_across_window

#Creating unmet need ratio column (with the help from USAC) (07/20/2022)
Data <- mutate(Data, unmet_need_device = unmet_student_needs_7_with_ecf_funding_will_provide_access_to_device + unmet_student_needs_9_with_ecf_funding_will_provide_access_to_device_and_connection, unmet_need_service = unmet_student_needs_8_with_ecf_funding_will_provide_access_to_connection + unmet_student_needs_9_with_ecf_funding_will_provide_access_to_device_and_connection)
Data <- mutate(Data, unmet_need_device_student_ratio = unmet_need_device/total_student_count, unmet_need_service_student_ratio = unmet_need_service/total_student_count)
Data[Data == Inf] <- NA
Data[Data == NaN] <- NA

#or 
Data$unmet_need_device_student_ratio[is.nan(Data$unmet_need_device_student_ratio)] <- NA 
Data$unmet_need_device_student_ratio[is.infinite(Data$unmet_need_device_student_ratio)] <- NA 

Data$unmet_need_service_student_ratio[is.nan(Data$unmet_need_service_student_ratio)] <- NA 
Data$unmet_need_service_student_ratio[is.infinite(Data$unmet_need_service_student_ratio)] <- NA 

###Creating column to find total funding cost per student
#Data$funding_request_total_student_per_capita <- Data$total_funding_commitment_request_amount/Data$total_student_count
#Data$funding_request_total_student_per_capita

#Data$funding_request_total_student_per_capita[is.nan(Data$funding_request_total_student_per_capita)] <- NA 
#Data$funding_request_total_student_per_capita[is.infinite(Data$funding_request_total_student_per_capita)] <- NA


####Step 2: Connecting external data sources 
####Mixing with Flagged Application
###Step 1: load the data
flagged_data <- read_csv("C:/Users/dkim/Downloads/List_of_Apps_Flagged_CSV.csv") 
str(flagged_data)
names(flagged_data) 
flagged_data <- clean_names(flagged_data)
colnames(flagged_data)[colnames(flagged_data) == "ben"] <- "billed_entity_number_ben"
colnames(flagged_data)[colnames(flagged_data) == "frn"] <- "funding_request_number_frn"



length(unique(flagged_data$billed_entity_number_ben)) #454 different applicants
length(unique(flagged_data$application_number)) #715 different applications
flagged_data <- flagged_data[,c(1,4,9)]
str(flagged_data)

#e_rate_new <- distinct(e_rate_new)


###Step 2: Change the name of billed entitity number

flagged_data$billed_entity_number_ben <- as.factor(flagged_data$billed_entity_number_ben)
flagged_data$funding_request_number_frn <- as.factor(flagged_data$funding_request_number_frn)

###Step 3: combined the fcc data with the flaggged application

#####Best way to fill in
five <- flagged_data$funding_request_number_frn #important

Data$funding_request_number_frn
Data <- mutate(Data, flagged_frn = ifelse(funding_request_number_frn %in% five,1,0)) #it kinda worked, be careful. Result is 2559, but I only have 1736
sum(Data$flagged_frn == 1) #823 different observations
length(Data)
summary(Data$flagged_frn) #test out to see if you can do nested if else staement Example: ifelse(funding_request_number_frn %in% five,ifelse(total_funding_request %in% six),1,0))
Data$flagged_frn <- as.factor(Data$flagged_frn)
str(Data)
names(Data)

length(unique(flagged_data$funding_request_number_frn)) #something to note: 1334 frn numbers but 1737 applications. Better, but could create duplicates
frn_filter <- filter(Data,funding_request_number_frn == "ECF2190003226")
dim(frn_filter)
view(frn_filter)
str(frn_filter)


frn_filter_test <- filter(fcc_test, funding_request_number_frn == "ECF2190003226")
dim(frn_filter_test)
sum(frn_filter_test$flagged == 1, na.rm = TRUE)
view(frn_filter_test)
######

####Mixing with DUNS Number Help
#Step 1: Load the data
duns_number_data <- read_csv("C:/Users/dkim/Downloads/NUMBER_7-20-22.csv")
str(duns_number_data)
duns_number_data <- clean_names(duns_number_data)

Data$row_id <- 1:nrow(Data)
Data$row_id

#Step 2: Change the data type
duns_number_data[sapply(duns_number_data, is.character)] <- lapply(duns_number_data[sapply(duns_number_data, is.character)], as.factor)
duns_number_data$ben <- as.factor(duns_number_data$ben)

#Step 3: Change the name of the variable
colnames(duns_number_data)[colnames(duns_number_data) == "ben"] <- "billed_entity_number_ben"
colnames(duns_number_data)[colnames(duns_number_data) == "applicant"] <- "applicant_name"
str(duns_number_data)

duns_number_data <- duns_number_data[,c(4,5,6,7)]
names(duns_number_data)
#Step 4: Join the datasets
dim(Data)
Data_complete <- left_join(Data, duns_number_data, by = "billed_entity_number_ben")
dim(Data_complete)
sum(duplicated(Data_complete$row_id)) #erase the rows that are duplicated

Data_complete <- Data_complete[!duplicated(Data_complete$row_id),]
dim(Data_complete) #I DID IT
Data_complete$applicant_duns_number

view(Data_complete)

#Step 5: Create the NSLP ratio
Data_complete$nslp_ratio <- Data_complete$nslp_student_count/Data_complete$total_student_count
Data_complete$nslp_ratio

#this is to remove any nan or infinite
Data_complete$nslp_ratio[is.nan(Data_complete$nslp_ratio)] <- NA 
Data_complete$nslp_ratio[is.infinite(Data_complete$nslp_ratio)] <- NA

#step 6: Create the difference in NSLP Ratio and Unmet need ratio

Data_complete$Unmet_need_device_NSLP_Ratio_Difference <- Data_complete$unmet_need_device_student_ratio - Data_complete$nslp_ratio
Data_complete$Unmet_need_service_NSLP_Ratio_Difference <- Data_complete$unmet_need_service_student_ratio - Data_complete$nslp_ratio
Data_complete$Unmet_need_device_NSLP_Ratio_Difference #if its positive, it has a higher chance of it bein flagged. If negative it has a lower chance
Data_complete$Unmet_need_service_NSLP_Ratio_Difference

Data_complete$applicant_duns_number
duns_number <- Data_complete %>% group_by(billed_entity_number_ben) %>% summarise(applicant_name = applicant_name, applicant_duns_number = applicant_duns_number)
duns_number$keep_or_delete <- ifelse(duplicated(duns_number$billed_entity_number_ben),0,1)
view(duns_number)
duns_number <- duns_number[duns_number$keep_or_delete == 1,]
duns_number <- na.omit(duns_number)
write_csv(duns_number, "C:/Users/dkim/Downloads/duns_fcc_data")



###Combining it with E-rate dataset (to get NSLP and library student count)
#run this first
Data$row_id <- 1:nrow(Data)
Data$row_id
###Step 1: load the data
e_rate <- read_csv("C:/Users/dkim/Downloads/E.csv") 
str(e_rate)
names(e_rate)
e_rate <- clean_names(e_rate)
colnames(e_rate)[colnames(e_rate) == "billed_entity_number"] <- "billed_entity_number_ben"
e_rate$billed_entity_number_ben

#######Finding count in system 07/21/2022
e_rate_library <- filter(e_rate, funding_year == c(2021,2022) & window_status == "In Window" & applicant_type %in% c("Library","Library System"))
dim(e_rate_library)
e_rate_library <- e_rate_library[,c(19,43)]
names(e_rate_library)

e_rate_library$billed_entity_number_ben <- as.factor(e_rate_library$billed_entity_number_ben)
colnames(e_rate_library)[colnames(e_rate_library) == "fulltime_enrollment"] <- "Library_Total_Student_Count"

Data_complete <- left_join(Data_complete, e_rate_library, by = "billed_entity_number_ben")
dim(Data_complete)
dim(Data)

sum(duplicated(Data_complete$row_id))

Data_complete <- Data_complete[!duplicated(Data_complete$row_id),]
dim(Data_complete) #I DID IT
summary(Data_complete$Library_Total_Student_Count)
length(unique(Data_complete$Library_Total_Student_Count)) #646 libraries and libraries applicants
fcc
#replaces NA value in total student count with library total student count values
Data_complete$total_student_count[is.na(Data_complete$total_student_count)] <- as.character(Data_complete$Library_Total_Student_Count[is.na(Data_complete$total_student_count)])
Data_complete$total_student_count <-as.numeric(Data_complete$total_student_count)
Data_complete$total_student_count
view(Data_complete)
sum(is.na(Data_complete$total_student_count))

#now put in the capita in here
Data_complete$funding_request_total_student_per_capita <- Data_complete$total_funding_commitment_request_amount/Data_complete$total_student_count
Data_complete$funding_request_total_student_per_capita
Data_complete$funding_request_total_student_per_capita[is.nan(Data_complete$funding_request_total_student_per_capita)] <- NA 
Data_complete$funding_request_total_student_per_capita[is.infinite(Data_complete$funding_request_total_student_per_capita)] <- NA

#Create column that shows library and library systems size
Data_complete <- mutate(Data_complete, All_Library_Size = case_when(applicant_type == "Library" & total_student_count > 0 & total_student_count <=1000 ~ "Small_Library_Size",
                                                                     applicant_type == "Library" & total_student_count >= 1001 & total_student_count <=4812 ~ "Medium_Library_Size",
                                                                     applicant_type == "Library" & total_student_count >= 4813 & total_student_count <=38082 ~ "Large_Library_Size",
                                                                     applicant_type == "Library" & total_student_count > 38083 ~ "Outliers_Library_Size"))

Data_complete <- mutate(Data_complete, Public_Library_Size = case_when(applicant_type == "Library" & Public_or_Private == "Public" & total_student_count > 0 & total_student_count <=3102 ~ "Small_Public_Library_Size",
                                                       applicant_type == "Library" & Public_or_Private == "Public" & total_student_count >= 3103 & total_student_count <=40044 ~ "Medium_Public_Library_Size",
                                                       applicant_type == "Library" & Public_or_Private == "Public" & total_student_count >= 40445 & total_student_count <=273184 ~ "Large_Public_Library_Size",
                                                       applicant_type == "Library" & Public_or_Private == "Public" & total_student_count > 273185 ~ "Outliers_Public_Library_Size"))

Data_complete <- mutate(Data_complete, All_Library_Systems_Size = case_when(applicant_type == "Library System" & total_student_count > 0 & total_student_count <=3187 ~ "Small_All_Library_Systems_Size",
                                                                              applicant_type == "Library System" & total_student_count >= 3188 & total_student_count <=41259 ~ "Medium_All_Library_Systems_Size",
                                                                              applicant_type == "Library System" & total_student_count >= 41260 & total_student_count <=273184 ~ "Large_All_Library_Systems_Size",
                                                                              applicant_type == "Library System" & total_student_count > 273185 ~ "Outliers_All_Library_Systems_Size"))

Data_complete <- mutate(Data_complete, Public_Library_Systems_Size = case_when(applicant_type == "Library System" & total_student_count > 0 & total_student_count <1000 ~ "Small_Public_Library_Systems_Size",
                                                                     applicant_type == "Library System" & total_student_count > 1000 & total_student_count <3000 ~ "Medium_Public_Library_Systems_Size",
                                                                     applicant_type == "Library System" & total_student_count > 3000 & total_student_count <6000 ~ "Large_Public_Library_Systems_Size",
                                                                     applicant_type == "Library System" & total_student_count > 6000 ~ "Outliers_Public_Library_Systems_Size"))


Data_complete$All_Library_Size <- replace(Data_complete$All_Library_Size, is.na(Data_complete$All_Library_Size),0)
Data_complete$Public_Library_Size <- replace(Data_complete$Public_Library_Size, is.na(Data_complete$Public_Library_Size),0)
Data_complete$All_Library_Systems_Size <- replace(Data_complete$All_Library_Systems_Size, is.na(Data_complete$All_Library_Systems_Size),0)
Data_complete$Public_Library_Systems_Size <- replace(Data_complete$Public_Library_Systems_Size, is.na(Data_complete$Public_Library_Systems_Size),0)

#Changing reference point
Data_complete[sapply(Data_complete, is.character)] <- lapply(Data_complete[sapply(Data_complete, is.character)], as.factor) #changing the reference level requires that its all factors 
str(Data_complete)

Data_complete$All_Library_Size <- relevel(Data_complete$All_Library_Size, ref = 4)
Data_complete$Public_Library_Size <- relevel(Data_complete$Public_Library_Size, ref = 4)
Data_complete$All_Library_Systems_Size <- relevel(Data_complete$All_Library_Systems_Size, ref = 4)
Data_complete$Public_Library_Systems_Size <- relevel(Data_complete$Public_Library_Systems_Size, ref = 4)

Data_complete <- Data_complete %>% relocate(All_Library_Size,Public_Library_Size,All_Library_Systems_Size,Public_Library_Systems_Size, .after = Library_Type)
names(Data_complete)

##Time to find the library population 
#library
Total_student_count_Library <- filter(Data_complete, applicant_type == "Library")
Library_Total_Student_Draft <- Total_student_count_Library %>% group_by(billed_entity_number_ben,form_version) %>% summarise(real_ben = unique(billed_entity_number_ben), real_library_student_count = unique(Library_Total_Student_Count))

Library_Total_Student_Draft$Keep_or_Delete <- ifelse(duplicated(Library_Total_Student_Draft$real_ben) & Library_Total_Student_Draft$form_version == "Original", 0,ifelse(duplicated(Library_Total_Student_Draft$real_ben) & max(Library_Total_Student_Draft$real_library_student_count),0,1))
Library_Total_Student_Draft$Keep_or_Delete

Library_Total_Student_Draft <- Library_Total_Student_Draft[Library_Total_Student_Draft$Keep_or_Delete == 1,]
sum(Library_Total_Student_Draft$real_library_student_count,na.rm = TRUE) #Real answer
summary(Library_Total_Student_Draft$real_library_student_count)
boxplot(Library_Total_Student_Draft$real_library_student_count) #7/13/2022 Use this on others
dim(Library_Total_Student_Draft)
quantile(Library_Total_Student_Draft$real_library_student_count,0.1, na.rm = TRUE)
quantile(Library_Total_Student_Draft$real_library_student_count,0.9, na.rm = TRUE)
length(unique(Library_Total_Student_Draft$real_ben)) #621 libraries
Library_Total_Student_Draft$real_ben

sum(Total_student_count_Library$applicant_type == "Library")
dim(Total_student_count_Library)

view(Library_Total_Student_Draft)

#library systems
levels(Data$applicant_type)
Total_student_count_Library_System <- filter(Data_complete, applicant_type == "Library System")
Library_System_Total_Student_Draft <- Total_student_count_Library_System %>% group_by(billed_entity_number_ben,form_version) %>% summarise(real_ben = unique(billed_entity_number_ben), real_library_student_count = unique(Library_Total_Student_Count))

Library_System_Total_Student_Draft$Keep_or_Delete <- ifelse(duplicated(Library_System_Total_Student_Draft$real_ben) & Library_System_Total_Student_Draft$form_version == "Original", 0,ifelse(duplicated(Library_System_Total_Student_Draft$real_ben) & max(Library_System_Total_Student_Draft$real_library_student_count),0,1))
Library_System_Total_Student_Draft$Keep_or_Delete

Library_System_Total_Student_Draft <- Library_System_Total_Student_Draft[Library_System_Total_Student_Draft$Keep_or_Delete == 1,]
sum(Library_System_Total_Student_Draft$real_library_student_count,na.rm = TRUE) #24,620,971
summary(Library_System_Total_Student_Draft$real_library_student_count)
boxplot(Library_System_Total_Student_Draft$real_library_student_count) #7/13/2022 Use this on others
dim(Library_System_Total_Student_Draft)
quantile(Library_System_Total_Student_Draft$real_library_student_count,0.1, na.rm = TRUE)
quantile(Library_System_Total_Student_Draft$real_library_student_count,0.9, na.rm = TRUE)
length(unique(Library_System_Total_Student_Draft$real_ben)) #513 library system
Library_System_Total_Student_Draft$real_ben

sum(Total_student_count_Library$applicant_type == "Library Systems")
dim(Total_student_count_Library)

view(Library_Total_Student_Draft)


dim(Data_complete)

write_csv(Data_complete,"C:/Users/dkim/Downloads/20220804_ECF_Dataset_main_modified_v0.csv" )
#20220727_ECF_Dataset_main_modified_v02
#########


######Mixing Esser data (new, 08/04/2022)
elementary_funds <- read_csv("C:/Users/dkim/Downloads/ESSER_BEN_20220804.csv")
str(elementary_funds)

elementary_funds <- clean_names(elementary_funds)

###Step 2: Change school name to match fcc data school name and duns number
colnames(elementary_funds)[colnames(elementary_funds) == "ben"] <- "billed_entity_number_ben"
elementary_funds$billed_entity_number_ben <- as.factor(elementary_funds$billed_entity_number_ben)
colnames(elementary_funds)[colnames(elementary_funds) == "esser_overlap"] <- "esser_service_type_overlap"
elementary_funds$esser_service_type_overlap <- as.factor(elementary_funds$esser_service_type_overlap)

###Step 3: Create new column
elementary_funds$billed_entity_number_ben
elementary_funds$ECF_ESSER_Overlap <- seq(1)
elementary_funds$ECF_ESSER_Overlap

elementary_funds <- elementary_funds[,c(1,3,4)]
###Step 4: Combine with main data
Data_complete <- left_join(Data_complete, elementary_funds, by = "billed_entity_number_ben")
dim(Data_complete)
dim(Data)

sum(duplicated(Data_complete$row_id))

Data_complete <- Data_complete[!duplicated(Data_complete$row_id),]
dim(Data_complete) #I DID IT

###Step 5: Customize
Data_complete$ECF_ESSER_Overlap <- replace_na(Data_complete$ECF_ESSER_Overlap, 0)
sum(Data_complete$ECF_ESSER_Overlap == 1)
Data_complete$ECF_ESSER_Overlap <- as.factor(Data_complete$ECF_ESSER_Overlap)

######Mixing GEER data (new, 08/04/2022)
geer_funds <- read_csv("C:/Users/dkim/Downloads/GEER_BEN_20220804.csv")
str(geer_funds)

geer_funds <- clean_names(geer_funds)

###Step 2: Change school name to match fcc data school name and duns number
colnames(geer_funds)[colnames(geer_funds) == "ben"] <- "billed_entity_number_ben"
geer_funds$billed_entity_number_ben <- as.factor(geer_funds$billed_entity_number_ben)
colnames(geer_funds)[colnames(geer_funds) == "geer_overlap"] <- "geer_service_type_overlap"
geer_funds$geer_service_type_overlap <- as.factor(geer_funds$geer_service_type_overlap)

###Step 3: Create new column
geer_funds$billed_entity_number_ben
geer_funds$ECF_GEER_Overlap <- seq(1)
geer_funds$ECF_GEER_Overlap

geer_funds <- geer_funds[,c(1,3,4)]
###Step 4: Combine with main data
Data_complete <- left_join(Data_complete, geer_funds, by = "billed_entity_number_ben")
dim(Data_complete)
dim(Data)

sum(duplicated(Data_complete$row_id))

Data_complete <- Data_complete[!duplicated(Data_complete$row_id),]
dim(Data_complete) #I DID IT

Data_complete$ECF_ESSER_Mix

###Step 5: Customize
Data_complete$ECF_GEER_Overlap <- replace_na(Data_complete$ECF_GEER_Overlap, 0)
sum(Data_complete$ECF_GEER_Overlap == 1)
Data_complete$ECF_GEER_Overlap <- as.factor(Data_complete$ECF_GEER_Overlap)

######Mixing Esser/Geer Mix data (new, 08/04/2022)
elementary_geer_funds <- read_csv("C:/Users/dkim/Downloads/ESEER_GEER_BEN_20220804.csv")
str(elementary_geer_funds)

elementary_geer_funds <- clean_names(elementary_geer_funds)

###Step 2: Change school name to match fcc data school name and duns number
colnames(elementary_geer_funds)[colnames(elementary_geer_funds) == "ben"] <- "billed_entity_number_ben"
elementary_geer_funds$billed_entity_number_ben <- as.factor(elementary_geer_funds$billed_entity_number_ben)
colnames(elementary_geer_funds)[colnames(elementary_geer_funds) == "esser_geer_ecf_overlap"] <- "esser_geer_service_type"
elementary_geer_funds$esser_geer_service_type <- as.factor(elementary_geer_funds$esser_geer_service_type)

###Step 3: Create new column
elementary_geer_funds$billed_entity_number_ben
elementary_geer_funds$ECF_ESSER_GEER_Overlap <- seq(1)
elementary_geer_funds$ECF_ESSER_GEER_Overlap

elementary_geer_funds <- elementary_geer_funds[,c(1,3,4)]
###Step 4: Combine with main data
Data_complete <- left_join(Data_complete, elementary_geer_funds, by = "billed_entity_number_ben")
dim(Data_complete)
dim(Data)

sum(duplicated(Data_complete$row_id))

Data_complete <- Data_complete[!duplicated(Data_complete$row_id),]
dim(Data_complete) #I DID IT

###Step 5: customize
Data_complete$ECF_ESSER_GEER_Overlap <- replace_na(Data_complete$ECF_ESSER_GEER_Overlap, 0)
sum(Data_complete$ECF_ESSER_GEER_Overlap == 1)
Data_complete$ECF_ESSER_GEER_Overlap <- as.factor(Data_complete$ECF_ESSER_GEER_Overlap)

### Creating all applicant type
#Creating new variables: applicant types like public schools but without the size
#School types
Data_complete <- mutate(Data_complete, Public_School_Type = case_when(applicant_type == "School" & Public_or_Private == "Public"~ "Public_School"))
Data_complete <- mutate(Data_complete, Private_School_Type = case_when(applicant_type == "School" & Public_or_Private == "Private" ~ "Private_School"))
Data_complete <- mutate(Data_complete, Charter_School_Type = case_when(applicant_type == "School"  & Charter_Yes_No == 1 ~ "Charter_School"))
Data_complete <- mutate(Data_complete, Tribal_School_Type = case_when(applicant_type == "School"  & Tribal_Yes_No == 1 ~ "Tribal_School"))
Data_complete <- mutate(Data_complete, Prek_School_Type = case_when(applicant_type == "School"  & PreK_Yes_No == 1 ~ "PreK_School"))
Data_complete <- mutate(Data_complete, HeadStart_School_Type = case_when(applicant_type == "School"  & HeadStart_Yes_No == 1 ~ "Headstart_School"))

#School District types
Data_complete <- mutate(Data_complete, Public_Districts_Type = case_when(applicant_type == "School District" & Public_or_Private == "Public" ~ "Public_District"))
Data_complete <- mutate(Data_complete, Private_Districts_Type = case_when(applicant_type == "School District" & Public_or_Private == "Private" ~ "Private_District"))
Data_complete <- mutate(Data_complete, Charter_Districts_Type = case_when(applicant_type == "School District" & Charter_Yes_No == 1 ~ "Charter_District"))
Data_complete <- mutate(Data_complete, ESA_Districts_Type = case_when(applicant_type == "School District" & ESA_Yes_No == 1 ~ "ESA_District"))

#Library types
Data_complete <- mutate(Data_complete, Public_Library_Type = case_when(applicant_type == "Library" & Public_or_Private == "Public" ~ "Public_Library"))
Data_complete <- mutate(Data_complete, Private_Library_Type = case_when(applicant_type == "Library" & Public_or_Private == "Private" ~ "Private_Library"))
Data_complete <- mutate(Data_complete, Tribal_Library_Type = case_when(applicant_type == "Library" & Tribal_Yes_No == 1 ~ "Tribal_Library"))
Data_complete <- mutate(Data_complete, All_Library_Type = case_when(applicant_type == "Library" ~ "All_Library"))

#Library Systems types
Data_complete <- mutate(Data_complete, Public_Library_Systems_Type = case_when(applicant_type == "Library System" & Public_or_Private == "Public" ~ "Public_Library_System"))
Data_complete <- mutate(Data_complete, Private_Library_Systems_Type = case_when(applicant_type == "Library System" & Public_or_Private == "Private" ~ "Private_Library_System"))
Data_complete <- mutate(Data_complete, All_Library_Systems_Type = case_when(applicant_type == "Library System" ~ "All_Library_System"))

#Consortium
Data_complete <- mutate(Data_complete, Consortium = case_when(applicant_type == "Consortium" ~ "Consortium"))

#Turn into factors
Data_complete$Public_School_Type <- as.character(Data_complete$Public_School_Type)
Data_complete$Private_School_Type <- as.character(Data_complete$Private_School_Type)
Data_complete$Charter_School_Type <- as.character(Data_complete$Charter_School_Type)
Data_complete$Tribal_School_Type <- as.character(Data_complete$Tribal_School_Type)
Data_complete$Prek_School_Type <- as.character(Data_complete$Prek_School_Type)
Data_complete$HeadStart_School_Type <- as.character(Data_complete$HeadStart_School_Type)
Data_complete$Public_Districts_Type <- as.character(Data_complete$Public_Districts_Type)
Data_complete$Private_Districts_Type <- as.character(Data_complete$Private_Districts_Type)
Data_complete$Charter_Districts_Type <- as.character(Data_complete$Charter_Districts_Type)
Data_complete$ESA_Districts_Type <- as.character(Data_complete$ESA_Districts_Type)
Data_complete$Public_Library_Type <- as.character(Data_complete$Public_Library_Type)
Data_complete$Private_Library_Type <- as.character(Data_complete$Private_Library_Type)
Data_complete$Tribal_Library_Type <- as.character(Data_complete$Tribal_Library_Type)
Data_complete$All_Library_Type <- as.character(Data_complete$All_Library_Type)
Data_complete$Public_Library_Systems_Type <- as.character(Data_complete$Public_Library_Systems_Type)
Data_complete$Private_Library_Systems_Type <- as.character(Data_complete$Private_Library_Systems_Type)
Data_complete$All_Library_Systems_Type <- as.character(Data_complete$All_Library_Systems_Type)
Data_complete$Consortium <- as.character(Data_complete$Consortium)


str(Data_complete$Public_School_Type)
#replacing <NA> with NA
Data_complete$Public_School_Type <- replace(Data_complete$Public_School_Type, is.na(Data_complete$Public_School_Type),NA)
Data_complete$Private_School_Type <- replace(Data_complete$Private_School_Type, is.na(Data_complete$Private_School_Type),NA)
Data_complete$Charter_School_Type <- replace(Data_complete$Charter_School_Type, is.na(Data_complete$Charter_School_Type),NA)
Data_complete$Tribal_School_Type <- replace(Data_complete$Tribal_School_Type, is.na(Data_complete$Tribal_School_Type),NA)
Data_complete$Prek_School_Type <- replace(Data_complete$Prek_School_Type, is.na(Data_complete$Prek_School_Type),NA)
Data_complete$HeadStart_School_Type <- replace(Data_complete$HeadStart_School_Type, is.na(Data_complete$HeadStart_School_Type),NA)
Data_complete$Public_Districts_Type <- replace(Data_complete$Public_Districts_Type, is.na(Data_complete$Public_Districts_Type),NA)
Data_complete$Private_Districts_Type <- replace(Data_complete$Private_Districts_Type, is.na(Data_complete$Private_Districts_Type),NA)
Data_complete$Charter_Districts_Type <- replace(Data_complete$Charter_Districts_Type, is.na(Data_complete$Charter_Districts_Type),NA)
Data_complete$ESA_Districts_Type <- replace(Data_complete$ESA_Districts_Type, is.na(Data_complete$ESA_Districts_Type),NA)
Data_complete$Public_Library_Type <- replace(Data_complete$Public_Library_Type, is.na(Data_complete$Public_Library_Type),NA)
Data_complete$Private_Library_Type <- replace(Data_complete$Private_Library_Type, is.na(Data_complete$Private_Library_Type),NA)
Data_complete$Tribal_Library_Type <- replace(Data_complete$Tribal_Library_Type, is.na(Data_complete$Tribal_Library_Type),NA)
Data_complete$All_Library_Type <- replace(Data_complete$All_Library_Type, is.na(Data_complete$All_Library_Type),NA)
Data_complete$Public_Library_Systems_Type <- replace(Data_complete$Public_Library_Systems_Type, is.na(Data_complete$Public_Library_Systems_Type),NA)
Data_complete$Private_Library_Systems_Type <- replace(Data_complete$Private_Library_Systems_Type, is.na(Data_complete$Private_Library_Systems_Type),NA)
Data_complete$All_Library_Systems_Type <- replace(Data_complete$All_Library_Systems_Type, is.na(Data_complete$All_Library_Systems_Type),NA)
Data_complete$Consortium <- replace(Data_complete$Consortium, is.na(Data_complete$Consortium),NA)

#replacing NA with 0
Data_complete$Public_School_Type <- replace(Data_complete$Public_School_Type, is.na(Data_complete$Public_School_Type),0)
Data_complete$Private_School_Type <- replace(Data_complete$Private_School_Type, is.na(Data_complete$Private_School_Type),0)
Data_complete$Charter_School_Type <- replace(Data_complete$Charter_School_Type, is.na(Data_complete$Charter_School_Type),0)
Data_complete$Tribal_School_Type <- replace(Data_complete$Tribal_School_Type, is.na(Data_complete$Tribal_School_Type),0)
Data_complete$Prek_School_Type <- replace(Data_complete$Prek_School_Type, is.na(Data_complete$Prek_School_Type),0)
Data_complete$HeadStart_School_Type <- replace(Data_complete$HeadStart_School_Type, is.na(Data_complete$HeadStart_School_Type),0)
Data_complete$Public_Districts_Type <- replace(Data_complete$Public_Districts_Type, is.na(Data_complete$Public_Districts_Type),0)
Data_complete$Private_Districts_Type <- replace(Data_complete$Private_Districts_Type, is.na(Data_complete$Private_Districts_Type),0)
Data_complete$Charter_Districts_Type <- replace(Data_complete$Charter_Districts_Type, is.na(Data_complete$Charter_Districts_Type),0)
Data_complete$ESA_Districts_Type <- replace(Data_complete$ESA_Districts_Type, is.na(Data_complete$ESA_Districts_Type),0)
Data_complete$Public_Library_Type <- replace(Data_complete$Public_Library_Type, is.na(Data_complete$Public_Library_Type),0)
Data_complete$Private_Library_Type <- replace(Data_complete$Private_Library_Type, is.na(Data_complete$Private_Library_Type),0)
Data_complete$Tribal_Library_Type <- replace(Data_complete$Tribal_Library_Type, is.na(Data_complete$Tribal_Library_Type),0)
Data_complete$All_Library_Type <- replace(Data_complete$All_Library_Type, is.na(Data_complete$All_Library_Type),0)
Data_complete$Public_Library_Systems_Type <- replace(Data_complete$Public_Library_Systems_Type, is.na(Data_complete$Public_Library_Systems_Type),0)
Data_complete$Private_Library_Systems_Type <- replace(Data_complete$Private_Library_Systems_Type, is.na(Data_complete$Private_Library_Systems_Type),0)
Data_complete$All_Library_Systems_Type <- replace(Data_complete$All_Library_Systems_Type, is.na(Data_complete$All_Library_Systems_Type),0)
Data_complete$Consortium <- replace(Data_complete$Consortium, is.na(Data_complete$Consortium),0)
Data_complete[sapply(Data_complete, is.character)] <- lapply(Data_complete[sapply(Data_complete, is.character)], as.factor)

str(Data_complete$Consortium)
sum(Data_complete$Consortium == "Consortium") #all libraries, library systems, and consortium do not work with other applicants type (by themselves it work)

#creating all applicant types
Data_complete <- mutate(Data_complete, applicant_all_type = case_when(applicant_type == "School" & Public_or_Private == "Public"~ "Public_School",
                                                                            applicant_type == "School" & Public_or_Private == "Private" ~ "Private_School",
                                                                            applicant_type == "School"  & Charter_Yes_No == 1 ~ "Charter_School",
                                                                            applicant_type == "School"  & Tribal_Yes_No == 1 ~ "Tribal_School",
                                                                            applicant_type == "School"  & PreK_Yes_No == 1 ~ "PreK_School",
                                                                            applicant_type == "School"  & HeadStart_Yes_No == 1 ~ "Headstart_School",
                                                                            applicant_type == "School District" & Public_or_Private == "Public" ~ "Public_District",
                                                                            applicant_type == "School District" & Public_or_Private == "Private" ~ "Private_District",
                                                                            applicant_type == "School District" & Charter_Yes_No == 1 ~ "Charter_District",
                                                                            applicant_type == "School District" & ESA_Yes_No == 1 ~ "Small_ESA_District",
                                                                            applicant_type == "Consortium"~ "Consortium",
                                                                            applicant_type == "Library" & Public_or_Private == "Private" ~ "Private_Library",
                                                                            applicant_type == "Library" & Public_or_Private == "Public" ~ "Public_Library",
                                                                            applicant_type == "Library System" & Public_or_Private == "Private" ~ "Private_Library_System",
                                                                            applicant_type == "Library System" & Public_or_Private == "Public" ~ "Public_Library_System"))



#log transform
Data_complete$total_funding_commitment_request_amount_log <- log(Data_complete$total_funding_commitment_request_amount + 1)
Data_complete$frn_approved_amount_log <- log(Data_complete$frn_approved_amount + 1)
Data_complete$total_student_count_log <- log(Data_complete$total_student_count + 1)
Data_complete$nslp_student_count_log <- log(Data_complete$nslp_student_count + 1)
Data_complete$unmet_need_device_log <- log(Data_complete$unmet_need_device + 1)
Data_complete$unmet_need_service_log <- log(Data_complete$unmet_need_service + 1)
Data_complete$one_time_unit_cost_log <- log(Data_complete$one_time_unit_cost +1 )
Data_complete$one_time_unit_quantity_log <- log(Data_complete$one_time_unit_quantity + 1)

#####Step 4: Creating that new dataset
str(Data_complete$ECF_ESSER_Overlap)
dim(Data_complete)

#Latest version 08/18/2022
write_csv(Data_complete, "C:/Users/dkim/Downloads/20220818_ECF_Dataset_main_modified_v1.csv")




length(unique(Data$total_funding_commitment_request_amount))
sum(Data$application_status == "Committed", na.rm = TRUE)
h <- filter(Data, application_status == "Committed") #There is 43,557 committed applications
length(unique(h$total_funding_commitment_request_amount))
length(unique(h$billed_entity_number_ben))
dim(h)

Data$funding_request_status
ggplot(data = h) + #shows the number of applicants, next step is to do percentages
  aes(x = funding_request_status) +
  geom_bar() +
  ggtitle(label = "Funding Request")

table(h$funding_request_status)
table(h$funding_request_status)/sum(table(h$funding_request_status))



summary(h$unment_totalstudent_ratio)



f <- Data_complete
sum(is.na(f$total_student_count))
f$total_student_count[is.na(f$total_student_count)] <- as.character(f$Library_Total_Student_Count[is.na(f$total_student_count)])
f$total_student_count <-as.numeric(f$total_student_count)
f$total_student_count
view(f)

###Unmet need/ total student ratio analysis
length(unique(Data_window1$billed_entity_number_ben))
9337-9133

##ECf Window 1 
#Data_window1 <- filter(Data, filing_window == "ECF Window 1" & application_status == "Committed")
#Data_window1 <- filter(Data, filing_window == "ECF Window 1" & funding_request_status %in% c("Funded","Pending"))
Data_window1 <- filter(Data_complete, filing_window == "ECF Window 1" & applicant_type %in% c("School","School District","Consortium"))
summary(Data_window1$unment_totalstudent_ratio)
sum(Data_window1$unment_totalstudent_ratio > 1.25, na.rm = TRUE) #An idea to fix the funding request status when filter: Lets just run everything, then when we reach to the ifelse statement lets saying cancelled and denied is 0

Data_complete$Unmet_need_service_NSLP_Ratio_Difference
#Data_window1 <- Data_window1 %>% group_by(billed_entity_number_ben,form_version) %>% summarise(real_ben = unique(billed_entity_number_ben),real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service)
#Data_window1 <- Data_window1 %>% group_by(billed_entity_number_ben,form_version,funding_request_status) %>% summarise(real_ben = unique(billed_entity_number_ben),real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service)
#Data_window1 <- Data_window1 %>% group_by(billed_entity_number_ben,form_version,funding_request_status,total_student_count) %>% summarise(real_ben = unique(billed_entity_number_ben),real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service)
#Data_window1 <- Data_window1 %>% group_by(billed_entity_number_ben,form_version,funding_request_status,total_student_count, applicant_type) %>% summarise(real_ben = unique(billed_entity_number_ben),real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service)
Data_window1 <- Data_window1 %>% group_by(billed_entity_number_ben,applicant_name,form_version,funding_request_status,applicant_type,total_student_count,Consultant_Yes_No) %>% summarise(real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service, nslp_student_count = nslp_student_count, nslp_ratio = nslp_ratio, Unmet_need_device_NSLP_Ratio_Difference = Unmet_need_device_NSLP_Ratio_Difference, Unmet_need_service_NSLP_Ratio_Difference = Unmet_need_service_NSLP_Ratio_Difference )

view(Data_window1) #Also potential Idea: Look at https://stackoverflow.com/questions/15629885/replace-na-in-column-with-value-in-adjacent-column#:~:text=You%20have%20to%20do%20TEST%24UNIT%20%5Bis.na%20%28TEST%24UNIT%29%5D%20%3C-,replaced%20and%20the%20values%20to%20replace%20them%20with. and see if its possible to combine the total student count with library student count
dim(Data_window1)
sum(Data_window1$real_unment_totalstudent_ratio) #to fix this i need to do nested forloop? and nested if else?
dim(Data_window1)

#Data_window1$Keep_or_Delete <- ifelse(duplicated(Data_window1$real_ben) & Data_window1$form_version == "Original", 0,ifelse(duplicated(Data_window1$real_ben) & max(Data_window1$real_unmet_need_device_and_service),0,1))
#Data_window1$Keep_or_Delete <- ifelse(duplicated(Data_window1$real_ben) & Data_window1$form_version == "Original", 0, ifelse(Data_window1$funding_request_status %in% c("Cancelled","Denied"),0, ifelse(duplicated(Data_window1$real_ben) & max(Data_window1$real_unmet_need_device_and_service),0,1))) #test run to fix the denied/cancelled problem
Data_window1$Keep_or_Delete <- ifelse(Data_window1$funding_request_status %in% c("Cancelled","Denied"),0,ifelse(duplicated(Data_window1$billed_entity_number_ben) & Data_window1$form_version == "Original", 0, ifelse(duplicated(Data_window1$billed_entity_number_ben) & max(Data_window1$real_unmet_need_device_and_service),0,1))) #test run to fix the denied/cancelled problem
#Question to ask: how to filter this. We want those applications that are funded and pending, but what if the applicants current application in cancelled but their original is pending? should they still count or not?

view(Data_window1)
write_csv(Data_window1, "C:/Users/dkim/Downloads/Data_Window1_test.csv")
#ifelse(duplicated(Data_window1$real_ben) & max(Data_window1$real_unmet_need_device_and_service),0,1)

sum(Data_window1$Keep_or_Delete == 0, na.rm = TRUE)

Data_window1_draft <- Data_window1[Data_window1$Keep_or_Delete == 1,]

summary(Data_window1_draft$real_unmet_need_device_student_ratio)
summary(Data_window1_draft$real_unmet_need_service_student_ratio)

summary(Data_window1_draft$Unmet_need_device_NSLP_Ratio_Difference)
summary(Data_window1_draft$Unmet_need_service_NSLP_Ratio_Difference)

view(filter(Data_window1,billed_entity_number_ben == 17029532))

boxplot(Data_window1_draft$real_unment_totalstudent_ratio) #Idea when it comes to picking the newest 
dim(Data_window1_draft)
quantile(Data_window1_draft$real_unment_totalstudent_ratio,0.1)
Data_window1_draft <- Data_window1_draft[rowSums(is.na(Data_window1_draft)) != ncol(Data_window1_draft), ] #to remove empty rows that has only NA
view(Data_window1_draft)
dim(Data_window1_draft)

write_csv(Data_window1_draft,"C:/Users/dkim/Downloads/ECF_Window_1")

#less than 100 students
Data_window1_draft2 <- filter(Data_window1_draft, total_student_count > 100)
summary(Data_window1_draft2$real_unmet_need_device_student_ratio)
summary(Data_window1_draft2$real_unmet_need_service_student_ratio)
write_csv(Data_window1_draft2, "C:/Users/dkim/Downloads/Window_1_No_100_students")

Data_window1_draft3 <- filter(Data_window1_draft, total_student_count <= 100)
write_csv(Data_window1_draft3, "C:/Users/dkim/Downloads/Window_1_Only_100_students")
view(Data_window1_draft3)

#to see how many applicants
length(unique(Data_window1_draft$real_ben)) #7720 applicants
sum(Data_window1_draft$real_unmet_need_device_student_ratio > 1.0, na.rm = TRUE) #1298 have higher than 1:1 ratio
sum(Data_window1_draft$real_unmet_need_device_student_ratio > 1.25, na.rm = TRUE) #732 applications have higher than 125% ratio

sum(Data_window1_draft$real_unmet_need_service_student_ratio > 1.0, na.rm = TRUE) #623 have higher than 1:1 ratio
sum(Data_window1_draft$real_unmet_need_service_student_ratio > 1.25, na.rm = TRUE) #431 applications have higher than 125% ratio


#To find which BEN has the highest ratio
view(Data_window1_draft) #BEN 17029532 who has 60000%, BEN 11159 who has 21,200%, BEN 16078147 who has 2,800%, BEN 17017756 who has 2,000%

#for the graph
sort(Data_window1_draft$real_unment_totalstudent_ratio, decreasing = TRUE)
Data_window1_draft$real_unment_totalstudent_ratio_new <- ifelse(Data_window1_draft$real_unment_totalstudent_ratio < 10,Data_window1_draft$real_unment_totalstudent_ratio,NA)
Data_window1_draft$real_unment_totalstudent_ratio_new

window1_boxplot <- ggplot(data = Data_window1_draft) +
                      aes(x = real_unment_totalstudent_ratio_new) +
                      geom_boxplot() +
                      ggtitle(label = "Window 1 Unmet Needs Ratio Distribution")+
                      xlab(label = "Unmet Needs Total Student ratio") +
                      scale_x_continuous(labels = function(x) paste0(x * 100, '%'))
                      #scale_x_discrete("Unmet Needs Total Student ratio", labels = c("2" = "200%","4" = "400%", "6" = "600%","8" = "800%"))+
                      #scale_x_continuous(c(2,3,4,5,6))
window1_boxplot + theme(axis.text.y = element_blank())

#bar graph
window1_graph <- Data_window1_draft
window1_graph$unmet_need_group_device <- ifelse(window1_graph$real_unmet_need_device_student_ratio < 1.0, "Lower than 1:1", ifelse(window1_graph$real_unmet_need_device_student_ratio >= 1.0 & window1_graph$real_unmet_need_device_student_ratio <= 1.25, "1:1", ifelse(window1_graph$real_unmet_need_device_student_ratio > 1.25, "bigger than 1.25", 0) ))
window1_graph$unmet_need_group_device <- as.factor(window1_graph$unmet_need_group_device)
window1_graph$unmet_need_group_device <- relevel(window1_graph$unmet_need_group_device, ref = 3)
levels(window1_graph$unmet_need_group_device)

ggplot(data = window1_graph) +
  aes(x = unmet_need_group_device) +
  geom_bar(stat = "identity")
  
ggplot(window1_graph) +
  geom_bar(aes(x = unmet_need_group_device)) +
  ggtitle(label = "Window 1 Unmet Needs Ratio Device") +
  xlab(label = "Unmet Needs Total Student ratio Device")

names(Data_window1_draft)


  
summary(Data_window1_draft$Unmet_need_device_NSLP_Ratio_Difference)
summary(Data_window1_draft$Unmet_need_service_NSLP_Ratio_Difference)

##ECf Window 2
#Data_window2 <- filter(Data, filing_window == "ECF Window 2" & application_status == "Committed")
#Data_window2 <- filter(Data, filing_window == "ECF Window 2" & funding_request_status %in% c("Funded","Pending"))
Data_window2 <- filter(Data_complete, filing_window == "ECF Window 2" & applicant_type %in% c("School","School District","Consortium"))
summary(Data_window2$unment_totalstudent_ratio)
sum(Data_window2$unment_totalstudent_ratio > 1.0, na.rm = TRUE) #3102 have higher than 1:1 ratio
sum(Data_window2$unment_totalstudent_ratio > 1.25, na.rm = TRUE) #1715 have higher than 125% ratio

#Data_window2 <- Data_window2 %>% group_by(billed_entity_number_ben,form_version) %>% summarise(real_ben = unique(billed_entity_number_ben),real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service)
Data_window2 <- Data_window2 %>% group_by(billed_entity_number_ben,form_version,funding_request_status,total_student_count,applicant_type) %>% summarise(real_ben = unique(billed_entity_number_ben),real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service)
Data_window2 <- Data_window2 %>% group_by(billed_entity_number_ben, applicant_name, form_version,funding_request_status,applicant_type,total_student_count,Consultant_Yes_No) %>% summarise(real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service, nslp_student_count = nslp_student_count, nslp_ratio = nslp_ratio, Unmet_need_device_NSLP_Ratio_Difference = Unmet_need_device_NSLP_Ratio_Difference, Unmet_need_service_NSLP_Ratio_Difference = Unmet_need_service_NSLP_Ratio_Difference )

view(Data_window2) 
summary(Data_window2$real_unment_totalstudent_ratio)
sum(Data_window2$real_unment_totalstudent_ratio) #to fix this i need to do nested forloop? and nested if else?
dim(Data_window2)

#Data_window2$Keep_or_Delete <- ifelse(duplicated(Data_window2$real_ben) & Data_window2$form_version == "Original", 0,ifelse(duplicated(Data_window2$real_ben) & max(Data_window2$real_unmet_need_device_and_service),0,1))
#Data_window2$Keep_or_Delete <- ifelse(duplicated(Data_window2$real_ben) & Data_window2$form_version == "Original", 0, ifelse(Data_window2$funding_request_status %in% c("Cancelled","Denied"),0, ifelse(duplicated(Data_window2$real_ben) & max(Data_window2$real_unmet_need_device_and_service),0,1))) #test run to fix the denied/cancelled problem
Data_window2$Keep_or_Delete <- ifelse(Data_window2$funding_request_status %in% c("Cancelled","Denied"),0,ifelse(duplicated(Data_window2$billed_entity_number_ben) & Data_window2$form_version == "Original", 0, ifelse(duplicated(Data_window2$billed_entity_number_ben) & max(Data_window2$real_unmet_need_device_and_service),0,1))) #test run to fix the denied/cancelled problem

sum(is.na(Data_window2$Keep_or_Delete)) #177 schools

Data_window2_draft <- Data_window2[Data_window2$Keep_or_Delete == 1,]
Data_window2_draft <- Data_window2_draft[rowSums(is.na(Data_window2_draft)) != ncol(Data_window2_draft), ] #to remove empty rows that has only NA
view(Data_window2_draft)
dim(Data_window2_draft)

summary(Data_window2_draft$real_unmet_need_device_student_ratio) #Mean is 0.53, Median is 0.37, Max is 16
summary(Data_window2_draft$real_unmet_need_service_student_ratio) #Mean is 0.23, Median is 0.02, Max is 16

summary(Data_window2_draft$Unmet_need_device_NSLP_Ratio_Difference) #Mean is 0.53, Median is 0.37, Max is 16
summary(Data_window2_draft$Unmet_need_service_NSLP_Ratio_Difference)

boxplot(Data_window2_draft$real_unment_totalstudent_ratio) 
dim(Data_window2_draft)
quantile(Data_window2_draft$real_unment_totalstudent_ratio,0.1)
view(Data_window2_draft)
length(unique(Data_window2_draft$real_ben))

#To see how many applicants are there
length(unique(Data_window2_draft$billed_entity_number_ben)) #3203 applicants
sum(Data_window2_draft$real_unmet_need_device_student_ratio > 1.0, na.rm = TRUE) #472 have higher than 1:1 ratio
sum(Data_window2_draft$real_unmet_need_device_student_ratio > 1.25, na.rm = TRUE) #255 applications have higher than 125% ratio

sum(Data_window2_draft$real_unmet_need_service_student_ratio > 1.0, na.rm = TRUE) #214 have higher than 1:1 ratio
sum(Data_window2_draft$real_unmet_need_service_student_ratio > 1.25, na.rm = TRUE) #147 applications have higher than 125% ratio

#Creating a file
write_csv(Data_window2_draft, "C:/Users/dkim/Downloads/ECF_Window_2")

#To find the highest ratio
view(Data_window2_draft)
#for graph
sort(Data_window2_draft$real_unment_totalstudent_ratio, decreasing = TRUE)
Data_window2_draft$real_unment_totalstudent_ratio_new <- ifelse(Data_window1_draft$real_unment_totalstudent_ratio < 10,Data_window1_draft$real_unment_totalstudent_ratio,NA)
Data_window2_draft$real_unment_totalstudent_ratio_new

window2_boxplot <- ggplot(data = Data_window2_draft) +
  aes(x = real_unment_totalstudent_ratio) +
  geom_boxplot() +
  ggtitle(label = "Window 2 Unmet Needs Ratio Distribution")+
  xlab(label = "Unmet Needs Total Student ratio") +
  scale_x_continuous(labels = function(x) paste0(x * 100, '%'))
#scale_x_discrete("Unmet Needs Total Student ratio", labels = c("2" = "200%","4" = "400%", "6" = "600%","8" = "800%"))+
#scale_x_continuous(c(2,3,4,5,6))
window2_boxplot + theme(axis.text.y = element_blank())


##Building a bar chart for all three windows

##ECf Window 3
#Data_window3 <- filter(Data, filing_window == "ECF Window 3" & application_status == "Committed")
#Data_window3 <- filter(Data, filing_window == "ECF Window 3" & funding_request_status %in% c("Funded","Pending"))
Data_window3 <- filter(Data_complete, filing_window == "ECF Window 3" & applicant_type %in% c("School","School District","Consortium"))

summary(Data_window3$unment_totalstudent_ratio)
sum(Data_window3$unment_totalstudent_ratio > 1.0, na.rm = TRUE) #3478 have higher than 1:1 ratio
sum(Data_window3$unment_totalstudent_ratio > 1.25, na.rm = TRUE) #1878 applications have higher than 125% ratio

#Data_window3 <- Data_window3 %>% group_by(billed_entity_number_ben,form_version) %>% summarise(real_ben = unique(billed_entity_number_ben),real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service)
Data_window3 <- Data_window3 %>% group_by(billed_entity_number_ben,form_version,funding_request_status,total_student_count,applicant_type) %>% summarise(real_ben = unique(billed_entity_number_ben),real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service)
Data_window3 <- Data_window3 %>% group_by(billed_entity_number_ben, applicant_name, form_version,funding_request_status,applicant_type,total_student_count,Consultant_Yes_No) %>% summarise(real_ben = unique(billed_entity_number_ben),real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service, nslp_student_count = nslp_student_count, nslp_ratio = nslp_ratio, Unmet_need_device_NSLP_Ratio_Difference = Unmet_need_device_NSLP_Ratio_Difference, Unmet_need_service_NSLP_Ratio_Difference = Unmet_need_service_NSLP_Ratio_Difference )
Data_window3 <- Data_window3 %>% group_by(billed_entity_number_ben, applicant_name, form_version,funding_request_status,applicant_type,total_student_count,Consultant_Yes_No) %>% summarise(real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service, nslp_student_count = nslp_student_count, nslp_ratio = nslp_ratio, Unmet_need_device_NSLP_Ratio_Difference = Unmet_need_device_NSLP_Ratio_Difference, Unmet_need_service_NSLP_Ratio_Difference = Unmet_need_service_NSLP_Ratio_Difference )

Data_window3 #the unmet needs won't intereact with the unmet need ratio
view(Data_window3) 

#Data_window3$Keep_or_Delete <- ifelse(duplicated(Data_window3$real_ben) & Data_window3$form_version == "Original", 0,ifelse(duplicated(Data_window3$real_ben) & max(Data_window3$real_unmet_need_device_and_service),0,1))
#Data_window3$Keep_or_Delete <- ifelse(duplicated(Data_window3$real_ben) & Data_window3$form_version == "Original", 0, ifelse(Data_window3$funding_request_status %in% c("Cancelled","Denied"),0, ifelse(duplicated(Data_window3$real_ben) & max(Data_window3$real_unmet_need_device_and_service),0,1))) #test run to fix the denied/cancelled problem
Data_window3$Keep_or_Delete <- ifelse(Data_window3$funding_request_status %in% c("Cancelled","Denied"),0,ifelse(duplicated(Data_window3$real_ben) & Data_window3$form_version == "Original", 0, ifelse(duplicated(Data_window3$real_ben) & max(Data_window3$real_unmet_need_device_and_service),0,1))) #test run to fix the denied/cancelled problem
Data_window3$Keep_or_Delete <- ifelse(Data_window3$funding_request_status %in% c("Cancelled","Denied"),0,ifelse(duplicated(Data_window3$billed_entity_number_ben) & Data_window3$form_version == "Original", 0, ifelse(duplicated(Data_window3$billed_entity_number_ben) & max(Data_window3$real_unmet_need_device_and_service),0,1))) #test run to fix the denied/cancelled problem

1298/8461
Data_window3$Keep_or_Delete
view(Data_window3)

Data_window3_draft <- Data_window3[Data_window3$Keep_or_Delete == 1,]
Data_window3_draft <- Data_window3_draft[rowSums(is.na(Data_window3_draft)) != ncol(Data_window3_draft), ] #to remove empty rows that has only NA
view(Data_window3_draft)
dim(Data_window3_draft)

summary(Data_window3_draft$real_unmet_need_device_student_ratio) #Mean is 0.6084, Median is 0.30, Max is 400
summary(Data_window3_draft$real_unmet_need_service_student_ratio) #Mean is 0.2853, Median is 0.0144, Max is 50.8

summary(Data_window3_draft$Unmet_need_device_NSLP_Ratio_Difference) #Mean is 0.6084, Median is 0.30, Max is 400
summary(Data_window3_draft$Unmet_need_service_NSLP_Ratio_Difference) #Mean is 0.2853, Median is 0.0144, Max is 50.8

boxplot(Data_window3_draft$real_unment_totalstudent_ratio) 
dim(Data_window3_draft)
quantile(Data_window3_draft$real_unment_totalstudent_ratio,0.1)
view(Data_window3_draft)
length(unique(Data_window3_draft$billed_entity_number_ben))

##All ECF Window
#Data_window_all <- filter(Data, filing_window == "ECF Window 3" & application_status == "Committed")
#Data_window_all <- filter(Data, filing_window == "ECF Window 3" & funding_request_status %in% c("Funded","Pending"))
Data_window_all <- filter(Data_complete, applicant_type %in% c("School","School District","Consortium"))

summary(Data_window_all$unment_totalstudent_ratio)
sum(Data_window_all$unment_totalstudent_ratio > 1.0, na.rm = TRUE) #3478 have higher than 1:1 ratio
sum(Data_window_all$unment_totalstudent_ratio > 1.25, na.rm = TRUE) #1878 applications have higher than 125% ratio

#Data_window3 <- Data_window3 %>% group_by(billed_entity_number_ben,form_version) %>% summarise(real_ben = unique(billed_entity_number_ben),real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service)
#Data_window_all <- Data_window_all %>% group_by(billed_entity_number_ben,form_version,funding_request_status,total_student_count,applicant_type) %>% summarise(real_ben = unique(billed_entity_number_ben),real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service)
#Data_window_all <- Data_window_all %>% group_by(billed_entity_number_ben, applicant_name, form_version,funding_request_status,applicant_type,total_student_count,Consultant_Yes_No) %>% summarise(real_ben = unique(billed_entity_number_ben),real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service, nslp_student_count = nslp_student_count, nslp_ratio = nslp_ratio, Unmet_need_device_NSLP_Ratio_Difference = Unmet_need_device_NSLP_Ratio_Difference, Unmet_need_service_NSLP_Ratio_Difference = Unmet_need_service_NSLP_Ratio_Difference )
Data_window_all <- Data_window_all %>% group_by(filing_window,billed_entity_number_ben, applicant_name, form_version,funding_request_status,applicant_type,total_student_count,Consultant_Yes_No) %>% summarise(real_unmet_need_device = unmet_need_device, real_unmet_need_device_student_ratio = unmet_need_device_student_ratio, real_unmet_need_service = unmet_need_service, real_unmet_need_service_student_ratio = unmet_need_service_student_ratio, real_unmet_need_device_and_service = unmet_need_device + unmet_need_service, nslp_student_count = nslp_student_count, nslp_ratio = nslp_ratio, Unmet_need_device_NSLP_Ratio_Difference = Unmet_need_device_NSLP_Ratio_Difference, Unmet_need_service_NSLP_Ratio_Difference = Unmet_need_service_NSLP_Ratio_Difference )

Data_window_all #the unmet needs won't intereact with the unmet need ratio
view(Data_window_all) 

#Data_window3$Keep_or_Delete <- ifelse(duplicated(Data_window3$real_ben) & Data_window3$form_version == "Original", 0,ifelse(duplicated(Data_window3$real_ben) & max(Data_window3$real_unmet_need_device_and_service),0,1))
#Data_window3$Keep_or_Delete <- ifelse(duplicated(Data_window3$real_ben) & Data_window3$form_version == "Original", 0, ifelse(Data_window3$funding_request_status %in% c("Cancelled","Denied"),0, ifelse(duplicated(Data_window3$real_ben) & max(Data_window3$real_unmet_need_device_and_service),0,1))) #test run to fix the denied/cancelled problem
Data_window_all$Keep_or_Delete <- ifelse(Data_window_all$funding_request_status %in% c("Cancelled","Denied"),0,ifelse(duplicated(Data_window3$real_ben) & Data_window3$form_version == "Original", 0, ifelse(duplicated(Data_window3$real_ben) & max(Data_window3$real_unmet_need_device_and_service),0,1))) #test run to fix the denied/cancelled problem
Data_window_all$Keep_or_Delete <- ifelse(Data_window_all$funding_request_status %in% c("Cancelled","Denied"),0,ifelse(duplicated(Data_window3$billed_entity_number_ben) & Data_window3$form_version == "Original", 0, ifelse(duplicated(Data_window3$billed_entity_number_ben) & max(Data_window3$real_unmet_need_device_and_service),0,1))) #test run to fix the denied/cancelled problem

1298/8461
Data_window_all$Keep_or_Delete
view(Data_window_all)

Data_window_all_draft <- Data_window_all[Data_window_all$Keep_or_Delete == 1,]
Data_window_all_draft <- Data_window_all_draft[rowSums(is.na(Data_window_all_draft)) != ncol(Data_window_all_draft), ] #to remove empty rows that has only NA
view(Data_window_all_draft)
dim(Data_window_all_draft)

summary(Data_window_all_draft$real_unmet_need_device_student_ratio) #Mean is 0.6084, Median is 0.30, Max is 400
summary(Data_window_all_draft$real_unmet_need_service_student_ratio) #Mean is 0.2853, Median is 0.0144, Max is 50.8

summary(Data_window_all_draft$Unmet_need_device_NSLP_Ratio_Difference) #Mean is 0.6084, Median is 0.30, Max is 400
summary(Data_window_all_draft$Unmet_need_service_NSLP_Ratio_Difference)

boxplot(Data_window_all_draft$real_unment_totalstudent_ratio) 
dim(Data_window_all_draft)
quantile(Data_window_all_draft$real_unment_totalstudent_ratio,0.1)
view(Data_window_all_draft)

#To find the amount of applicants
length(unique(Data_window_all_draft$real_ben)) #6653 applicants
sum(Data_window_all_draft$real_unmet_need_device_student_ratio > 1.0, na.rm = TRUE) #1104 have higher than 1:1 ratio
sum(Data_window_all_draft$real_unmet_need_device_student_ratio > 1.25, na.rm = TRUE) #640 applications have higher than 125% ratio

sum(Data_window_all_draft$real_unmet_need_service_student_ratio > 1.0, na.rm = TRUE) #580 have higher than 1:1 ratio
sum(Data_window_all_draft$real_unmet_need_service_student_ratio > 1.25, na.rm = TRUE) #441 applications have higher than 125% ratio

#Creating a file
write_csv(Data_window_all_draft, "C:/Users/dkim/Downloads/ECF_Window_3")

#To find the highest ratio
view(Data_window_all_draft)

#Building a histogram for all windows
graph <- Data_window_all_draft[Data_window_all_draft$Unmet_need_device_NSLP_Ratio_Difference > -2 & Data_window_all_draft$Unmet_need_device_NSLP_Ratio_Difference < 2,]

ggplot(graph, aes(x = Unmet_need_device_NSLP_Ratio_Difference, fill = filing_window )) +                           # Modify x- & y-axis limits
  geom_histogram() +
  #scale_x_continuous(n.breaks = 15) 
  xlim(-1, 2) +
  ggtitle(label = "Unmet NSLP Ratio difference - Device") +
  xlab(label = "Unmet NSLP Ratio Difference")
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

Data_window_all_draft$filing_window


#Data_complete$Unmet_need_device_NSLP_Ratio_Difference <- Data_complete$unmet_need_device_student_ratio - Data_complete$nslp_ratio
#Data_complete$Unmet_need_service_NSLP_Ratio_Difference <- Data_complete$unmet_need_service_student_ratio - Data_complete$nslp_ratio
hist(Data_complete$Unmet_need_device_NSLP_Ratio_Difference)
hist(Data_complete$Unmet_need_service_NSLP_Ratio_Difference)
dim(Data_window1_draft)

###Looking at nslp unmet need difference 
ggplot(Data_window1_draft, aes(x = Unmet_need_device_NSLP_Ratio_Difference)) +                           # Modify x- & y-axis limits
  geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = 0.1, fill = "blue") +
  xlim(- 0.8, 2)

ggplot(Data_window1_draft, aes(x = Unmet_need_device_NSLP_Ratio_Difference)) +                           # Modify x- & y-axis limits
  geom_histogram() +
  xlim(- 0.8, 2)

hist(Data_window1_draft$Unmet_need_device_NSLP_Ratio_Difference)
hist(Data_window1_draft$Unmet_need_service_NSLP_Ratio_Difference)
sum(Data_window1_draft$Unmet_need_device_NSLP_Ratio_Difference > 0, na.rm = TRUE)
sum(Data_window1_draft$Unmet_need_service_NSLP_Ratio_Difference > 0, na.rm = TRUE)
sum(Data_window1_draft$Unmet_need_device_NSLP_Ratio_Difference == 0, na.rm = TRUE)
sum(Data_window1_draft$Unmet_need_service_NSLP_Ratio_Difference == 0, na.rm = TRUE)
summary(Data_window1_draft$Unmet_need_device_NSLP_Ratio_Difference) #-0.08 median can be the bench mark
summary(Data_window1_draft$Unmet_need_service_NSLP_Ratio_Difference)#-0.34 median can be the bench mark

sum(Data_window2_draft$Unmet_need_device_NSLP_Ratio_Difference > 0, na.rm = TRUE)
sum(Data_window2_draft$Unmet_need_service_NSLP_Ratio_Difference > 0, na.rm = TRUE)
sum(Data_window2_draft$Unmet_need_device_NSLP_Ratio_Difference == 0, na.rm = TRUE)
sum(Data_window2_draft$Unmet_need_service_NSLP_Ratio_Difference == 0, na.rm = TRUE)
summary(Data_window2_draft$Unmet_need_device_NSLP_Ratio_Difference) #-0.07 median can be the bench mark
summary(Data_window2_draft$Unmet_need_service_NSLP_Ratio_Difference)#-0.37 median can be the bench mark

sum(Data_window3_draft$Unmet_need_device_NSLP_Ratio_Difference > 0, na.rm = TRUE)
sum(Data_window3_draft$Unmet_need_service_NSLP_Ratio_Difference > 0, na.rm = TRUE)
sum(Data_window3_draft$Unmet_need_device_NSLP_Ratio_Difference == 0, na.rm = TRUE)
sum(Data_window3_draft$Unmet_need_service_NSLP_Ratio_Difference == 0, na.rm = TRUE)
summary(Data_window3_draft$Unmet_need_device_NSLP_Ratio_Difference) #-0.12 median can be the bench mark
summary(Data_window3_draft$Unmet_need_service_NSLP_Ratio_Difference)#-0.37 median can be the bench mark

