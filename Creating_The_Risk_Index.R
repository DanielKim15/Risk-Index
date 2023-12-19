######Creating the risk index for the FCC

old <- read_csv("C:/Users/dkim/Downloads/Dataset_main_modified_v2.csv")
dim(old)
####Step 1: Load the Data 
funded_data <- read_csv("C:/Users/dkim/Downloads/20220915.csv")
funded_data$frn_unique_identifier_line_item <- paste(funded_data$funding_request_number_frn, funded_data$frn_line_item_id)
length(unique(funded_data$frn_unique_identifier_line_item))

#getting rid of missing data in total student count
sum(funded_data$total_student_count == 0, na.rm = TRUE) #119 with 0
funded_data$total_student_count <- replace(funded_data$total_student_count, funded_data$total_student_count %in% 0, NA)
sum(is.na(funded_data$total_student_count))  #replaced 0 with NA, originally 714, now its 833

funded_data <- funded_data[!is.na(funded_data$total_student_count),]
dim(funded_data)

####Step 1.2: Adding in service and device residuals
funded_data$service_residuals <- funded_data$monthly_recurring_unit_cost - 20
funded_data$service_residuals <- replace(funded_data$service_residuals, funded_data$service_residuals == -20, NA)
summary(funded_data$service_residuals)

funded_data$device_residuals <- funded_data$one_time_unit_cost - ifelse(funded_data$product_type %in% "Laptops",367,ifelse(funded_data$product_type %in% "Tablets",329,ifelse(funded_data$product_type %in% "Modems",280,ifelse(funded_data$product_type %in% "Routers",676,ifelse(funded_data$product_type %in% "Wi-Fi hotspots",90,ifelse(funded_data$product_type %in% "Modem/Router combined",476,0))))))
funded_data$device_residuals

funded_data <- filter(funded_data, funding_request_status %in% "Funded")
funded_data <- filter(funded_data, funding_request_status %in% "Funded" & product_type %in% c("Laptops","Modem/Router combined","Modems","Routers","Tablets","Wi-Fi hotspots",NA) & connection_type %in% c(NA,"Cable Modem","Datacasting Customer Premises Equipment","DSL","Installation, Activation, and Initial Configuration","Leased Lit Fiber","Lit Fiber Network (Construction)","Maintenance and Operation","Microwave","Mobile Broadband","Network Equipment","Other","Private Line Circuits","Satellite","Wireless Network (Self-Provisioning)","Wireless Network (Third-Party Construction)"))
str(funded_data)
dim(funded_data)


length(unique(funded_data$frn_unique_identifier_line_item))
funded_data$frn_unique_identifier_line_item

sum(funded_data$flagged_frn == 1)
###Step 2: Add some data in
#School types
funded_data <- mutate(funded_data, Public_School_Type = case_when(applicant_type == "School" & Public_or_Private == "Public"~ "Public_School"))
funded_data <- mutate(funded_data, Private_School_Type = case_when(applicant_type == "School" & Public_or_Private == "Private" ~ "Private_School"))
funded_data <- mutate(funded_data, Charter_School_Type = case_when(applicant_type == "School"  & Charter_Yes_No == 1 ~ "Charter_School"))
funded_data <- mutate(funded_data, Tribal_School_Type = case_when(applicant_type == "School"  & Tribal_Yes_No == 1 ~ "Tribal_School"))
funded_data <- mutate(funded_data, Prek_School_Type = case_when(applicant_type == "School"  & PreK_Yes_No == 1 ~ "PreK_School"))
funded_data <- mutate(funded_data, HeadStart_School_Type = case_when(applicant_type == "School"  & HeadStart_Yes_No == 1 ~ "Headstart_School"))

#School District types
funded_data <- mutate(funded_data, Public_Districts_Type = case_when(applicant_type == "School District" & Public_or_Private == "Public" ~ "Public_District"))
funded_data <- mutate(funded_data, Private_Districts_Type = case_when(applicant_type == "School District" & Public_or_Private == "Private" ~ "Private_District"))
funded_data <- mutate(funded_data, Charter_Districts_Type = case_when(applicant_type == "School District" & Charter_Yes_No == 1 ~ "Charter_District"))
funded_data <- mutate(funded_data, ESA_Districts_Type = case_when(applicant_type == "School District" & ESA_Yes_No == 1 ~ "ESA_District"))

#Library types
funded_data <- mutate(funded_data, Public_Library_Type = case_when(applicant_type == "Library" & Public_or_Private == "Public" ~ "Public_Library"))
funded_data <- mutate(funded_data, Private_Library_Type = case_when(applicant_type == "Library" & Public_or_Private == "Private" ~ "Private_Library"))
funded_data <- mutate(funded_data, Tribal_Library_Type = case_when(applicant_type == "Library" & Tribal_Yes_No == 1 ~ "Tribal_Library"))
funded_data <- mutate(funded_data, All_Library_Type = case_when(applicant_type == "Library" ~ "All_Library"))

#Library Systems types
funded_data <- mutate(funded_data, Public_Library_Systems_Type = case_when(applicant_type == "Library System" & Public_or_Private == "Public" ~ "Public_Library_System"))
funded_data <- mutate(funded_data, Private_Library_Systems_Type = case_when(applicant_type == "Library System" & Public_or_Private == "Private" ~ "Private_Library_System"))
funded_data <- mutate(funded_data, All_Library_Systems_Type = case_when(applicant_type == "Library System" ~ "All_Library_System"))

#Consortium
funded_data <- mutate(funded_data, Consortium = case_when(applicant_type == "Consortium" ~ "Consortium"))

#Turn into factors
funded_data$Public_School_Type <- as.character(funded_data$Public_School_Type)
funded_data$Private_School_Type <- as.character(funded_data$Private_School_Type)
funded_data$Charter_School_Type <- as.character(funded_data$Charter_School_Type)
funded_data$Tribal_School_Type <- as.character(funded_data$Tribal_School_Type)
funded_data$Prek_School_Type <- as.character(funded_data$Prek_School_Type)
funded_data$HeadStart_School_Type <- as.character(funded_data$HeadStart_School_Type)
funded_data$Public_Districts_Type <- as.character(funded_data$Public_Districts_Type)
funded_data$Private_Districts_Type <- as.character(funded_data$Private_Districts_Type)
funded_data$Charter_Districts_Type <- as.character(funded_data$Charter_Districts_Type)
funded_data$ESA_Districts_Type <- as.character(funded_data$ESA_Districts_Type)
funded_data$Public_Library_Type <- as.character(funded_data$Public_Library_Type)
funded_data$Private_Library_Type <- as.character(funded_data$Private_Library_Type)
funded_data$All_Library_Type <- as.character(funded_data$All_Library_Type)
funded_data$Tribal_Library_Type <- as.character(funded_data$Tribal_Library_Type)
funded_data$Public_Library_Systems_Type <- as.character(funded_data$Public_Library_Systems_Type)
funded_data$Private_Library_Systems_Type <- as.character(funded_data$Private_Library_Systems_Type)
funded_data$All_Library_Systems_Type <- as.character(funded_data$All_Library_Systems_Type)
funded_data$Consortium <- as.character(funded_data$Consortium)


str(funded_data$Public_School_Type)
#replacing <NA> with NA
funded_data$Public_School_Type <- replace(funded_data$Public_School_Type, is.na(funded_data$Public_School_Type),NA)
funded_data$Private_School_Type <- replace(funded_data$Private_School_Type, is.na(funded_data$Private_School_Type),NA)
funded_data$Charter_School_Type <- replace(funded_data$Charter_School_Type, is.na(funded_data$Charter_School_Type),NA)
funded_data$Tribal_School_Type <- replace(funded_data$Tribal_School_Type, is.na(funded_data$Tribal_School_Type),NA)
funded_data$Prek_School_Type <- replace(funded_data$Prek_School_Type, is.na(funded_data$Prek_School_Type),NA)
funded_data$HeadStart_School_Type <- replace(funded_data$HeadStart_School_Type, is.na(funded_data$HeadStart_School_Type),NA)
funded_data$Public_Districts_Type <- replace(funded_data$Public_Districts_Type, is.na(funded_data$Public_Districts_Type),NA)
funded_data$Private_Districts_Type <- replace(funded_data$Private_Districts_Type, is.na(funded_data$Private_Districts_Type),NA)
funded_data$Charter_Districts_Type <- replace(funded_data$Charter_Districts_Type, is.na(funded_data$Charter_Districts_Type),NA)
funded_data$ESA_Districts_Type <- replace(funded_data$ESA_Districts_Type, is.na(funded_data$ESA_Districts_Type),NA)
funded_data$Public_Library_Type <- replace(funded_data$Public_Library_Type, is.na(funded_data$Public_Library_Type),NA)
funded_data$Private_Library_Type <- replace(funded_data$Private_Library_Type, is.na(funded_data$Private_Library_Type),NA)
funded_data$Tribal_Library_Type <- replace(funded_data$Tribal_Library_Type, is.na(funded_data$Tribal_Library_Type),NA)
funded_data$All_Library_Type <- replace(funded_data$All_Library_Type, is.na(funded_data$All_Library_Type),NA)
funded_data$Public_Library_Systems_Type <- replace(funded_data$Public_Library_Systems_Type, is.na(funded_data$Public_Library_Systems_Type),NA)
funded_data$Private_Library_Systems_Type <- replace(funded_data$Private_Library_Systems_Type, is.na(funded_data$Private_Library_Systems_Type),NA)
funded_data$All_Library_Systems_Type <- replace(funded_data$All_Library_Systems_Type, is.na(funded_data$All_Library_Systems_Type),NA)
funded_data$Consortium <- replace(funded_data$Consortium, is.na(funded_data$Consortium),NA)

#replacing NA with 0
funded_data$Public_School_Type <- replace(funded_data$Public_School_Type, is.na(funded_data$Public_School_Type),0)
funded_data$Private_School_Type <- replace(funded_data$Private_School_Type, is.na(funded_data$Private_School_Type),0)
funded_data$Charter_School_Type <- replace(funded_data$Charter_School_Type, is.na(funded_data$Charter_School_Type),0)
funded_data$Tribal_School_Type <- replace(funded_data$Tribal_School_Type, is.na(funded_data$Tribal_School_Type),0)
funded_data$Prek_School_Type <- replace(funded_data$Prek_School_Type, is.na(funded_data$Prek_School_Type),0)
funded_data$HeadStart_School_Type <- replace(funded_data$HeadStart_School_Type, is.na(funded_data$HeadStart_School_Type),0)
funded_data$Public_Districts_Type <- replace(funded_data$Public_Districts_Type, is.na(funded_data$Public_Districts_Type),0)
funded_data$Private_Districts_Type <- replace(funded_data$Private_Districts_Type, is.na(funded_data$Private_Districts_Type),0)
funded_data$Charter_Districts_Type <- replace(funded_data$Charter_Districts_Type, is.na(funded_data$Charter_Districts_Type),0)
funded_data$ESA_Districts_Type <- replace(funded_data$ESA_Districts_Type, is.na(funded_data$ESA_Districts_Type),0)
funded_data$Public_Library_Type <- replace(funded_data$Public_Library_Type, is.na(funded_data$Public_Library_Type),0)
funded_data$Private_Library_Type <- replace(funded_data$Private_Library_Type, is.na(funded_data$Private_Library_Type),0)
funded_data$Tribal_Library_Type <- replace(funded_data$Tribal_Library_Type, is.na(funded_data$Tribal_Library_Type),0)
funded_data$All_Library_Type <- replace(funded_data$All_Library_Type, is.na(funded_data$All_Library_Type),0)
funded_data$Public_Library_Systems_Type <- replace(funded_data$Public_Library_Systems_Type, is.na(funded_data$Public_Library_Systems_Type),0)
funded_data$Private_Library_Systems_Type <- replace(funded_data$Private_Library_Systems_Type, is.na(funded_data$Private_Library_Systems_Type),0)
funded_data$All_Library_Systems_Type <- replace(funded_data$All_Library_Systems_Type, is.na(funded_data$All_Library_Systems_Type),0)
funded_data$Consortium <- replace(funded_data$Consortium, is.na(funded_data$Consortium),0)
funded_data[sapply(funded_data, is.character)] <- lapply(funded_data[sapply(funded_data, is.character)], as.factor)

str(funded_data$Consortium)
sum(funded_data$Consortium == "Consortium") #all libraries, library systems, and consortium do not work with other applicants type (by themselves it work)

#log transform
funded_data$total_funding_commitment_request_amount_log <- log(funded_data$total_funding_commitment_request_amount + 1)
funded_data$frn_approved_amount_log <- log(funded_data$frn_approved_amount + 1)
funded_data$total_student_count_log <- log(funded_data$total_student_count + 1)
funded_data$nslp_student_count_log <- log(funded_data$nslp_student_count + 1)
funded_data$unmet_need_device_log <- log(funded_data$unmet_need_device + 1)
funded_data$unmet_need_service_log <- log(funded_data$unmet_need_service + 1)
funded_data$one_time_unit_cost_log <- log(funded_data$one_time_unit_cost + 1)
funded_data$one_time_unit_quantity_log <- log(funded_data$one_time_unit_quantity + 1)
hist(funded_data$unmet_need_device)
names(funded_data)

hist(funded_data$unmet_need_device)
summary(funded_data$unmet_need_device_student_ratio)
#building all applicant column

funded_data[sapply(funded_data, is.character)] <- lapply(funded_data[sapply(funded_data, is.character)], as.factor)
funded_data <- mutate(funded_data, applicant_all_type = case_when(
  Charter_Districts_Type %in% "Charter_District" ~ "Charter_District", ESA_Districts_Type %in% "ESA_District" ~ "ESA_District", Tribal_Library_Type %in% "Tribal_Library" ~ "Tribal_Library", Public_Library_Systems_Type %in% "Public_Library_System" ~ "Public_Library_System",
  Private_School_Type %in% "Private_School" ~"Private_School", Public_Library_Type %in% "Public_Library" ~ "Public_Library", Consortium %in% "Consortium" ~ "Consortium",
  Charter_School_Type %in% "Charter_School" ~ "Charter_School", Private_Library_Type %in% "Private_Library" ~ "Private_Library",
  Tribal_School_Type %in% "Tribal_School" ~ "Tribal_School", Private_Library_Systems_Type %in% "Private_Library_System" ~ "Private_Library_System",
  Prek_School_Type %in% "PreK_School"~ "PreK_School",
  HeadStart_School_Type %in% "Headstart_School" ~ "Headstart_School",
  Public_Districts_Type %in% "Public_District"  ~ "Public_District",
  Private_Districts_Type %in% "Private_District" ~ "Private_District",
  Public_School_Type %in% "Public_School" ~ "Public_School"))

funded_data$applicant_all_type <- as.factor(funded_data$applicant_all_type)
levels(funded_data$applicant_all_type)


funded_data <- funded_data %>% group_by(funding_request_number_frn) %>% mutate(total_device_quantity = sum(one_time_unit_quantity), total_service_quantity = sum(monthly_quantity), total_device_service_quantity = total_device_quantity + total_service_quantity )
dim(funded_data)

#####Step 3: Start normalizing your key question results
#standardizing formula
part1 <- log((funded_data$total_funding_commitment_request_amount - min(funded_data$total_funding_commitment_request_amount))+1)
#part2 <- part1/max(funded_data$total_funding_commitment_request_amount)
part2 <- part1/max(part1)

summary(part1)
summary(part2)
hist(part2)


predict4 <- predict(train_test_3, funded_data, type = "response") #it seems i need to make a model for this particular analysis
summary(predict4)

names(multivariate_kq1)
multivariate_kq1$funding_request_number_frn
funded_data$funding_request_number_frn
str(funded_data$funding_request_number_frn)
funded_data$funding_request_number_frn <- as.factor(funded_data$funding_request_number_frn)
str(multivariate_kq1$funding_request_number_frn)
godzilla <- multivariate_kq1[,c(102,103,104,105,106,101,2)]
colnames(multivariate_kq1)
colnames(funded_data)
test <- left_join(funded_data, godzilla, by = "funding_request_number_frn")
dim(test)
dim(funded_data)
test$row_id <- 1:nrow(test)
test$row_id
key_question_1.5_data$frn_unique_identifier_line_item
names(key_question_1.5_data)
test2 <- left_join(test,key_question_1.5_data[,c(2,164,180,181,182,183)],"frn_unique_identifier_line_item")
dim(test2)
sum(duplicated(test2$row_id))
test3 <- test2[!duplicated(test2$row_id),]

test_send <- test2[!duplicated(test2$row_id),]
dim(test_send)
dim(test3)



##Creating unique identifier

#######################################################
#using the normalization function 09/13/2022
#Top and bottom coding
rescale    = function(x, topcode = (10^10), bottomcode = -(10^10), logValue = FALSE) {
  x[x>topcode] = topcode
  x[x<bottomcode] = bottomcode
  if (logValue==TRUE) {
  y = log(x - min(x, na.rm = TRUE) + 1)
  } else {
  y = (x - min(x, na.rm = TRUE))
  }
  
  y/(max(y, na.rm = TRUE) - min(y, na.rm = TRUE))
}

#NSLP Variance device
y = rescale(test3$Unmet_need_device_NSLP_Ratio_Difference,3 , -3)#160 data points are lost for this top/bottom code
sort(test3$Unmet_need_device_NSLP_Ratio_Difference,decreasing=FALSE)
hist(y)
y
summary(y)
hist(funded_data$total_student_count)
test3$NSLP_variance_device_scaled <- rescale(test3$Unmet_need_device_NSLP_Ratio_Difference,3 , -3,logValue = FALSE)

#NSLP Variance service
y = rescale(test3$Unmet_need_service_NSLP_Ratio_Difference,3 , -3,logValue = FALSE)#273 data points are lost for this top/bottom code
sort(test3$Unmet_need_service_NSLP_Ratio_Difference,decreasing=FALSE)
hist(y)
test3$NSLP_variance_service_scaled <- rescale(test3$Unmet_need_service_NSLP_Ratio_Difference,3 , -3, logValue = FALSE)

#Total funding predicted residuals
y = rescale(test3$funding_predicted_residuals, 1000000, -350000, logValue = FALSE)
hist(y)
y = rescale(test3$funding_predicted_residuals, 1000000, -450000, logValue = FALSE) #try this?
hist(y)
sort(test3$funding_predicted_residuals, decreasing = TRUE)
sum(test3$funding_predicted_residuals > 1050000, na.rm = TRUE) #1642 for 1 million, 966 if its 2 million, 586 if its 3 million
sum(test3$funding_predicted_residuals < -550000, na.rm = TRUE) #543,406 if 450000, 324 if 550000, so around 2000 data will be missing if we bottom/top code
summary(test3$funding_predicted_residuals)
hist(test3$funding_predicted_residuals)

test3$total_funding_residuals_scaled <-  rescale(test3$funding_predicted_residuals, 1000000, -450000, logValue = FALSE)
hist(test3$total_funding_residuals_scaled)

#Cost per student predict residuals
cost_per_student_funds_predicted

y = rescale(test3$cost_per_student_funds_predicted,logValue = TRUE, topcode = 3000)
#par(mar=c(1, 1, 1, 1)) #this fixes the plot is too big error
hist(y)

summary(test3$cost_per_student_funds_predicted)
sort(test3$cost_per_student_funds_predicted, decreasing = TRUE)
                         
test3$cost_per_student_residuals_scaled <- rescale(test3$cost_per_student_funds_predicted,logValue = TRUE, topcode = 3000)


#device residuals
test3$device_residuals
summary(test3$device_residuals)
sort(test3$device_residuals.x, decreasing = FALSE)
test3$device_residuals.x
y = rescale(test3$device_residuals.x, topcode = 700,bottomcode = -700,logValue = FALSE)
y = rescale(test3$device_residuals, topcode = 8000)
hist(y)
sort(test3$device_residuals.x, decreasing = TRUE)
y
test3$device_residuals.x
summary(test3$device_residuals.x)

test3$cost_per_unit_residuals_scaled <- rescale(test3$device_residuals.x, topcode = 700,bottomcode = -700,logValue = FALSE)
test3$cost_per_unit_residuals_scaled
sum(test3$cost_per_unit_residuals_scaled == 0.5,na.rm = TRUE)

#service residuals
y = rescale(test3$service_residuals.x, topcode = 500,bottomcode = -500,logValue = TRUE)
hist(y)
view(test3$service_residuals.x)
test3$service_residuals.x

test3$cost_per_monthly_residuals_scaled <- rescale(test3$service_residuals.x, topcode = 500,bottomcode = -500,logValue = TRUE)
view(test3$cost_per_monthly_residuals_scaled)
sum(test3$cost_per_monthly_residuals_scaled == 0.5, na.rm = TRUE)

#Unmet need ratio device residuals
y = rescale(test3$unmet_need_device_student_ratio,topcode = 2.5 ,logValue = TRUE)
hist(y)
sort(test3$unmet_need_device_student_ratio, decreasing = TRUE)

test3$unmet_need_student_ratio_device_scaled <- rescale(test3$unmet_need_device_student_ratio, topcode = 2.5,logValue = TRUE)


#Unmet need ratio service residuals
y = rescale(test3$unmet_need_service_student_ratio,topcode = 2 ,logValue = TRUE)
hist(y)
sort(test3$unmet_need_service_student_ratio, decreasing = TRUE)

test3$unmet_need_student_ratio_service_scaled <- rescale(test3$unmet_need_service_student_ratio,topcode = 2 ,logValue = TRUE)



updated_data_3$funding_request_number_frn.x
#########################################################################3

###Now combined this with the new dataset from the team
#Subtype relationship request and invoicing method
new_data <- read_csv("C:/Users/dkim/Downloads/20220906_Subtype Request Relationship_v02.csv")
dim(new_data)
new_data <- clean_names(new_data)
new_data$unique_identifier
colnames(new_data)[colnames(new_data) == "unique_identifier"] <- "frn_unique_identifier_line_item"
new_data$frn_unique_identifier_line_item
str(new_data$frn_unique_identifier_line_item)
new_data$frn_unique_identifier_line_item <- gsub("_"," ", new_data$frn_unique_identifier_line_item)
names(new_data)

updated_data <- left_join(test3,new_data[,c(1,2,3)], by = "frn_unique_identifier_line_item")
dim(updated_data)
updated_data <- updated_data[!duplicated(updated_data$row_id),] #to get rid of duplicates
updated_data$invoicing_method_relationship
updated_data$cost_per_monthly_residuals_scaled

#Multiwindow particpitatnt
multiwindow <- read_csv("C:/Users/dkim/Downloads/20220830_Multi Window Participants_v03.csv")
dim(multiwindow)
multiwindow <- clean_names(multiwindow)
multiwindow$unique_identifier
colnames(multiwindow)[colnames(multiwindow) == "unique_identifier"] <- "frn_unique_identifier_line_item"
multiwindow$frn_unique_identifier_line_item
str(multiwindow$frn_unique_identifier_line_item)
multiwindow$frn_unique_identifier_line_item <- gsub("_"," ", multiwindow$frn_unique_identifier_line_item)
names(multiwindow)

updated_data_2 <- left_join(updated_data,multiwindow[,c(2,3,4)], by = "frn_unique_identifier_line_item")
dim(updated_data_2)
updated_data_2 <- updated_data_2[!duplicated(updated_data_2$row_id),] #to get rid of duplicates
updated_data_2$multi_window_w_request

updated_data_2$invoicing_method_relationship <- replace_na(updated_data_2$invoicing_method_relationship, 0)
updated_data_2$multi_window_w_request <- replace_na(updated_data_2$multi_window_w_request,0)
updated_data_2$multi_window_w_unmet_need <- replace_na(updated_data_2$multi_window_w_unmet_need ,0)

updated_data_2$invoicing_method_relationship
updated_data_2$multi_window_w_request
updated_data_2$multi_window_w_unmet_need

#creating the esf variable
updated_data_2$ECF_ESSER_Overlap
updated_data_2$ESF_all_overlap <- updated_data_2$ECF_GEER_Overlap + updated_data_2$ECF_ESSER_Overlap
sum(updated_data_2$ESF_all_overlap == 2)
updated_data_2$ESF_all_overlap <- replace(updated_data_2$ESF_all_overlap,updated_data_2$ESF_all_overlap == 2 ,1)
updated_data_2$ESF_all_overlap <- as.factor(updated_data_2$ESF_all_overlap)
str(updated_data_2$ESF_all_overlap)

###Adding in the random_forest_result
random_forest_result_data <- read_csv("C:/Users/dkim/Downloads/20220916_Random_Forest_Results_v02.csv")
colnames(random_forest_result_data)
random_forest_result_data$frn_unique_identifier_line_item
which(colnames(random_forest_result_data) %in% "frn_unique_identifier_line_item")
random_forest_result_data_simple <- random_forest_result_data[,c(137,240,241,242)]

updated_data_3 <- left_join(updated_data_2, random_forest_result_data_simple, by = "frn_unique_identifier_line_item")
dim(updated_data_3)
updated_data_3 <- updated_data_3[!duplicated(updated_data_3$row_id),]

updated_data_3$service_type
updated_data_3
sum(is.na(updated_data_3$total_student_count))
sum(updated_data_3$total_student_count == 0, na.rm = TRUE)
#new variable: is frn authorized disbursment greater than 0
sum(updated_data_3$frn_authorized_disbursement > 0)
updated_data_3$frn_authorized_disbursement_Yes_No <- ifelse(updated_data_3$frn_authorized_disbursement > 0, 1,0)
sum(updated_data_3$frn_authorized_disbursement_Yes_No == 1)
######Part 4: Create that new dataset
names(updated_data_3)
why <- updated_data_3[,c(41,164,182,183,184,185,186,187,188,189,191,192,193,194,83)]

risk_index_data <- updated_data_3[,c(41,164,181,182,183,184,185,186,187,188,191,192,193,194,83,197,7,15,50,44,66,88,89,90,91,92,93,195,196,6)]

dim(risk_index_data)
colnames(risk_index_data)
str(risk_index_data)
risk_index_data$service_type

risk_index_data$ESF_all_overlap <- ifelse(risk_index_data$filing_window %in% "ECF Window 1" & risk_index_data$ESF_all_overlap == 1, 1,0) #this is to make sure that we only count esf values in window 1 if there was a program cross over. theres 255 lines
risk_index_data$ESF_all_overlap <- as.numeric(risk_index_data$ESF_all_overlap) #add service nslp difference
risk_index_data$ESF_all_overlap <- replace(risk_index_data$ESF_all_overlap, risk_index_data$ESF_all_overlap %in% 2, 1)
risk_index_data$ESF_all_overlap <- replace(risk_index_data$ESF_all_overlap, risk_index_data$ESF_all_overlap %in% 1, 0)

risk_index_data[sapply(risk_index_data, is.factor)] <- lapply(risk_index_data[sapply(risk_index_data, is.factor)], as.numeric)

#####Test to see the if else statement #success
risk_index_data_test <- risk_index_data

risk_index_data$NSLP_variance_device_scaled <- ifelse(risk_index_data$service_type %in% "Services", NA, risk_index_data$NSLP_variance_device_scaled)
risk_index_data$NSLP_variance_service_scaled <- ifelse(risk_index_data$service_type %in% "Equipment",NA, risk_index_data$NSLP_variance_service_scaled)

risk_index_data$cost_per_unit_residuals_scaled <- ifelse(risk_index_data$service_type %in% "Services",NA, risk_index_data$cost_per_unit_residuals_scaled)
risk_index_data$cost_per_monthly_residuals_scaled <- ifelse(risk_index_data$service_type %in% "Equipment",NA, risk_index_data$cost_per_monthly_residuals_scaled)

risk_index_data$unmet_need_student_ratio_device_scaled <- ifelse(risk_index_data$service_type %in% "Services",NA,risk_index_data$unmet_need_student_ratio_device_scaled)
risk_index_data$unmet_need_student_ratio_service_scaled <- ifelse(risk_index_data$service_type %in% "Equipment", NA, risk_index_data$unmet_need_student_ratio_service_scaled)


risk_index_data$service_type
view(risk_index_data)
####

#indexVars = colnames(risk_index_data)[3:length(colnames(risk_index_data))]
result <- rowSums(risk_index_data[,indexVars])
hist(result)
str(risk_index_data)

dfout <- risk_index_data[,c(3:15)]

indexFun0 = function(df, values=rep(1, 15)) {
    dfout = df
    vales_scaled = values/sum(values)
    for (i in 1:length(values)){
      df[is.na(df[,i]),i] <- 0
      df[,i] = df[,i]/(var(df[,i], na.rm = TRUE)^0.5)
  #    df[is.na(df[,i]),i] <- mean(is.na(df[,i]), na.rm = TRUE) #Use this when no bottom coding
      dfout[,i]  = df[,i] * vales_scaled[i]
    }
    riskIndex = rowSums(dfout)
    riskIndex = riskIndex/max(riskIndex)
    return (riskIndex)
}

#understanding how the function works
#This is to combat imbalance in variance between continous variables and binary variables
sort(risk_index_data$total_funding_residuals_scaled/(var(risk_index_data$total_funding_residuals_scaled, na.rm = TRUE)^0.5),decreasing = TRUE)

rep(1:13)/sum(rep(1:13)) #this is the normalized variables

rep(1:13)/sum(rep(1:13)) * risk_index_data$total_funding_residuals_scaled/(var(risk_index_data$total_funding_residuals_scaled, na.rm = TRUE)^0.5)

#riskIndex = indexFun0(risk_index_data[,c(3:15)], rep(1, 15)) #replace rep 1, 13 with scales so that we can weigh the columns
riskIndex = indexFun0(risk_index_data[,c(3:15)], scales)
sum(riskIndex > 0.99) 
#add in random forest results and applicant types that had positive impact
16+14+13+12+11+10+9+6+9
(scales/sum(scales))*100 #explanation
sum(scales)

scales = c('NSLP_variance_device_scaled' = 1,
           'NSLP_variance_service_scaled' = 1 ,
           'total_funding_residuals_scaled' =  10,
           'cost_per_student_residuals_scaled' = 8,
           'cost_per_unit_residuals_scaled' = 9,
           'cost_per_monthly_residuals_scaled' = 9,
           'unmet_need_student_ratio_device_scaled' = 2,
           'unmet_need_student_ratio_service_scaled' = 2, #replace the weighting with numbers that matches to the percentage from below (example, to make nslp 2% write down 2 on the weights)
           'multi_window_w_request' = 6,
           'multi_window_w_unmet_need' = 4,
           'ESF_all_overlap' = 5,
           'Predicted_Flagged_FRN_Scaled' = 7,
           'Consultant_Yes_No' = 3)


scales = c('NSLP_variance_device_scaled' = 1.5, #use this
           'NSLP_variance_service_scaled' = 1.5,
           'total_funding_residuals_scaled' =  15,
           'cost_per_student_residuals_scaled' = 12,
           'cost_per_unit_residuals_scaled' = 13.4,
           'cost_per_monthly_residuals_scaled' = 13.4,
           'unmet_need_student_ratio_device_scaled' = 3,
           'unmet_need_student_ratio_service_scaled' = 3, #replace the weighting with numbers that matches to the percentage from below (example, to make nslp 2% write down 2 on the weights)
           'multi_window_w_request' = 9,
           'multi_window_w_unmet_need' = 6,
           'ESF_all_overlap' = 7.5,
           'Predicted_Flagged_FRN_Scaled' = 10.4,
           'Consultant_Yes_No' = 4.5)
ifelse(risk_index_data[,"cost_per_unit_residuals_scaled"] & risk_index_data[,"service_type"] %in% "Equipment",1,0)

#Service and device thing
indexFun2 = function(df, values=rep(1, 15)) {
  dfout = df
  vales_scaled = values/sum(values)
  for (i in 1:length(values)){
    df[is.na(df[,i]),i] <- 0
    df[,i] = df[,i]/(var(df[,i], na.rm = TRUE)^0.5)
    ifelse(df[,"cost_per_unit_residuals_scaled"] & df[,"service_type"] %in% "Equipment" & ,)
    dfout[,i]  = df[,i] * vales_scaled[i]
  }
  riskIndex = rowSums(dfout)
  riskIndex = riskIndex/max(riskIndex)
  return (riskIndex)
}

#for devices use only NSLP_variance_device_scaled, unmet_need_student_ratio_device_scaled, 
ifelse(risk_index_data[,"cost_per_unit_residuals_scaled"] & risk_index_data[,"service_type"] %in% "Equipment" &  risk_index_data[,"NSLP_variance_device_scaled"] & risk_index_data[,"unmet_need_student_ratio_device_scaled"],)


'total_funding_residuals_scaled' =  15,
'cost_per_unit_residuals_scaled' = 13.4,
'cost_per_monthly_residuals_scaled' = 13.4,
'cost_per_student_residuals_scaled' = 12,
'Predicted_Flagged_FRN_Scaled' = 10.4,
'multi_window_w_request' = 9,
'ESF_all_overlap' = 7.5,
'multi_window_w_unmet_need' = 6,
'Consultant_Yes_No' = 4.5
'unmet_need_student_ratio_device_scaled' = 3,
'unmet_need_student_ratio_service_scaled' = 3,
'NSLP_variance_device_scaled' = 1.5,
'NSLP_variance_service_scaled' = 1.5,

1.5+1.5+15+12+13+13+3+3+9+6+7+10+4

var(risk_index_data$cost_per_student_residuals_scaled, na.rm = TRUE)
var(risk_index_data$Consultant_Yes_No)


# var(xA + yB) = x**2 * var(A) + y**2 * var(B) +2*x*y* cov (A,B)

par(mar = c(1, 1, 1, 1))
hist(20 + 60*riskIndex, breaks = 100) #main tool to focus on

risk_index_data$final_risk_score <- 20 + 60*riskIndex
summary(risk_index_data$final_risk_score)
summary(20 + 60*riskIndex)
sort(risk_index_data$final_risk_score, decreasing = TRUE)
sort(riskIndex, decreasing = TRUE)

mean(riskIndex > .4)

risk_index_data$ESF_all_overlap <- as.numeric(risk_index_data$ESF_all_overlap)


summary(risk_index_data$final_risk_score)
#########Finally, make the dataset

##updated 9/15/2022
colnames(is.na(risk_index_data))
only_necssary_risk_index <- risk_index_data[,c(1,2,19,20,21,22,23,24,25,26,27,28,29,32,33)]
write_csv(only_necssary_risk_index, "C:/Users/dkim/Downloads/20220915_risk_index_rough_draft_v07.csv")

write_csv(risk_index_data, "C:/Users/dkim/Downloads/20220916_all_risk_index_data_v04.csv")
str(risk_index_data)
colnames(risk_index_data)
risk_index_data[is.na(risk_index_data)] <- 0
str(risk_index_data)
sum(risk_index_data$cost_per_monthly_residuals_scaled == 0.5, na.rm = TRUE)
sum(risk_index_data$cost_per_unit_residuals_scaled == 0.5, na.rm = TRUE)

new_risk_index_data <- risk_index_data[,c(1,2,16,17,18,19,20,21,22,23,24,25,26,27,29)]
colnames(new_risk_index_data)

risk_inded_data_only_components <- risk_index_data[,c(1,2,29,3,4,5,6,7,8,9,10,11,12,13,14,15)]
str(risk_inded_data_only_components)
write_csv(risk_inded_data_only_components, "C:/Users/dkim/Downloads/20220914_risk_index_components.csv")


write_csv(new_risk_index_data, "C:/Users/dkim/Downloads/20220914_risk_index_rough_draft_v06.csv")


#For unmet need service or service residual, replace NA with the mean of the value (mean imputation) (only if we're not bottom coding)

##now lets find the weighting


sum(risk_index_data$ESF_all_overlap == 2)

dim(risk_index_data)
str(risk_index_data)
write_csv(risk_index_data, "C:/Users/dkim/Downloads/20220913_Risk_Index_all.csv")

install.packages("writexl")
library(writexl)
write_xlsx(risk_index_data, "C:/Users/dkim/Downloads/20220913_Risk_Index_all_v03.xlsx")
write_excel_csv(risk_index_data, "C:/Users/dkim/Downloads/20220913_Risk_Index_all_v02.xlsx")
#For binary variables like ECF Overlap, Consultant Y_N, Applicant Subtype we can just say 1/0. When we add the weights that's when we'll know its true impact

hmm <- filter(funded_data, flagged_frn %in% 1)

table(hmm$applicant_all_type) #private district has the most, 29%. Private School is 2nd, 26%. Charter school is 3rd, 24%. Public district is 4th, 10%.
table(hmm$applicant_all_type)/sum(table(hmm$applicant_all_type))



send <- read_csv("C:/Users/dkim/Downloads/20220829_ECF_Dataset_main_modified_v2.csv")
send <- filter(send, funding_request_status %in% c("Funded","Pending") & product_type %in% c("Laptops","Modem/Router combined","Modems","Routers","Tablets","Wi-Fi hotspots",NA) & connection_type %in% c(NA,"Cable Modem","Datacasting Customer Premises Equipment","DSL","Installation, Activation, and Initial Configuration","Leased Lit Fiber","Lit Fiber Network (Construction)","Maintenance and Operation","Microwave","Mobile Broadband","Network Equipment","Other","Private Line Circuits","Satellite","Wireless Network (Self-Provisioning)","Wireless Network (Third-Party Construction)"))
dim(send)

send$line_item_unique_identifier <- paste(send$funding_request_number_frn, send$frn_line_item_id)
send$line_item_unique_identifier

write_csv(send, "C:/Users/dkim/Downloads/20220906_funded_pending_dataset.csv")


sum(test3$flagged_frn== 1)

write_csv(test_send, "C:/Users/dkim/Downloads/20220915_Random_Forest_Line_Item_Data_Level_new.csv")

dim(test_send)
test_send
test_send$frn_unique_identifier_line_item
test_send$frn_line_item_id
test_send$frn



names(updated_data_3)
why <- updated_data_3[,c(41,164,178,179,180,181,182,183,184,185,186,187,188,189,190,191,83,194,7,15,50,44,66,88,89,90,91,92,93,192,193,6,21)]
write_csv(why,"C:/Users/dkim/Downloads/check_it_out.csv")
