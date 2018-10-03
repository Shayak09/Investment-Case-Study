setwd("F:/Data Science/Investment Case Group Project") # setting file directory

companies<-read.delim("companies.txt", header = TRUE) # loading companies.txt file to comapnies dataFrame
rounds2<- read.csv("rounds2.csv") #loading rounds2.csv file to rounds2 dataFrame

install.packages("dplyr") #Downloading binary package
library("dplyr") 

#Ex: 1.1 Sl.No 1

rounds2$company_permalink<-toupper(rounds2$company_permalink) # coverting all permalinks to Uppercase for sorting
nrow(distinct(rounds2, company_permalink)) # to get count of all distinct values in company_permalink column

#Ex: 1.1 Sl.No 2

companies$permalink<-toupper(companies$permalink) #converting all permalinks of companies to uppercase
nrow(distinct(companies, permalink)) #to get count of all distinct values in permalinks

#Ex: 1.1 Sl.No 3

permalink

#Ex: 1.1 Sl.No 4

companies_links<-!(distinct(rounds2, company_permalink) %in% distinct(companies, permalink))
length(which(companies_links==TRUE))

#Ex: 1.1 Sl.No 5

companies$permalink<-toupper(companies$permalink)  #converting all to uppercase
rounds2$company_permalink<-toupper(rounds2$company_permalink)   #converting all to uppercase
master_frame<-merge(rounds2, companies, by.x ="company_permalink", by.y = "permalink") #merging dataframes base on permalinks
nrow(master_frame)  #counting observations

#Ex: 2.1

groupBy_FundingType<- group_by(master_frame, funding_round_type)  #grouping by funding type
FundingType_Average<- summarise(groupBy_FundingType, mean(raised_amount_usd, na.rm = TRUE))  # df based on mean of invest amount

#Ex: 2.1 Sl.No 1

FundingType_Average[FundingType_Average$funding_round_type=="venture",] # average values for venture funding amount

#Ex: 2.1 Sl.No 2

FundingType_Average[FundingType_Average$funding_round_type=="angel",] # average values for angel funding amount

#Ex: 2.1 Sl.No 3

FundingType_Average[FundingType_Average$funding_round_type=="seed",] # average values for seed funding amount

#Ex: 2.1 Sl.No 4

FundingType_Average[FundingType_Average$funding_round_type=="private_equity",] # average values for private equity funding amount


#Ex: 2.1 Sl.No 5

colnames(FundingType_Average)<-c("funding_round_type", "Avg_Funding")
FundingType_Average_desc<-arrange(filter(FundingType_Average, Avg_Funding>=5000000 & Avg_Funding<=15000000), desc(Avg_Funding)) #creating df for the given range in decreasing order
FundingType_Average_desc[which.max(FundingType_Average_desc$Avg_Funding),] # retrieving max value

#Checkpoint 3: Country Analysis

#As we now know Venture is the investment type best suited for Sparks Funds

venture_frame<- filter(master_frame, master_frame$funding_round_type=="venture" & master_frame$country_code !="")  #created a new subset  #removing out NA and blank values from countryCodes
group_by_country<-(group_by(venture_frame, country_code) %>% summarise_at(vars(raised_amount_usd), sum, na.rm=TRUE))  # grouping countries based on aggregate of funds
top9_countries<-top_n(group_by_country, n=9, group_by_country$raised_amount_usd) #filtered top # countries based on raised funds, these are not in any order as of now
top9<- arrange(top9_countries, desc(top9_countries$raised_amount_usd)) #arrange those 9 countries in descending order based on amount raised
#As you can see, CHN(China)is not in English speaking list, we can manually ignore it.

#Hence top 3 English speaking countries are:1> USA, 2> GBR, 3>IND


#Checkpoint 4: Sector Analysis 1

install.packages("tidyr")
library("tidyr") # installing packages

mapping<-read.csv("mapping.csv", header = TRUE) #load mapping.csv to dataFrame
mapping$Blanks<-NULL #remove Blanks column from mapping dataFrame
mapping_new<-gather(mapping, key="Category_name", value="Category_value", 2:9, na.rm = TRUE)  # converting wide to long format
mapping_sector<- subset(mapping_new, mapping_new$Category_value==1) #filtering down to values having only specific category and subcategory
mapping_sector$Category_value<-NULL # final trimmed and cleaned mapping sheet relating the category and sub-category

mapping_sector$category_list<-gsub("0","na", mapping_sector$category_list) #replacing all 0 by na
mapping_sector$category_list<-toupper(mapping_sector$category_list) #making category_list as upperCase

install.packages("stringr") 
library("stringr") #install packages

master_frame$Primary_Sector<-str_split(master_frame$category_list, pattern = "\\|", simplify = TRUE)[,1]  #extracting primary sector of each category list
master_frame$category_list<-NULL    #removing category_list column
master_frame$Primary_Sector<-toupper(master_frame$Primary_Sector) # uppercasing the primary sector
master_frame_sector<-merge(master_frame, mapping_sector, by.x = "Primary_Sector", by.y="category_list")  # merging the two files base on category list


#Checkpoint 5: Sector Analysis 2

D1<-filter(master_frame_sector, country_code=="USA", funding_round_type=="venture", raised_amount_usd>=5000000 & raised_amount_usd<=15000000)  #df for USA

D1_groupby<-group_by(D1, Category_name)  #grouping D1 by Category name
D1_count<- summarise(D1_groupby, length(Category_name))  #getting length for the above
colnames(D1_count)<-c("Category_name", "No of Investment")
D1_sum<- summarise(D1_groupby, sum(raised_amount_usd, na.rm = TRUE))  #getting sum for the above
colnames(D1_sum)<-c("Category_name", "Total amount Invested")
D1<-merge(D1, D1_count, by="Category_name")
D1<- merge(D1, D1_sum, by="Category_name")  #final df for USA based on predecsribed stipulations

D2<-filter(master_frame_sector, country_code=="GBR", funding_round_type=="venture", raised_amount_usd>=5000000 & raised_amount_usd<=15000000) #df for GBR
D2_groupby<-group_by(D2, Category_name)  #grouping D2 by Category name
D2_count<- summarise(D2_groupby, length(Category_name))  #getting length for the above
colnames(D2_count)<-c("Category_name", "No of Investment")
D2_sum<- summarise(D2_groupby, sum(raised_amount_usd, na.rm = TRUE))  #getting sum for the above
colnames(D2_sum)<-c("Category_name", "Total amount Invested")
D2<-merge(D2, D2_count, by="Category_name")
D2<- merge(D2, D2_sum, by="Category_name")  #final df for GBR based on predecsribed stipulations

D3<-filter(master_frame_sector, country_code=="IND", funding_round_type=="venture", raised_amount_usd>=5000000 & raised_amount_usd<=15000000) #df for IND
D3_groupby<-group_by(D3, Category_name)  #grouping D3 by Category name
D3_count<- summarise(D3_groupby, length(Category_name))  #getting length for the above
colnames(D3_count)<-c("Category_name", "No of Investment")
D3_sum<- summarise(D3_groupby, sum(raised_amount_usd, na.rm = TRUE))  #getting sum for the above
colnames(D3_sum)<-c("Category_name", "Total amount Invested")
D3<-merge(D3, D3_count, by="Category_name")
D3<- merge(D3, D3_sum, by="Category_name")  #final df for IND based on predecsribed stipulations


master_frame_sector_tableau<-filter(master_frame_sector, funding_round_type=="venture", raised_amount_usd>=5000000 & raised_amount_usd<=15000000) #df for tableau plot3


#Sl.No 1

nrow(D1) #total number of USA investments
nrow(D2) #total number of GBR investments
nrow(D3) #total number of IND investments

#Sl.No 2

sum(D1$raised_amount_usd) #total USA investment
sum(D2$raised_amount_usd) #total GBR investment
sum(D3$raised_amount_usd) #total IND investment

#Sl.No 3,4,5
#for D1



D1_count<-arrange(D1_count, desc(D1_count$`No of Investment`)) #arranging in descending order of no of investment


#Top Sector is Others But neglecting it as not able to find the particular Sector and hence taking the next sector as Top sector and susequent sectors as next highest sectors
D1_count[2,1]  # selecting top sector
D1_count[3,1]  # selecting 2nd top sector
D1_count[4,1]  # selecting 3rd top sector

#for D2

D2_count<-arrange(D2_count, desc(D2_count$`No of Investment`)) #arranging in descending order of no of investment
#Top Sector is Others But neglecting it as not able to find the particular Sector and hence taking the next sector as Top sector and susequent sectors as next highest sectors
D2_count[2,1] #selecting top sector
D2_count[3,1] #selecting 2nd top sector
D2_count[4,1] #selecting 3rd top sector

#for D3

D3_count<-arrange(D3_count, desc(D3_count$`No of Investment`)) #arranging in descending order of no of investment
#Top Sector is Others But neglecting it as not able to find the particular Sector and hence taking the next sector as Top sector and susequent sectors as next highest sectors
D3_count[2,1] #selecting top sector
D3_count[3,1] #selecting 2nd top sector
D3_count[4,1] #selecting 3rd top sector

#Sl.No. 6,7,8

D1_count[2,2] #no of investment in top sector
D1_count[3,2] #no of investment in 2nd top sector
D1_count[4,2] #no of investment in 3rd top sector

D2_count[2,2] #no of investment in top sector
D2_count[3,2] #no of investment in 2nd top sector
D2_count[4,2] #no of investment in 3rd top sector

D3_count[2,2] #no of investment in top sector
D3_count[3,2] #no of investment in 2nd top sector
D3_count[4,2] #no of investment in 3rd top sector

#Sl.No 9  #For point 3 (top sector count-wise), which company received the highest investment? 

D1_subset<-subset(D1, D1$Category_name=="Social..Finance..Analytics..Advertising")  #created a df with only top sector count-wise
D1_subset_group<- group_by(D1_subset, company_permalink) %>% summarise_at(vars(raised_amount_usd), sum, na.rm=TRUE) #creating df with distinct companies and total amount raised
D1_subset_group<-arrange(D1_subset_group, desc(raised_amount_usd)) #arranging in decreasing order of raised amount
D1_subset_group[1,1] #company with highest investment

D2_subset<-subset(D2, D2$Category_name=="Social..Finance..Analytics..Advertising")  #created a df with only top sector count-wise
D2_subset_group<- group_by(D2_subset, company_permalink) %>% summarise_at(vars(raised_amount_usd), sum, na.rm=TRUE) #creating df with distinct companies and total amount raised
D2_subset_group<-arrange(D2_subset_group, desc(raised_amount_usd)) #arranging in decreasing order of raised amount
D2_subset_group[1,1] #company with highest investment

D3_subset<-subset(D3, D3$Category_name=="Social..Finance..Analytics..Advertising")  #created a df with only top sector count-wise
D3_subset_group<- group_by(D3_subset, company_permalink) %>% summarise_at(vars(raised_amount_usd), sum, na.rm=TRUE) #creating df with distinct companies and total amount raised
D3_subset_group<-arrange(D3_subset_group, desc(raised_amount_usd)) #arranging in decreasing order of raised amount
D3_subset_group[1,1] #company with highest investment

#Sl. No 10  #For point 4 (second best sector count-wise), which company received the highest investment?

D1_subset_second<-subset(D1, D1$Category_name=="Cleantech...Semiconductors")  #created a df with only 2nd top sector count-wise
D1_subset_group_second<- group_by(D1_subset_second, company_permalink) %>% summarise_at(vars(raised_amount_usd), sum, na.rm=TRUE) #creating df with distinct companies and total amount raised
D1_subset_group_second<-arrange(D1_subset_group_second, desc(raised_amount_usd)) #arranging in decreasing order of raised amount
D1_subset_group_second[1,1] #company with highest investment


D2_subset_second<-subset(D2, D2$Category_name=="Cleantech...Semiconductors")  #created a df with only 2nd top sector count-wise
D2_subset_group_second<- group_by(D2_subset_second, company_permalink) %>% summarise_at(vars(raised_amount_usd), sum, na.rm=TRUE) #creating df with distinct companies and total amount raised
D2_subset_group_second<-arrange(D2_subset_group_second, desc(raised_amount_usd)) #arranging in decreasing order of raised amount
D2_subset_group_second[1,1] #company with highest investment


D3_subset_second<-subset(D3, D3$Category_name=="News..Search.and.Messaging")  #created a df with only 2nd top sector count-wise
D3_subset_group_second<- group_by(D3_subset_second, company_permalink) %>% summarise_at(vars(raised_amount_usd), sum, na.rm=TRUE) #creating df with distinct companies and total amount raised
D3_subset_group_second<-arrange(D3_subset_group_second, desc(raised_amount_usd)) #arranging in decreasing order of raised amount
D3_subset_group_second[1,1] #company with highest investment
