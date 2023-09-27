#############################################################
## Join the ESA data with the Village Data, generate the dataset needed for analyssis (forest cover etc.)
## dobeirne@mail.ubc.ca - Feb 2023
#############################################################

########################################################################################################################################################
####################################################### Village Count and Population ###################################################################
########################################################################################################################################################
dft_n$year <- as.numeric(dft_n$year) #For some reason dft_n was class character and it should be numeric

pop <- list.files(path="./data/population", pattern = ".xlsx")
pop_data<- list()
for(i in 1:length(pop)){
  pop_data[[i]] <- as.data.frame(read_excel(paste0('./data/population/',pop[[i]])))
}


pop_data[[7]] <-  pop_data[[7]] %>%             #1999 was duplicated 3 times in the dataset, removing dupliactes solves issue
  distinct(.keep_all = TRUE)
pop_data[[2]] <-  pop_data[[7]] %>%             #1999 was duplicated 3 times in the dataset, removing dupliactes solves issue
  distinct(.keep_all = TRUE)
pop_data[[16]] <-  pop_data[[7]] %>%             #1999 was duplicated 3 times in the dataset, removing dupliactes solves issue
  distinct(.keep_all = TRUE)

year<-1993
for(i in 1:length(pop_data)){
  pop_data[[i]]$year <- year              
  year=year+1                             # Adds year infor to each row (useful for matching)
  pop_data[[i]]$ADM3_PCODE <- str_sub(pop_data[[i]]$ADM3_PCODE, end = -3) #Removes the 00 at the end of each post code
  pop_data[[i]]$ADM3_PCODE <- paste0('TH', pop_data[[i]]$ADM3_PCODE) #Adds TH to the postcode to match the other data
  pop_data[[i]] <- pop_data[[i]] %>% filter(ADM3_PCODE != "TH") #Removes non-Tambons that are same level as Tambon (ie sub-districts of Bangkok etc) bot used in this analysis
}


#Create list of lists for Vil_count and population
tamb_pop <- list()
tamb_vil_count <- list()


for(i in 1:length(pop_data)){
  #First thing is to get a count of the villages
  tamb_vil_count[[i]] <- pop_data[[i]] %>%
    filter(ADM4_PCODE!=0 | Village != "-") %>% #filter out the summary rows keep only rows where there is a village name
    group_by(ADM3_PCODE)%>%                        #group and the Tambon level
    summarise(village_count = n_distinct(Village))   #count unique occurences of village names within each Tambon ID
}

for(i in 1:length(pop_data)){
  #Next thing is to get a count of the populations in each Tambon
  #Get rid of 00 at the end of the Tambon 
  tamb_pop[[i]] <- pop_data[[i]] %>% 
    filter(ADM3_PCODE!=0 & ADM4_PCODE == 0 ) %>% #filter so that only the Tambon summary's are present
    group_by(ADM3_PCODE)%>%                    # group them at the Tambon PCODE level so the you can sum the duplicates then delete them
    summarise(ttl_pop = sum(ttl_pop), Households = sum(Households), year=year )
}


pop_data_all_years <- rbind(tamb_pop[[1]],tamb_pop[[2]],tamb_pop[[3]],tamb_pop[[4]],tamb_pop[[5]],
                            tamb_pop[[6]],tamb_pop[[7]],tamb_pop[[8]],tamb_pop[[9]],tamb_pop[[10]],
                            tamb_pop[[11]],tamb_pop[[12]],tamb_pop[[13]],tamb_pop[[14]],tamb_pop[[15]],tamb_pop[[16]],
                            tamb_pop[[17]],tamb_pop[[18]],tamb_pop[[19]],tamb_pop[[20]])%>% 
  distinct(ADM3_PCODE, year, .keep_all=TRUE)

#There are some ADM3PCODEs that are note present in every year - remove these
table(pop_data_all_years$year)
pop_data_all_years<-  pop_data_all_years %>%
                        group_by(ADM3_PCODE) %>%
                        filter(all(1993:2012 %in% year))
table(pop_data_all_years$year)


## Save these Files
save(tamb_pop, file="./data/pop_data.Rdata")
save(pop_data_all_years, file="./data/pop_data_all_years.Rdata")
save(tamb_vil_count, file="./data/vil_data.Rdata")
