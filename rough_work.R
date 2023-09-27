#############################################################
## Script for troubleshooting - bug finding and copy / pasting etc etc
## dobeirne@mail.ubc.ca - Feb 2023
#############################################################
library(stringr)
#Load df and dft scri[ts to see hwere the difference is

load("./data/dataframe_C_F.Rdata")
load("./data/dataframe.Rdata")
df_og$area_forest == dft$area_forest
df_og <- df %>% mutate(area_forest = ifelse(area_forest == 0, area_forest, area_forest/(10/3))) 
df_og$comp<- paste0(df_og$year, df_og$area_forest, df_og$ADM3_PCODE)
dft$comp<- paste0(dft$year, dft$area_forest, dft$ADM3_PCODE)
dft$area_forest <- as.numeric(dft$area_forest)
df_og$area_forest <- as.numeric(df_og$area_forest)
df_og$comp <- df_og$area_forest - dft$area_forest

# This is here because the 1995 forest_change variable is non-zero (it's 95-94 bcause data was subsetted after the area_forest var was created)
dft <- dft %>% mutate(forest_change = ifelse(year==1995, 0, forest_change))1995



vil_pop_1 <- read_excel("./data/population/th_pop_2001.xlsx") %>%
  as.data.frame() 

#Count the number of villages in each tmabon-amphoe-province combintation
vil_pop_1 <- vil_pop_1 %>% filter(ADM4_PCODE != 0 | ADM1_PCODE == 10) %>%
                           filter(ADM3_PCODE != 0)

vil_pop_1$ADM3_PCODE <- str_sub(vil_pop_1$ADM3_PCODE, end = -3)
vil_pop_1$ADM3_PCODE <- paste0('TH', vil_pop_1$ADM3_PCODE)

vil_count_alt <- rename(count(vil_pop_1, ADM3_PCODE), Freq = n) 

vil_pop_1 %>% filter(ADM3_PCODE=='TH110108')

vil_count_alt <- as.data.frame(vil_count_alt)

dft_n <- left_join(dft, vil_count_alt, by="ADM3_PCODE")

unique(dft_n$ADM1_EN)
length(unique(test$ADM3_PCODE))

n_occur <- data.frame(table(test$ADM3_PCODE))
n_occur[n_occur$Freq > 1,]
View(test[test$ADM3_PCODE %in% n_occur$Var1[n_occur$Freq > 1],])



#Test Cleaning 02/03/22
test <- pop_data[[1]] %>% group_by(ADM3_PCODE) #group data by Tambon

View(pop_data[[1]])
count <- test %>% count(ADM3_PCODE) 





test <- pop_data[[1]] %>% filter(ADM1_PCODE != 10)
test <- test %>% filter(ADM4_PCODE==0 & Tambon != "-")


n_occur <- data.frame(table(test$ADM3_PCODE))      #Count up number of occurences of each amd3_pcode
n_occur[n_occur$Freq > 1,]                         #Filter for just the ones that are >1 (none of them should be >1 )
View(test[test$ADM3_PCODE %in% n_occur$Var1[n_occur$Freq > 2],])

vil_count_alt <- rename(count(pop_data[[7]], ADM3_PCODE), Freq = n)  # count up occurrences of a particular ADM3_PCODE to get villages per Tambon in 2001 (treatment year)



pop_data_all_years %>%  distinct(ADM3_PCODE, year, .keep_all=TRUE)

pop_data_all_years %>% 
  group_by(ADM3_PCODE, year) %>% 
  mutate(dupe = n()>1)%>%
  filter(dupe==T)

########################################################################################################################################################
####################################################### Village Count and Population ###################################################################
########################################################################################################################################################
dft_n$year <- as.numeric(dft_n$year) #For some reason dft_n was class character and it should be numeric

pop <- list.files(path="./data/population", pattern = ".xlsx")
pop_data<- list()
for(i in 1:length(pop)){
  pop_data[[i]] <- as.data.frame(read_excel(paste0('./data/population/',pop[[i]])))
}


pop_data[[5]] <-  pop_data[[5]] %>%             #1999 was duplicated 3 times in the dataset, removing dupliactes solves issue
                     distinct(.keep_all = TRUE)
year<-1995
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
                            tamb_pop[[11]],tamb_pop[[12]],tamb_pop[[13]])%>% 
                            distinct(ADM3_PCODE, year, .keep_all=TRUE)
## Save these Files
save(tamb_pop, file="./data/pop_data.Rdata")
save(pop_data_all_years, file="./data/pop_data_all_years.Rdata")
save(tamb_vil_count, file="./data/vil_data.Rdata")



########################################################################################################################################################
####################################################### Differences in my data ###################################################################
########################################################################################################################################################

load("./data/dataframe_C_F.Rdata")
df_og <- dft
load("./data/dataframe_p_c_f.Rdata")
df_n <- dft

df_og%>%group_by(year)%>% 
  summarise(ttl_crop = sum(area_crop), ttl_forest = sum(area_forest)) %>% 
  ggplot(aes(x=year, y=ttl_forest, group=1))+
  geom_line()

df_n%>%group_by(year)%>% 
  summarise(ttl_crop = sum(area_crop), ttl_forest = sum(area_forest)) %>% 
  ggplot(aes(x=year, y=ttl_forest, group=1))+
  geom_line()

table(dft$year)


load("./data/inter_df_fc.Rdata")
load("./data/inter_df_cc.Rdata")


######################################
######## Forest Change
filter(df, year==1996)%>%filter(forest_change >0)


##########################################
#### Looking at population data - why weird?
df <- dft
# ttl Population Cover Over Time
pplot <- df%>%
  filter(p_forest_baseline >.2)%>%
  group_by(year)%>% 
  summarise(ttl_pop = sum(ttl_pop), ttl_forest = sum(area_forest)) %>% 
  ggplot(aes(x=year))+
  geom_line(aes(y=ttl_pop))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

df %>% group_by(year)%>%
summarise(n_districts = n_distinct(ADM3_PCODE)) 

summarise(max=max(pop_change, na.rm = T))

df %>% filter(p_forest_baseline >.2, year!=1995) %>% mutate(pop_change = ttl_pop-lag(ttl_pop)) %>% filter(pop_change> 1000) %>% count(year)

df %>%
  mutate(ttl_pop = ifelse(year == 1994, (lag(ttl_pop) + lead(ttl_pop))/2, ttl_pop)) %>%
  mutate(ttl_pop = ifelse(year == 2008, (lag(ttl_pop) + lead(ttl_pop))/2, ttl_pop)) %>%
  group_by(year)%>% 
  summarise(ttl_pop = sum(ttl_pop), ttl_forest = sum(area_forest)) %>% 
  ggplot(aes(x=year))+
  geom_line(aes(y=ttl_pop))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  
  
  mutate(pop_dens = ttl_pop/area_km2)%>%
  filter(p_forest_baseline >.2)%>% 
  mutate(area_forest = winsorize(area_forest, threshold =.01))%>%
  feols(area_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE + p_forest_baseline, cluster=c('ADM3_PCODE')) 

df %>% group_by(year) %>% summarise(mean_change = mean(forest_change))


##########################################################################################

