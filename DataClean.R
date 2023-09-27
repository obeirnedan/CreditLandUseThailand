#############################################################
## Join the ESA data with the Village Data, generate the dataset needed for analyssis (forest cover etc.)
## dobeirne@mail.ubc.ca - Feb 2023
#############################################################

##############################
# 0 - Load librairies
##############################

library(dplyr)
library(readxl)
library(raster)
library(sf)
library(units)
library(tidyverse)
############################## 
# 1 - Source file 
##############################

setwd("~/R/CreditLandUseThailand")
load("./data/LandCoverThailand/esa_thai_apr.Rdata")
############################## 
# 2 - Count number of Villages per Tambon
##############################


# Language to Thai
Sys.setlocale(locale = "Thai")
# 
# # Read in file of villages as dataframe select just the important variables
# vil_count <- read_excel("./data/village_names.xlsx") %>%
#   as.data.frame() %>%
#   dplyr::select(VillName, TamName,  AmpName, ProvName)
# 
# #Count the number of villages in each tmabon-amphoe-province combintation
# vil_count <- rename(count(vil_count, TamName, AmpName, ProvName), Freq = n) %>%
#   `colnames<-` (c("Tambon",'Amphoe', "Province", "village_count"))

load("./data/vil_data.Rdata") #load villlage count data from the registry office data called tamb_vil_count
load("./data/pop_data_all_years.Rdata") #Load the all the population data for every year in one large dataframe

vil_count_01 <- tamb_vil_count[[7]] #The village count using the 2001 population registry data - ie the number of villages in 2001 at start of program

plot(esa_thai[[1]])
############################## 
# 3 - Determine Forest Cover 
############################## 


# # List of names of rasters for each year
# rasternames <- c("thai_forestcover_1992",
#                  "thai_forestcover_1993",
#                  "thai_forestcover_1994",
#                  "thai_forestcover_1995",
#                  "thai_forestcover_1996",
#                  "thai_forestcover_1997",
#                  "thai_forestcover_1998",
#                  "thai_forestcover_1999",
#                  "thai_forestcover_2000",
#                  "thai_forestcover_2001",
#                  "thai_forestcover_2002",
#                  "thai_forestcover_2003",
#                  "thai_forestcover_2004",
#                  "thai_forestcover_2005",
#                  "thai_forestcover_2006",
#                  "thai_forestcover_2007",
#                  "thai_forestcover_2008",
#                  "thai_forestcover_2009",
#                  "thai_forestcover_2010",
#                  "thai_forestcover_2011",
#                  "thai_forestcover_2012",
#                  "thai_forestcover_2013",
#                  "thai_forestcover_2014")

# Read in the saved rasters
# nat_forest <- vector(mode = "list", length = length(rasternames)) #initialise variable name

nat_forest <- esa_thai
nat_cropland <- esa_thai
nat_urban <- esa_thai

nat_forest[[3]]
# Load each raster into list 
# for(i in 1:length(rasternames)){
#   nat_forest[[i]] <- raster(paste0('./data/forest_cover_thailand_92_14/',rasternames[[i]]))
# }

#Consolidate raster values into two - forest(1) and not-forest(0)
# Note this raster, downloaded from ESA website has unusual categorization of items - the below commented categorisation is what the file should look like if the value were correct (using file from Dennis's computer and the commented value-categories yields identical result)
# More restrictve and less restrictive ways of categorising forests
for(i in 1:length(nat_forest)){
  nat_forest[[i]][nat_forest[[i]] %in% c(0,10,11,12,20,30,50,60,62,70,
                                         72,80,82, 110,120,121,122,130,
                                         140,150,151,152,153,180,190,200,
                                         201,202,210,220)] <- 0

  nat_forest[[i]][nat_forest[[i]] %in% c(40,61,71,81,90, 100,160,170)] <- 1
  # 
  # nat_forest[[i]][nat_forest[[i]] %in% c(0,10,11,12,20,30,
  #                                         110,120,121,122,130,
  #                                        140,150,151,152,153,180,190,200,
  #                                        201,202,210,220)] <- 0
  # 
  # nat_forest[[i]][nat_forest[[i]] %in% c(40,50,60,61,62,70,71,72,80,81,82,90,100,160,170)] <- 1

}

for (i in 1:length(nat_cropland)){
  nat_cropland[[i]][nat_cropland[[i]] %in% c(40,50,60,61,62,70,71,72,80,81,82,90,100,160,170, 110,120,121,122,130,
                                             140,150,151,152,153,180,190,200,
                                             201,202,210,220)] <- 0
  
  nat_cropland[[i]][nat_cropland[[i]] %in% c(10,11,12,20,30)] <- 1
}

for(i in 1:length(nat_urban)){
  nat_urban[[i]][nat_urban[[i]] %in% c(0,10,11,12,20,30,40,50,60,61,62,70,71,72,80,81,82,90,100,160,170,
                                         110,120,121,122,130,
                                         140,150,151,152,153,180,200,
                                         201,202,210,220)] <- 0
  
  nat_urban[[i]][nat_urban[[i]] %in% c(190)] <- 1
  
}

#Take a look at what is now forest/not
plot(nat_forest[[10]])
plot(nat_cropland[[10]])
plot(nat_urban[[10]])

############################## 
# 4 - Get forest cover levels and change per Tambon
##############################


# Read in the polygons of admin borders
nat_border <- read_sf("./data/admin_boundary_shapes/tha_admbnda_adm0_rtsd_20220121.shp")
prov_border <- st_read("./data/admin_boundary_shapes/tha_admbnda_adm1_rtsd_20220121.shp")
dist_border <- st_read("./data/admin_boundary_shapes/tha_admbnda_adm2_rtsd_20220121.shp")
tamb_border <- st_read("./data/admin_boundary_shapes/tha_admbnda_adm3_rtsd_20220121.shp")

# Convert shape files to spatial 
admin_borders <- list(nat_border, prov_border, dist_border, tamb_border)  %>%
  lapply(as_Spatial)



# Initialise list for proportion of each Tambon that is forested / cropland for each year 1991 - 2014
area_cover <- vector("list", 23) #area forest
crop_cover <- vector("list", 23) #area cropland
urban_cover<- vector("list", 23) #area urban

# Extract raster values by Tambon and get sum forest (300x300m resolution pixels)
for(i in 1:length(area_cover)){
  area_cover[[i]] <- raster::extract(nat_forest[[i]], tamb_border['geometry'], fun=sum)
}
# Extract raster values by Tambon and get sum crop (300x300m resolution pixels)
for(i in 1:length(area_cover)){
  crop_cover[[i]] <- raster::extract(nat_cropland[[i]], tamb_border['geometry'], fun=sum)
}
# Extract raster values by Tambon and get sum crop (300x300m resolution pixels)
for(i in 1:length(urban_cover)){
  urban_cover[[i]] <- raster::extract(nat_urban[[i]], tamb_border['geometry'], fun=sum)
}

# change this vector into a dataframe, set collumn names forest cover
area_cover_df <- as.data.frame(area_cover)
colnames(area_cover_df) <- c('forest_cover_1992', "forest_cover_1993", "forest_cover_1994", "forest_cover_1995", "forest_cover_1996", 
                             "forest_cover_1997", "forest_cover_1998", "forest_cover_1999", 'forest_cover_2000', "forest_cover_2001",
                             "forest_cover_2002", "forest_cover_2003", "forest_cover_2004", "forest_cover_2005", "forest_cover_2006", 
                             "forest_cover_2007", "forest_cover_2008", "forest_cover_2009", "forest_cover_2010",        
                             "forest_cover_2011", "forest_cover_2012", "forest_cover_2013", "forest_cover_2014")

# change this vector into a dataframe, set collumn names crop cover
crop_cover_df <- as.data.frame(crop_cover)
colnames(crop_cover_df) <- c('crop_cover_1992', "crop_cover_1993", "crop_cover_1994", "crop_cover_1995", "crop_cover_1996", 
                             "crop_cover_1997", "crop_cover_1998", "crop_cover_1999", 'crop_cover_2000', "crop_cover_2001",
                             "crop_cover_2002", "crop_cover_2003", "crop_cover_2004", "crop_cover_2005", "crop_cover_2006", 
                             "crop_cover_2007", "crop_cover_2008", "crop_cover_2009", "crop_cover_2010",        
                             "crop_cover_2011", "crop_cover_2012", "crop_cover_2013", "crop_cover_2014")

# change this vector into a dataframe, set collumn names crop cover
urban_cover_df <- as.data.frame(urban_cover)
colnames(urban_cover_df) <- c('urban_cover_1992', "urban_cover_1993", "urban_cover_1994", "urban_cover_1995", "urban_cover_1996", 
                              "urban_cover_1997", "urban_cover_1998", "urban_cover_1999", 'urban_cover_2000', "urban_cover_2001",
                              "urban_cover_2002", "urban_cover_2003", "urban_cover_2004", "urban_cover_2005", "urban_cover_2006", 
                              "urban_cover_2007", "urban_cover_2008", "urban_cover_2009", "urban_cover_2010",        
                              "urban_cover_2011", "urban_cover_2012", "urban_cover_2013", "urban_cover_2014")


df <- cbind(area_cover_df, tamb_border)
crop_cover_df <- cbind(crop_cover_df, tamb_border['ADM3_PCODE'])
urban_cover_df <- cbind(urban_cover_df, tamb_border['ADM3_PCODE'])

#You Should save the crop and are cover dfs at this stage so you don't have to re-run the extrct function becuase it's suuuuper slow


# Create list of area of each row 
km2 <- (st_area(df[,'geometry'])) %>%
  units::set_units(., km^2)
# Add list as column in main data frame
df$area_km2 <- km2 

#Drop the units class from collumns
for (i in 1:length(colnames(df))){
  ifelse(class(df[[i]])=='units', df[[i]] <- drop_units(df[[i]]), 1)
}

df <- as.data.frame(df)
crop_cover_df <- as.data.frame(crop_cover_df)
urban_cover_df <- as.data.frame(urban_cover_df)
save(df, file= "./data/inter_data/inter_df_fc_r.Rdata")

save(crop_cover_df,file= "./data/inter_data/inter_df_cc.Rdata")
save(urban_cover_df,file= "./data/inter_data/inter_df_uc.Rdata")
##################################################################################################################################################
############################################# Try it With New Stuff ###################################################################
##################################################################################################################################################
# load("./data/inter_data/inter_df_fc.Rdata")
# load("./data/inter_data/inter_df_cc.Rdata")
# load('./data/inter_data/inter_df_uc.Rdata')

df <- inner_join(vil_count_01, df, by="ADM3_PCODE" )

crop_cover_df <-  inner_join(vil_count_01, crop_cover_df, by="ADM3_PCODE" )

urban_cover_df <-  inner_join(vil_count_01, urban_cover_df, by="ADM3_PCODE" )

dft <- df
crop_cover_dft <- crop_cover_df
urban_cover_dft <- urban_cover_df


dft$baseline_forest <- dft$forest_cover_1995 * .09
crop_cover_dft$baseline_crop <- crop_cover_dft$crop_cover_1995 * .09
crop_cover_dft$baseline_urban <- urban_cover_dft$urban_cover_1995 * .09


dft <- dft %>% 
  pivot_longer(                            #pivot to correct format for feols
    cols = starts_with("forest_cover"),
    names_to = "year",
    names_prefix = "forest_cover_",
    values_to = "area_forest",
    values_drop_na = T)%>%
 # subset(year %in%  c(1995:2007))%>%        #Reduce data set to correct number of years
  mutate(area_forest = area_forest*.09,     #multiply by .09 to get to km2 (300m^2 was resolution)
         treat = if_else(year > 2001, 1,0),
         p_forest = area_forest/area_km2,   #proportion of Tambon covered in forest
         p_forest_baseline = baseline_forest/area_km2)%>%            #proportion of Tambon in forest in 1995 - baseline period
  group_by(ADM3_PCODE)%>%
         mutate(forest_change = area_forest - lag(area_forest),
         ihs_area_forest = asinh(area_forest),
         ihs_forest_change = ihs_area_forest - lag(ihs_area_forest),
         vil_per_km2 = village_count/area_km2)%>%
         ungroup()%>%
         as.data.frame() 



crop_cover_dft <- crop_cover_dft %>% 
  pivot_longer(                            #pivot to correct format for feols
    cols = starts_with("crop_cover"),
    names_to = "year",
    names_prefix = "crop_cover_",
    values_to = "area_crop",
    values_drop_na = T)%>%
 # subset(year %in%  c(1995:2007))%>%        #Reduce data set to correct number of years ('92-'94 shows non-credible figures for forest cover)
  mutate(area_crop = area_crop*.09)%>%
  group_by(ADM3_PCODE)%>%
  mutate(crop_change= area_crop - lag(area_crop),
         ihs_area_crop = asinh(area_crop),
         ihs_crop_change = ihs_area_crop - lag(ihs_area_crop))%>%
  ungroup()%>%
  as.data.frame()


urban_cover_dft <- urban_cover_dft %>% 
  pivot_longer(                            #pivot to correct format for feols
    cols = starts_with("urban_cover"),
    names_to = "year",
    names_prefix = "urban_cover_",
    values_to = "area_urban",
    values_drop_na = T)%>%
 # subset(year %in%  c(1995:2007))%>%        #Reduce data set to correct number of years ('92-'94 shows non-credible figures for forest cover)
  mutate(area_urban = area_urban*.09)%>%
  group_by(ADM3_PCODE)%>%
  mutate(urban_change= area_urban - lag(area_urban),
         ihs_area_urban = asinh(area_urban),
         ihs_urban_change = ihs_area_urban - lag(ihs_area_urban))%>%
  ungroup()%>%
  as.data.frame()



all(dft$ADM3_PCODE == urban_cover_dft$ADM3_PCODE)
dft <- data.frame(cbind(dft, crop_cover_dft, urban_cover_dft)) # combine all three data sets on forest, crop, and urban into one 

dft <- dft %>% subset(select=-c(ADM3_REF, ADM3ALT1EN, ADM3ALT2EN, ADM3ALT1TH, ADM3ALT2TH, validTo, validOn, date, ADM3_PCODE.1,ADM3_PCODE.2,village_count.1,village_count.2,geometry.1,geometry.2,year.1,year.2))


#pop_data_all_years$ADM3_PCODE <- as.factor(pop_data_all_years$ADM3_PCODE)
#dft$ADM3_PCODE <- as.factor(dft$ADM3_PCODE)
#pop_data_all_years$year <- as.factor(pop_data_all_years$year)
dft$year <- as.numeric(dft$year)



pop_data_all_years <- as.data.frame(pop_data_all_years)


dft <- inner_join(dft, pop_data_all_years, by=c("ADM3_PCODE"="ADM3_PCODE", "year"="year"))

colnames(dft)


# Find NA  values and outliers

sapply(dft, anyNA)


summary(dft$vil_per_km2)
dft %>% filter(vil_per_km2 >5)

dft <- dft %>% filter(ttl_pop > 100)

dft <- dft %>% mutate(time_to_treat = year - 2001)    #create lag/lead variable for treatment
dft <- dft %>% mutate(pop_dens = ttl_pop/area_km2)


save(dft, file="./data/dataframe_p_c_f_r.Rdata")

##################################################################################################################################################
############################################# Try it With All Years ###################################################################
##################################################################################################################################################

load("./data/inter_data/inter_df_fc_r.Rdata")

View(df)








load("./data/inter_df_fc.Rdata")
load("./data/inter_df_cc.Rdata")


df <- inner_join(vil_count_01, df, by="ADM3_PCODE" )
crop_cover_df <-  inner_join(vil_count_01, crop_cover_df, by="ADM3_PCODE" )



dft <- df
crop_cover_dft <- crop_cover_df

dft$baseline_forest <- dft$sum_cover_1992* .09
crop_cover_dft$baseline_crop <- crop_cover_dft$crop_perc_cover_1992 * .09



dft <- dft %>% 
  pivot_longer(                            #pivot to correct format for feols
    cols = starts_with("sum_cover"),
    names_to = "year",
    names_prefix = "sum_cover_",
    values_to = "area_forest",
    values_drop_na = T)%>%
  mutate(area_forest = area_forest*.09,     #multiply by .09 to get to km2 (300m^2 was resolution)
         treat = if_else(year > 2001, 1,0),
         p_forest = area_forest/area_km2,   #proportion of Tambon covered in forest
         p_forest_baseline = baseline_forest/area_km2)%>%            #proportion of Tambon in forest in 1995 - baseline period
  group_by(ADM3_PCODE)%>%
  mutate(forest_change = area_forest - lag(area_forest),
         ihs_area_forest = asinh(area_forest),
         ihs_forest_change = ihs_area_forest - lag(ihs_area_forest),
         vil_per_km2 = village_count/area_km2)%>%
  ungroup()%>%
  as.data.frame() 



crop_cover_dft <- crop_cover_dft %>% 
  pivot_longer(                            #pivot to correct format for feols
    cols = starts_with("crop_perc_cover"),
    names_to = "year",
    names_prefix = "crop_perc_cover_",
    values_to = "area_crop",
    values_drop_na = T)%>%
#  subset(year %in%  c(1995:2007))%>%        #Reduce data set to correct number of years ('92-'94 shows non-credible figures for forest cover)
  mutate(area_crop = area_crop*.09)%>%
  group_by(ADM3_PCODE)%>%
  mutate(crop_change= area_crop - lag(area_crop),
         ihs_area_crop = asinh(area_crop),
         ihs_crop_change = ihs_area_crop - lag(ihs_area_crop))%>%
  ungroup()%>%
  as.data.frame()

all(dft$ADM3_PCODE == crop_cover_dft$ADM3_PCODE)
dft <- cbind(dft, crop_cover_dft)
dft <- as.data.frame(dft)

dft <- dft %>% subset(select=which(!duplicated(names(.)))) 
dft$village_count <- dft$village_count.y
dft <- dft%>% subset(select=-c(village_count.x, village_count.y))


#pop_data_all_years$ADM3_PCODE <- as.factor(pop_data_all_years$ADM3_PCODE)
#dft$ADM3_PCODE <- as.factor(dft$ADM3_PCODE)
#pop_data_all_years$year <- as.factor(pop_data_all_years$year)
dft$year <- as.numeric(dft$year)



pop_data_all_years <- as.data.frame(pop_data_all_years)


dft <- inner_join(dft, pop_data_all_years, by=c("ADM3_PCODE"="ADM3_PCODE", "year"="year"))

colnames(dft)
filter(dft, year==1995) %>% View()



dft<-subset(dft,select=-c(ADM3_REF, ADM3ALT1EN, ADM3ALT2EN, ADM3ALT1TH, ADM3ALT2TH, validTo))





# Find NA  values and outliers

sapply(dft, anyNA)


summary(dft$vil_per_km2)
dft %>% filter(vil_per_km2 >5)

dft <- dft %>% filter(ttl_pop > 100)

dft <- dft %>% mutate(time_to_treat = year - 2001)    #create lag/lead variable for treatment
dft <- dft %>% mutate(pop_dens = ttl_pop/area_km2)

dft %>% subset(year %in%  c(1992:2014))   #Make sure we have not lost any years when joining / filtering etc



save(dft, file="./data/dataframe_p_c_f_93_14.Rdata")




############################## 
# 5 - Plots to ensure data is correct 
##############################


# OK lets figure out what is causing this odd grouping around 


#I first found the error when I made a scatter plot of forest change - vil/km
# Restricted to single year so each point represents a distinct Tambon (note only happens at small values if include full range effect is not visibletion can't see it )

q_99 <- quantile(dft$forest_change, .99)
q_1 <- quantile(dft$forest_change, .01)
      
      #Scatter Plot of Treatment and Outcome variable
dft  %>% 
        mutate(forest_change = if_else(forest_change > q_99, q_99, forest_change),
               forest_change = if_else(forest_change < q_1, q_1, forest_change))%>%
        ggplot(aes(x=vil_per_km2,y=forest_change)) +
        geom_point()



#In order to figure out what is causing this stratification along forest_change I plot 
#the thee constituent values of forest change - area_forest, prop_forest, and area to see which are stratified
# Note I am taking quantiles / restricting the range in order to highlighhts stratification (Full range its hard to see)

#First I look at forest area to see if that is stratified
q_99 <- quantile(dft$area_forest, .6)
q_1 <- quantile(dft$area_forest, .4)

dft  %>% 
  mutate(area_forest = if_else(area_forest > q_99, q_99, area_forest))%>%
  filter(area_forest!=0, year==2000)%>%
  ggplot(aes(x=vil_per_km2,y=area_forest)) +
  geom_point()

#So as expected area_forest is also stratified, more stratified at lower values
# Now I look at prop_forest (the number of forest pixels divided by number of pixels in each Tambon) 


q_99 <- quantile(dft$prop_forest, .99)
q_1 <- quantile(dft$prop_forest, .01)

dft  %>%
  mutate(prop_forest = if_else(prop_forest > q_99, q_99, prop_forest))%>%
  filter(prop_forest!=0, year==2000)%>%
  ggplot(aes(x=vil_per_km2,y=prop_forest)) +
  geom_point()

#OK so proportion of forest doesn't have any stratification - must be coming from the area_km2 variable

q_99 <- quantile(dft$area_km2, .5)
q_1 <- quantile(dft$area_km2, .02)


dft  %>%
  mutate(area_km2 = if_else(area_km2 > q_99, q_99, area_km2))%>%
  filter(area_km2!=0, year==2000)%>%
  ggplot(aes(x=vil_per_km2,y=area_km2)) +
  geom_point()

# Data is stratified along a very odd way almost like a utility function it is because vil_per_km2 is also using the area variable
# I'm going to scatter plot it with village count to have a more idependant figure in the plot
q_99 <- quantile(dft$area_km2, .3)
q_1 <- quantile(dft$area_km2, .02)


dft  %>%
  mutate(area_km2 = if_else(area_km2 > q_99, q_99, area_km2))%>%
  filter(area_forest!=0, year==2000)%>%
  ggplot(aes(x=Tambon,y=area_km2)) +
  geom_point()

plot(nat_forest[[1]])

# Plotting Forest Area by Tambon

q_99 <- quantile(dft$area_forest, .3)
q_1 <- quantile(dft$area_forest, .02)


dft  %>%
  mutate(area_forest = if_else(area_forest > q_99, q_99, area_forest))%>%
  filter(area_forest!=0,area_forest<1, year==2000)%>%
  ggplot(aes(x=Tambon,y=area_forest)) +
  geom_point()


