#############################################################
## Similar to Data Clean but filter out any Tambon's with large urban centres 
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
library(exactextractr)
############################## 
# 1 - Source file 
##############################

setwd("~/R/CreditLandUseThailand")
load("./data/LandCoverThailand/thai_forest_cover_dennis")


############################## 
# 2 - Count number of Villages per Tambon
##############################


# Language to Thai
Sys.setlocale(locale = "Thai")


load("./data/vil_data.Rdata") #load villlage count data from the registry office data called tamb_vil_count
load("./data/pop_data_all_years.Rdata") #Load the all the population data for every year in one large dataframe


vil_count_01 <- tamb_vil_count[[7]] #The village count using the 2001 population registry data - ie the number of villages in 2001 at start of program


############################## 
# 3 - Determine Forest Cover 
############################## 


# List of names of rasters for each year
rasternames <- c("thai_forestcover_1992",
                 "thai_forestcover_1993",
                 "thai_forestcover_1994",
                 "thai_forestcover_1995",
                 "thai_forestcover_1996",
                 "thai_forestcover_1997",
                 "thai_forestcover_1998",
                 "thai_forestcover_1999",
                 "thai_forestcover_2000",
                 "thai_forestcover_2001",
                 "thai_forestcover_2002",
                 "thai_forestcover_2003",
                 "thai_forestcover_2004",
                 "thai_forestcover_2005",
                 "thai_forestcover_2006",
                 "thai_forestcover_2007",
                 "thai_forestcover_2008",
                 "thai_forestcover_2009",
                 "thai_forestcover_2010",
                 "thai_forestcover_2011",
                 "thai_forestcover_2012",
                 "thai_forestcover_2013",
                 "thai_forestcover_2014")

# Read in the saved rasters
nat_forest <- vector(mode = "list", length = length(rasternames)) #initialise variable name

for(i in 1:length(rasternames)){
  nat_forest[[i]] <- raster(paste0('./data/thai_forest_cover_dennis/',rasternames[[i]]))
}


# nat_forest <- esa_thai
# nat_cropland <- esa_thai


# Load each raster into list 
# for(i in 1:length(rasternames)){
#   nat_forest[[i]] <- raster(paste0('./data/forest_cover_thailand_92_14/',rasternames[[i]]))
# }

#Consolidate raster values 
for(i in 1:length(nat_forest)){

  nat_forest[[i]][nat_forest[[i]] %in% c(190)] <- 1 #urban 
    
  nat_forest[[i]][nat_forest[[i]] %in% c(40,50, 60, 61, 62, 70, 72, 80, 81, 82, 90, 100, 110, 120, 121,
                                         122, 130, 140, 150, 152, 153, 160, 170, 180 )] <- 2 # forest/bio area 
  
   nat_forest[[i]][nat_forest[[i]] %in% c(10,11,12,20,30)] <- 3   #crop 
   
   nat_forest[[i]][nat_forest[[i]] %in% c(200, 201, 210, 220)] <- 4    #bare areas  / water
}

#Take a look at what is now forest/not
plot(nat_forest[[20]])




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


forest_rast <- nat_forest
urban_rast <-  nat_forest
crop_rast <-  nat_forest
bare_rast <-  nat_forest

for(i in 1:length(nat_forest)){
  forest_rast[[i]][forest_rast[[i]] %in% c(1,3,4)] <- 0 # not forest
  forest_rast[[i]][forest_rast[[i]] %in% c(2)] <- 1 # forest/bio area 
  
  urban_rast[[i]][urban_rast[[i]] %in% c(2,2,4)] <- 0 # not urban
  urban_rast[[i]][urban_rast[[i]] %in% c(1)] <- 1 # urban
  
  crop_rast[[i]][crop_rast[[i]] %in% c(1,3,4)] <- 0 #not crop
  crop_rast[[i]][crop_rast[[i]] %in% c(2)] <- 1 # crop
  
  bare_rast[[i]][bare_rast[[i]] %in% c(1,3,4)] <- 0 # not bare
  bare_rast[[i]][bare_rast[[i]] %in% c(2)] <- 1 # bare
}

forest_cover <- vector("list", 23) #area forest
crop_cover <- vector("list", 23) #area cropland
urban_cover <- vector("list", 23) #area forest
bare_cover <- vector("list", 23) #area cropland


for(i in 1:length(forest_cover)){
  forest_cover[[i]] <- raster::extract(nat_forest[[i]], tamb_border['geometry'], fun=sum)
}
for(i in 1:length(crop_cover)){
  crop_cover[[i]] <- raster::extract(nat_forest[[i]], tamb_border['geometry'], fun=sum)
}

for(i in 1:length(urban_cover)){
  urban_cover[[i]] <- raster::extract(nat_forest[[i]], tamb_border['geometry'], fun=sum)
}
for(i in 1:length(bare_cover)){
  bare_cover[[i]] <- raster::extract(nat_forest[[i]], tamb_border['geometry'], fun=sum)
}


# change this vector into a dataframe, set collumn names forest cover
forest_cover_df <- as.data.frame(forest_cover)
colnames(forest_cover_df) <- c('forest_cover_1992', "forest_cover_1993", "forest_cover_1994", "forest_cover_1995", "forest_cover_1996", 
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

# change this vector into a dataframe, set collumn names crop cover
bare_cover_df <- as.data.frame(bare_cover)
colnames(bare_cover_df) <- c('bare_cover_1992', "bare_cover_1993", "bare_cover_1994", "bare_cover_1995", "bare_cover_1996", 
                             "bare_cover_1997", "bare_cover_1998", "bare_cover_1999", 'bare_cover_2000', "bare_cover_2001",
                             "bare_cover_2002", "bare_cover_2003", "bare_cover_2004", "bare_cover_2005", "bare_cover_2006", 
                             "bare_cover_2007", "bare_cover_2008", "bare_cover_2009", "bare_cover_2010",        
                             "bare_cover_2011", "bare_cover_2012", "bare_cover_2013", "bare_cover_2014")

df_names <- c('forest_cover_df', 'crop_cover_df','urban_cover_df', 'bare_cover_df')



df <- cbind(forest_cover_df, tamb_border)
crop_cover_df <- cbind(crop_cover_df, tamb_border['ADM3_PCODE'])
urban_cover_df <- cbind(urban_cover_df, tamb_border['ADM3_PCODE'])
bare_cover_df <- cbind(bare_cover_df, tamb_border['ADM3_PCODE'])

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

save(df, file= "./data/inter_df.Rdata")

##################################################################################################################################################
############################################# Try it With New Stuff ###################################################################
##################################################################################################################################################
load("./data/inter_df.Rdata")


df <- inner_join(vil_count_01, df, by="ADM3_PCODE" )
crop_cover_df <- inner_join(vil_count_01, crop_cover_df, by="ADM3_PCODE" )
urban_cover_df <- inner_join(vil_count_01, urban_cover_df, by="ADM3_PCODE" )
bare_cover_df <- inner_join(vil_count_01, bare_cover_df, by="ADM3_PCODE" )


dft <- df
crop_cover_dft <- crop_cover_df
urban_cover_dft <- urban_cover_df
bare_cover_dft <- bare_cover_df


dft$baseline_forest <- dft$forest_cover_1995 * .09
crop_cover_dft$baseline_crop <- crop_cover_dft$crop_cover_1995 * .09
urban_cover_dft$baseline_crop <- urban_cover_dft$urban_cover_1995 * .09
bare_cover_dft$baseline_crop <- bare_cover_dft$bare_cover_1995 * .09

dft <- dft %>% 
  pivot_longer(                            #pivot to correct format for feols
    cols = starts_with("forest_cover"),
    names_to = "year",
    names_prefix = "forest_cover_",
    values_to = "area_forest",
    values_drop_na = T)%>%
  #subset(year %in%  c(1995:2007))%>%        #Reduce data set to correct number of years
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
 #subset(year %in%  c(1995:2007))%>%        #Reduce data set to correct number of years ('92-'94 shows non-credible figures for forest cover)
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
  #subset(year %in%  c(1995:2007))%>%        #Reduce data set to correct number of years ('92-'94 shows non-credible figures for forest cover)
  mutate(area_urban = area_urban*.09)%>%
  group_by(ADM3_PCODE)%>%
  mutate(urban_change= area_urban - lag(area_urban),
         ihs_area_urban = asinh(area_urban),
         ihs_urban_change = ihs_area_urban - lag(ihs_area_urban))%>%
  ungroup()%>%
  as.data.frame()




bare_cover_dft <- bare_cover_dft %>% 
  pivot_longer(                            #pivot to correct format for feols
    cols = starts_with("bare_cover"),
    names_to = "year",
    names_prefix = "bare_cover_",
    values_to = "area_bare",
    values_drop_na = T)%>%
  #subset(year %in%  c(1995:2007))%>%        #Reduce data set to correct number of years ('92-'94 shows non-credible figures for forest cover)
  mutate(area_bare = area_bare*.09)%>%
  group_by(ADM3_PCODE)%>%
  mutate(bare_change= area_bare - lag(area_bare),
         ihs_area_bare = asinh(area_bare),
         ihs_bare_change = ihs_area_bare - lag(ihs_area_bare))%>%
  ungroup()%>%
  as.data.frame()


View(bare_cover_dft)





dft <- cbind(dft, crop_cover_dft,urban_cover_dft,bare_cover_dft, by='ADM3_PCODE')

dft <- as.data.frame(dft)

dft <- dft %>% subset(select=which(!duplicated(names(.)))) 
colnames(dft)



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

dft %>% subset(year %in%  c(1995:2007))   #Make sure we have not lost any years when joining / filtering etc



save(dft, file="./data/dataframe_p_c_f.Rdata")

##################################################################################################################################################
############################################# Try it With All Years ###################################################################
##################################################################################################################################################
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



dft<-subset(dft,select=-c(date, by, validOn, Shape_Area, Shape_Leng, ADM3_REF, ADM3ALT1EN, ADM3ALT2EN, ADM3ALT1TH, ADM3ALT2TH, validTo))





# Find NA  values and outliers

sapply(dft, anyNA)


summary(dft$vil_per_km2)
dft %>% filter(vil_per_km2 >5)

dft <- dft %>% filter(ttl_pop > 100)

dft <- dft %>% mutate(time_to_treat = year - 2001)    #create lag/lead variable for treatment
dft <- dft %>% mutate(pop_dens = ttl_pop/area_km2)

dft %>% subset(year %in%  c(1992:2014))   #Make sure we have not lost any years when joining / filtering etc



save(dft, file="./data/dataframe_fcub_92_14.Rdata") #fcub = forest crop urban bare 
