#############################################################
## Join the ESA data with the Village Data, generate the dataset needed for analyssis (forest cover etc.)
## dobeirne@mail.ubc.ca - Feb 2023
#############################################################

##############################
# 0 - Load librairies
##############################

library(fixest)     ## NB: Requires version >=0.9.0
library(tidyverse)
library(data.table)
library(dplyr)
library(datawizard)

############################## 
# 1 - Source file 
##############################

setwd("~/R/CreditLandUseThailand")
load("./data/dataframe_p_c_f.Rdata")
dft <- as.data.frame(dft) %>%   subset(select=-c(geometry)) # any land cover that is considered tree cover >15% and up


##################################################################################################################################################
############################################# All Tree Cover - All Districts - Levels ###################################################################
##################################################################################################################################################

#restrict years to ones of interest
df <- dft %>% 
  subset(year %in% c(1995:2007))

# Forest Level Raw
DiD1_a <- df %>% 
  feols(area_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Forest Level Winsorize
DiD1_b <- df %>% 
 mutate(area_forest = winsorize(area_forest, threshold =.01),
 vil_per_km2 =  winsorize(vil_per_km2, threshold =.01),
 pop_dens = winsorize(pop_dens, threshold =.01))%>%
  feols(area_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Forest Level asinh
DiD1_c <- df %>%
 mutate(area_forest = asinh(area_forest),
 vil_per_km2 =  asinh(vil_per_km2),
 pop_dens = asinh(pop_dens))%>%
  feols(area_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

tab1 <-  etable(DiD1_a, DiD1_b, DiD1_c, tex=T)

##################################################################################################################################################
############################################# All Tree Cover - All Districts - Percentage ###################################################################
##################################################################################################################################################
df <- df %>% mutate(vil_per_km2=vil_per_km2*1000000) #Change so that credit is in baht/km2 not mil_baht/km2

# Forest Percetnage Raw
DiD2_a <- df %>% 
  feols(p_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

 regtest <- lm

# Forest Level Winsorized
DiD2_b <- df %>% 
 mutate(p_forest = winsorize(p_forest, threshold =.01),
 vil_per_km2 =  winsorize(vil_per_km2, threshold =.01),
 pop_dens = winsorize(pop_dens, threshold =.01))%>%
  feols(p_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

DiD2_c <- df %>%
 mutate(vil_per_km2 =  asinh(vil_per_km2),
 pop_dens = asinh(pop_dens))%>%
  feols(p_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

tab2 <-  etable(DiD2_a, DiD2_b, DiD2_c, tex=F)



##################################################################################################################################################
############################################# All Tree Cover - Mixed Districts - Levels ###################################################################
##################################################################################################################################################


df <- dft %>% 
  filter(p_forest_baseline > .2)%>%
  mutate(p_crop_baseline = baseline_crop/area_km2,
         p_urban_baseline = baseline_urban/area_km2)%>%
  filter(p_crop_baseline > .2)%>%
  subset(year %in% c(1995:2007))

# Forest Level Raw
DiD5_a <- df %>% 
  feols(area_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Forest Level Winsorize
DiD5_b <- df %>% 
  mutate(area_forest = winsorize(area_forest, threshold =.01),
         vil_per_km2 =  winsorize(vil_per_km2, threshold =.01),
         pop_dens = winsorize(pop_dens, threshold =.01))%>%
  feols(area_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Forest Level - >20% forest
DiD5_c <- df %>%
  mutate(area_forest = asinh(area_forest),
         vil_per_km2 =  asinh(vil_per_km2),
         pop_dens = asinh(pop_dens))%>%
  feols(area_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

tab5 <-  etable(DiD5_a, DiD5_b, DiD5_c, tex=T)


##################################################################################################################################################
############################################# All Tree Cover - Mixed Districts - Percentage ###################################################################
##################################################################################################################################################


# Forest Percetnage Raw
DiD6_a <- df %>% 
  feols(p_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

pdf <- panel(df,  ~ADM3_PCODE+year)
# 
# #As above including lags
# DiD6_a <- pdf %>% 
#   feols(p_forest ~ vil_per_km2:treat + l(p_forest,1:3) + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE'))


# Forest Level Winsorized
DiD6_b <- df %>% 
  mutate(p_forest = winsorize(p_forest, threshold =.01),
         vil_per_km2 =  winsorize(vil_per_km2, threshold =.01),
         pop_dens = winsorize(pop_dens, threshold =.01))%>%
  feols(p_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

DiD6_c <- df %>%
  mutate(vil_per_km2 =  asinh(vil_per_km2),
         pop_dens = asinh(pop_dens))%>%
  feols(p_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# 
# DiD6_c <- df %>%
#   mutate(vil_per_km2 =  asinh(vil_per_km2),
#          pop_dens = asinh(pop_dens))%>%
#   feols(p_forest ~  vil_per_km2:treat + l(p_forest,3) + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

mean(asinh(df$vil_per_km2))

tab6 <-  etable(DiD6_a, DiD6_b, DiD6_c, tex=F)



tab1
tab2
tab5
tab6




##################################################################################################################################################
############################################# Extra Stuff ###################################################################
##################################################################################################################################################

#Add the ALLYEARS regressions to one large table and shove in appendix

tab_app1 <- etable(DiD1_a, DiD1_b, DiD1_c,DiD2_a, DiD2_b, DiD2_c,DiD5_a, DiD5_b, DiD5_c,DiD6_a, DiD6_b, DiD6_c, tex=T)


##################################################################################################################################################
############################################# All Crop Cover - All Districts - Levels ###################################################################
##################################################################################################################################################

#restrict years to ones of interest
df <- dft %>% 
  subset(year %in% c(1995:2007))

# Forest Level Raw
DiD1_a <- df %>% 
  feols(area_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Forest Level Winsorize
DiD1_b <- df %>% 
  mutate(area_crop = winsorize(area_crop, threshold =.01),
         vil_per_km2 =  winsorize(vil_per_km2, threshold =.01),
         pop_dens = winsorize(pop_dens, threshold =.01))%>%
  feols(area_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

DiD1_c <- df %>%
  mutate(area_crop = asinh(area_crop),
         vil_per_km2 =  asinh(vil_per_km2),
         pop_dens = asinh(pop_dens))%>%
  feols(area_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

tab1 <-  etable(DiD1_a, DiD1_b, DiD1_c, tex=T)

##################################################################################################################################################
############################################# All Crop Cover - All Districts - Percentage ###################################################################
##################################################################################################################################################


# Forest Percetnage Raw
DiD2_a <- df %>% 
  mutate(p_crop = area_crop/area_km2)%>%
  feols(p_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Forest Level Winsorized
DiD2_b <- df %>% 
  mutate(p_crop = area_crop/area_km2)%>%
  mutate(p_crop = winsorize(p_crop, threshold =.01),
         vil_per_km2 =  winsorize(vil_per_km2, threshold =.01),
         pop_dens = winsorize(pop_dens, threshold =.01))%>%
  feols(p_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

DiD2_c <- df %>%
  mutate(p_crop = area_crop/area_km2)%>%
  mutate(vil_per_km2 =  asinh(vil_per_km2),
         pop_dens = asinh(pop_dens))%>%
  feols(p_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

tab2 <-  etable(DiD2_a, DiD2_b, DiD2_c, tex=T)



##################################################################################################################################################
############################################# All Crop Cover - Mixed Districts - Levels ###################################################################
##################################################################################################################################################


df <- dft %>% 
  filter(p_forest_baseline > .2)%>%
  mutate(p_crop_baseline = baseline_crop/area_km2,
         p_urban_baseline = baseline_urban/area_km2)%>%
  filter(p_crop_baseline > .2)%>%
  subset(year %in% c(1995:2007))

# Forest Level Raw
DiD5_a <- df %>% 
  feols(area_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Forest Level Winsorize
DiD5_b <- df %>% 
  mutate(area_crop = winsorize(area_crop, threshold =.01),
         vil_per_km2 =  winsorize(vil_per_km2, threshold =.01),
         pop_dens = winsorize(pop_dens, threshold =.01))%>%
  feols(area_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Forest Level - >20% forest
DiD5_c <- df %>%
  mutate(area_crop = asinh(area_crop),
         vil_per_km2 =  asinh(vil_per_km2),
         pop_dens = asinh(pop_dens))%>%
  feols(area_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

tab5 <-  etable(DiD5_a, DiD5_b, DiD5_c, tex=T)


##################################################################################################################################################
############################################# All Crop Cover - Mixed Districts - Percentage ###################################################################
##################################################################################################################################################


# Forest Percetnage Raw
DiD6_a <- df %>% 
  mutate(p_crop = area_crop/area_km2)%>%
  feols(p_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Forest Level Winsorized
DiD6_b <- df %>% 
  mutate(p_crop = area_crop/area_km2)%>%
  mutate(p_crop = winsorize(p_crop, threshold =.01),
         vil_per_km2 =  winsorize(vil_per_km2, threshold =.01),
         pop_dens = winsorize(pop_dens, threshold =.01))%>%
  feols(p_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

DiD6_c <- df %>%
  mutate(p_crop = asinh(area_crop/area_km2))%>%
  mutate(vil_per_km2 =  asinh(vil_per_km2),
         pop_dens = asinh(pop_dens))%>%
  feols(p_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 


tab6 <-  etable(DiD6_a, DiD6_b, DiD6_c, tex=T)


dft %>% filter(area_crop/area_km2 >1) %>% View


tab1
tab2
tab5
tab6







































##################################################################################################################################################
#############################################Regression with population density ###################################################################
##################################################################################################################################################




# Forest Level 
DiD1_a <- dft %>% 
  feols(area_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 


# Winsorised Forest Level 
DiD1_b <- df %>% 
  mutate(area_crop = winsorize(area_crop, threshold =.01))%>%
  feols(area_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Inverse Hyperbolic Sine Forest Level 
DiD1_c <- df %>% 
  feols(ihs_area_crop ~ vil_per_km2:treat + pop_dens| year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Change in Forest Level 
DiD1_d <- df %>% 
  feols(forest_change ~ vil_per_km2:treat  + pop_dens| year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Change in IHS Forest Level 
DiD1_e <- df %>% 
  feols(ihs_forest_change ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Change in Winsorised Forest Level 
DiD1_f <- df %>% 
  mutate(forest_change =  winsorize(forest_change, threshold =.01))%>%
  feols(forest_change ~ vil_per_km2:treat + pop_dens| year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Unrestricted Forest (RC) Regressions 
UFpop <- etable(DiD1_a, DiD1_b, DiD1_c, DiD1_d, DiD1_e, DiD1_f, tex=F)



############################## 
# 2-b - Regression on Restricted forest cover
##############################

df_r_f <- df %>% 
  filter(village_count > 0)%>%
  filter(p_forest_baseline > .2)

# Forest Level 
DiD2_a <- df_r_f %>% 
  feols(p_forest ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 


# Winsorised Forest Level 
DiD2_b <- df_r_f %>% 
  mutate(area_crop = winsorize(df_r_f$area_crop, threshold =.01))%>%
  feols(area_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 


# Inverse Hyperbolic Sine Forest Level 
DiD2_c <- df_r_f %>% 
  feols(ihs_area_crop ~ asinh(vil_per_km2):treat + asinh(pop_dens) | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Change in Forest Level 
DiD2_d <- df_r_f %>% 
  feols(forest_change ~ vil_per_km2:treat + pop_dens  | year + ADM3_PCODE + p_forest_baseline, cluster=c('ADM3_PCODE')) 

# Change in IHS Forest Level 
DiD2_e <- df_r_f %>% 
  feols(ihs_forest_change ~ vil_per_km2:treat + pop_dens| year + ADM3_PCODE + p_forest_baseline, cluster=c('ADM3_PCODE')) 

# Change in Winsorised Forest Level Restricted
DiD2_f <- df_r_f %>% 
  mutate(forest_change =  winsorize(df_r_f$forest_change, threshold =.01))%>%
  feols(forest_change ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE + p_forest_baseline, cluster=c('ADM3_PCODE')) 

# Restrcited Forest (RF) Regressions
RFpop <- etable(DiD2_a, DiD2_b, DiD2_c, DiD2_d, DiD2_e, DiD2_f, tex=F)




############################## 
# 3-b - Regression on Unrestricted crop cover
##############################

# Forest Level 
DiD3_a <- df %>% 
  feols(area_crop ~ vil_per_km2:treat + pop_dens  | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 


# Winsorised Forest Level 
DiD3_b <- df %>% 
  mutate(area_crop = winsorize(dft$area_crop, threshold =.01))%>%
  feols(area_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 


# Inverse Hyperbolic Sine Forest Level 
DiD3_c <- df %>% 
  feols(ihs_area_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Change in Forest Level 
DiD3_d <- df %>% 
  feols(crop_change ~vil_per_km2:treat + pop_dens  | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Change in Winsorised Forest Level Restricted
DiD3_e <- df %>% 
  mutate(area_crop =  winsorize(dft$area_crop, threshold =.01))%>%
  feols(area_crop ~ vil_per_km2:treat + pop_dens| year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Change in IHS Forest Level 
DiD3_f <- df %>% 
  feols(ihs_crop_change ~ vil_per_km2:treat + pop_dens  | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 


# Restricted Forest (RF) Regressions
UCpop <- etable(DiD3_a, DiD3_b, DiD3_c, DiD3_d, DiD3_e, DiD3_f, tex=F)


############################## 
# 4-b - Regression on Restricted crop cover
##############################

df_r_c <- df %>% 
  filter(village_count > 0)%>%
  filter(baseline_forest > .2)%>%
  filter(ttl_pop < 100000) 

# Forest Level 
DiD4_a <- df_r_c %>% 
  feols(area_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 


# Winsorised Forest Level 
DiD4_b <- df_r_c %>% 
  mutate(area_crop = winsorize(df_r_c$area_crop, threshold =.01))%>%
  feols(area_crop ~ vil_per_km2:treat + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 


# Inverse Hyperbolic Sine Forest Level 
DiD4_c <- df_r_c %>% 
  feols(ihs_area_crop ~ vil_per_km2:treat  + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 


# Change in Forest Level 
DiD4_d <- df_r_c %>% 
  feols(crop_change ~ vil_per_km2:treat  + pop_dens  | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Change in Winsorised Forest Level Restricted
DiD4_e <- df_r_c %>% 
  mutate(crop_change =  winsorize(df_r_c$crop_change, threshold =.01))%>%
  feols(crop_change ~ vil_per_km2:treat  + pop_dens  | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 

# Change in IHS Forest Level 
DiD4_f <- df_r_c %>% 
  feols(ihs_crop_change ~ vil_per_km2:treat  + pop_dens | year + ADM3_PCODE, cluster=c('ADM3_PCODE')) 


# Restrcited Forest (RF) Regressions
RCpop <- etable(DiD4_a, DiD4_b, DiD4_c, DiD4_d, DiD4_e, DiD4_f, tex=F)

UFpop
RFpop
UCpop
RCpop
##################################################################################################################################################
############################################# Regressions using fixed population ###################################################################
##################################################################################################################################################


mean_fc <- mean(df$forest_change)
sd_fc <- sd(df$forest_change)
mean_cc <- mean(df$crop_change)
sd_cc <- sd(df$crop_change)

