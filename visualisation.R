#############################################################
## Make Plots, Graphs, and Maps
## dobeirne@mail.ubc.ca - Feb 2023
#############################################################


##############################
# 0 - Load libraries
##############################
library(ggplot2)
library(datawizard)
library(ggpubr)
library(fixest)
library(xtable)
############################## 
# 1 - Source file 
##############################
setwd("~/R/CreditLandUseThailand")
load("./data/dataframe_p_c_f.Rdata")

fert <- read.csv('./data/fert.csv')
gdp <- readxl::read_excel("./data/gdp_data.xlsx")
dft <- as.data.frame(dft) %>%   subset(select=-c(geometry)) # any land cover that is considered tree cover >15% and up



df <- dft %>% 
  filter(p_forest_baseline > .2)%>%
  mutate(p_crop_baseline = baseline_crop/area_km2,
         p_urban_baseline = baseline_urban/area_km2)%>%
  filter(p_crop_baseline > .2)%>%
  subset(year %in% c(1995:2007))

df <- dft %>% subset(year %in% c(1995:2007))
df <- df %>% mutate(vil_per_km2=vil_per_km2*1000000) #Change so that credit is in baht/km2 not mil_baht/km2

var <- c('Variable', 'Mean', 'SD', 'N')
Variable <- c('Population', 'Credit/Km2','Crop Area', 'Forest Area')
Mean <- c(mean(df$ttl_pop),mean(df$vil_per_km2),mean(df$area_crop),mean(df$area_forest))
SD <- c(sd(df$ttl_pop),sd(df$vil_per_km2),sd(df$area_crop),sd(df$area_forest)) 
N <- c(n_distinct(df$ADM3_PCODE),n_distinct(df$ADM3_PCODE),n_distinct(df$ADM3_PCODE),n_distinct(df$ADM3_PCODE))

sum_stats <- data.frame(Variable, Mean, SD, N)
colnames(sum_stats) <- var
sum_stats <- format(sum_stats, digits = 2)
xtable(sum_stats)


#Scatter Plot of Treatment and Forest Cover Level
f_c_corr <-   df  %>% 
                ggplot(aes(x=vil_per_km2,y=area_forest)) +
                geom_point(size = 0.1)+
                geom_smooth(method=lm, se=FALSE)+
                ylim(0,400)+
                ggtitle("All Districts Forest Cover - Credit")+
                ylab("Forest Cover (KM2)")+
                xlab("$ / KM2")



                # Log Transformed Forest - Vil LEvel 
ln_f_c_corr <-   df  %>% 
                #mutate(forest_change = winsorize(df$forest_change, threshold =.01))%>%
                ggplot(aes(x=winsorize(vil_per_km2, threshold =.01),y=winsorize(area_forest, threshold =.01))) +
                geom_point(size = 0.1)+
                geom_smooth(method=lm, se=FALSE)+
                ylim(0,250)+
                ggtitle("Winsorised All Districts Forest Cover - Credit")+
                ylab("Forest Cover (KM2)")+
                xlab("$ / KM2")
                            

df <- dft %>% 
  filter(p_forest_baseline > .2)%>%
  mutate(p_crop_baseline = baseline_crop/area_km2,
         p_urban_baseline = baseline_urban/area_km2)%>%
  filter(p_crop_baseline > .2)%>%
  subset(year %in% c(1995:2007))           
              
              #Scatter Plot of Treatment and Crop Cover
rf_c_corr  <-    df  %>% 
                #mutate(area_crop = winsorize(df$area_crop, threshold =.01))%>%
                ggplot(aes(x=vil_per_km2,y=area_forest)) +
                geom_point(size = 0.1)+
                geom_smooth(method=lm, se=FALSE)+
                ylim(0,350)+
                ggtitle("Mixed Us Districts Forest - Credit")+
                ylab("Forest Cover (KM2)")+
                xlab("$ / KM2")
              
              #Scatter Plot of Treatment and Crop Change
ln_rf_c_corr <-    df  %>% 
                #mutate(crop_change = winsorize(df$crop_change, threshold =.01))%>%
                ggplot(aes(x=winsorize(vil_per_km2, threshold =.01),y=winsorize(area_forest, threshold =.01))) +
                geom_point(size = 0.1)+
                geom_smooth(method=lm, se=FALSE)+
                ylim(0,250)+
                ggtitle("Winsorised Mixed Us Districts Forest - Credit")+
                ylab("Forest Cover (KM2)")+
                xlab("$ / KM2")





figure <- ggarrange(f_c_corr, ln_f_c_corr, rf_c_corr, ln_rf_c_corr,
                    ncol = 2, nrow = 2)
figure



# fao fertiliser use data

  ftplot <- fert %>%
  filter(Item == 'Nutrient nitrogen N (total)')  %>%
  group_by(Year)%>% 
  summarise( ttl_fert = sum(Value)) %>% 
  ggplot(aes(x=Year))+
  geom_line(aes(y=ttl_fert))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4))+
  ggtitle("Thai Fertiliser Use Over Time")+
  ylab("Fertiliser (Tonnes)")+
  xlab("Year")+
  theme_bw()+
  theme(text = element_text(family = "serif"))


# World Bank Yearly Data in % land area available: ( 'https://databank.worldbank.org/reports.aspx?source=2&series=AG.LND.FRST.K2&country=THA')
forest <- c(	37.54134941,	37.47029693,	37.39924446,	
           37.32819198,	37.2571395,	37.18608702,	37.39650414,	37.60692126,	37.81733837,
           38.02775549,	38.2381726,	38.44858972,	38.65900683,	38.86942395,	39.07984106,	
           39.29025818,	39.28556049,	39.28086281,	39.27616512)
year <- c(seq(from=1995, to= (1992 + (length(forest)-1))))
wb_fc <- data.frame(forest, year)
wb_fc$forest <- wb_fc$forest/100 *510000 #multiply percentage by area of thailand


services <- c(25.4573,	26.36098,	26.90396,	27.19453,	28.678,	28.12943,	32.9241,	34.0381,	35.34557,	38.49863,	38.94458,	38.26642,	38.54847,	39.06964,	40.22821,	41.11111,	39.5509,	38.0346)
agriculture <- c(54.01022,	52.03444,	52.52293,	54.11882,	51.74048,	51.70787,	47.60991,	46.13151,	44.90262,	39.29028,	38.67606,	39.78234,	39.53115,	39.75113,	38.99155,	38.2445,	41.00846,	42.14297)
year <- c(seq(from=1995, to= (1995 + (length(services)-1))))
labour_data <- data.frame(services, agriculture, year)


#convert data from wide to long format
labour_data <- labour_data %>% pivot_longer(cols=c('agriculture', 'services'),
                          names_to='variable',
                          values_to='percent')


lab_plot <- labour_data %>% ggplot(aes(x=year, y=percent))+
  geom_line(aes(colour = variable))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+
  ggtitle("Thailand employment as % of total employment ")+
  ylab("Employment (%)")+
  xlab("Year")+
  theme_bw()+
  theme(text = element_text(family = "serif"))+
  scale_color_manual(name='Sector', labels=c('Agriculture','Service' ),
                                                                 values=c('darkolivegreen3', 'cornflowerblue' ))



gdp <- gdp[1:2,]
gdp <- gdp %>% dplyr::select(-c("Series Name", "Series Code", "Country Name","Country Code", "1990 [YR1990]"))
colnames(gdp) <- c(seq(from=1995, to= (1995 + (length(gdp)-1))))

gdp <- gdp %>% 
pivot_longer(                            #pivot to correct format for feols
  cols=everything(),
  names_to = "year",
  values_to = "gdp",
  values_drop_na = T)

gdp$year <- as.numeric(gdp$year)

wb_plot <- wb_fc %>% ggplot(aes(x=year))+
  geom_line(aes(y=forest))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+
  ggtitle("FAO Forest Over Time")+
  ylab("Forest Cover (KM2)")+
  xlab("Year")+
  theme_bw()+
  theme(text = element_text(family = "serif"))


gdp_plot <- gdp %>% 
  mutate(gdp = gdp/1000000000)%>%
  ggplot(aes(x=year))+
  geom_line(aes(y=gdp))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3))+
  ggtitle("GDP of Thailand")+
  ylab("GDP (Billions US$)")+
  xlab("Year")+
  theme_bw()+
  theme(text = element_text(family = "serif"))


# ttl Forest Cover Over Time
fplot <- dft%>%group_by(year)%>% 
  summarise(ttl_crop = sum(area_crop), ttl_forest = sum(area_forest)) %>% 
  ggplot(aes(x=year))+
  geom_line(aes(y=ttl_forest))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4))+
  ggtitle("Thai Forest Over Time")+
  ylab("Forest Cover (KM2)")+
  xlab("Year")+
  theme_bw()+
  theme(text = element_text(family = "serif"))



figure <- ggarrange(fplot, wb_plot  ,ncol=2 , nrow=1)
figure

############################################
######### Employment by Sector #############
emp <- read_csv("data/emp_stats/c0b949dc-21a9-443b-8802-ca924c74a249_Data.csv")

#Quick Clean of Data
emp <- emp %>%
                na.omit()
emp <- select(emp, -c(1:4))
colnames(emp) <- c(1994:2008) #Fix colnname to years numeric not strings
emp$Sector <- c("Agriculture", "Industry","Services")
emp <- emp %>% relocate(Sector)


emp <- emp %>%
  pivot_longer(cols = 2:16,
               names_to = 'year',
               values_to = 'share') %>%
  pivot_wider(names_from = 'Sector',
              values_from = 'share') %>%
  mutate(year = as.numeric(year))%>%
  subset(year %in% c(1995:2007))
  
  

emplot <- emp %>% 
  select(year, Agriculture, Industry, Services) %>%
  gather(key = "Sector", value = "Share", -year)

emplot %>% 
  ggplot(aes(x = factor(year), y = Share, group=Sector))+
  geom_line(aes(color = Sector, linetype = Sector)) + 
  scale_color_manual(values = c("darkred", "steelblue", 'olivedrab'))+
  scale_x_discrete()

############################################
######### Event Study ######################

df <- dft %>% 
  filter(p_forest_baseline > .2)%>%
  mutate(p_crop_baseline = baseline_crop/area_km2,
         p_urban_baseline = baseline_urban/area_km2)%>%
  filter(p_crop_baseline > .2)%>%
  subset(year %in% c(1995:2007))

df <- dft %>% subset(year %in% c(1995:2007))


# Do the TWFE Regression
mod_twfe <- feols(area_forest ~ i(time_to_treat, vil_per_km2, ref=c(-1)) + pop_dens |
                    ADM3_PCODE + year,
                  cluster = c('ADM3_PCODE'),
                  data=df)

 iplot(mod_twfe,
              xlab = 'Time to Treatment',
              ylab = "Estimate (Area Forest) and 95% Conf. Int.",
              main = "Event Study: Treatment in year 2001 \n")


aggregate(mod_twfe, c("ATT" = "time_to_treat::[^-]:vil_per_km2"))



summary(mod_twfe)

###############################################################
######### Data Over Time  Visualizations ######################

df <- dfa %>% 
  filter(p_forest_baseline > .2)%>%
  mutate(p_crop_baseline = baseline_crop/area_km2,
         p_urban_baseline = baseline_urban/area_km2)%>%
  filter(p_crop_baseline > .2)%>%
  subset(year %in% c(1995:2007))

df <- df %>% subset(year %in% c(1995:2007))


# ttl Forest Cover Over Time
fplot <- df%>%group_by(year)%>% 
          summarise(ttl_crop = sum(area_crop), ttl_forest = sum(area_forest)) %>% 
          ggplot(aes(x=year))+
          geom_line(aes(y=ttl_forest))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  ggtitle("Thai Forest Over Time")+
  ylab("Forest Cover (KM2)")+
  xlab("Year")

 # ttl crop Cover Over Time
cplot <- df%>%group_by(year)%>% 
          summarise(ttl_crop = sum(area_crop), ttl_forest = sum(area_forest)) %>% 
          ggplot(aes(x=year))+
          geom_line(aes(y=ttl_crop))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  ggtitle("Thai Crop Cover Over Time")+
  ylab("Crop Cover (KM2)")+
  xlab("Year")


# ttl Population  Over Time
pplot <- df%>%group_by(year)%>% 
  summarise(ttl_pop = sum(ttl_pop), ttl_forest = sum(area_forest)) %>% 
  ggplot(aes(x=year))+
  geom_line(aes(y=ttl_pop))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  ggtitle("Thai Population")+
  ylab("Population Cover (KM2)")+
  xlab("Year")

# ttl crop Cover Over Time
uplot <- df%>%group_by(year)%>% 
  summarise(ttl_urban = sum(area_urban), ttl_forest = sum(area_forest)) %>% 
  ggplot(aes(x=year))+
  geom_line(aes(y=ttl_urban))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  ggtitle("Thai Urban Cover Over Time")+
  ylab("Urban Cover (KM2)")+
  xlab("Year")



fcpplot <- ggarrange(fplot, cplot, pplot, uplot,
                    ncol = 2, nrow = 2)
fcpplot

table(df)

###############################################################
######### Histograms and Visualizations ######################


df <- dft %>% 
  filter(p_forest_baseline > .2)%>%
  mutate(p_crop_baseline = baseline_crop/area_km2,
         p_urban_baseline = baseline_urban/area_km2)%>%
  filter(p_crop_baseline > .2)%>%
  subset(year %in% c(1995:2007))

df <- dft %>% subset(year %in% c(1995:2007))


#histogram of forest
fh <- df%>% ggplot(aes(x=area_forest))+
        geom_histogram(bins=30)+
  ylab("Count")+
  xlab("Area Forest (KM2)")+
  theme_bw()+
  theme(text = element_text(family = "serif"))

#histogram of vil_per_km2
vh <- df%>% ggplot(aes(x=vil_per_km2))+
  geom_histogram(bins=30)+
  ylab("Count")+
  xlab("Credit ($/KM2)")+
  theme_bw()+
  theme(text = element_text(family = 'serif'))

#histogram of vil_per_km2
ch <- df%>% ggplot(aes(x=area_crop))+
  geom_histogram(bins=30)+
  ylab("Count")+
  xlab("Area Crop (KM2)")+
  theme_bw()+
  theme(text = element_text(family = 'serif'))

# Win histogram of forest
wfh <- df%>% ggplot(aes(x=winsorize(area_forest, threshold =.01)))+
  geom_histogram(bins=30)+
  ylab("Count")+
  xlab("Wisorise(Area Forest)")+
  theme_bw()+
  theme(text = element_text(family = 'serif'))

# Win histogram of vil_per_km2
wvh <- df%>% ggplot(aes(x=winsorize(vil_per_km2, threshold =.01)))+
  geom_histogram(bins=30)+
  ylab("Count")+
  xlab("Wisorise(Credit)")+
  theme_bw()+
  theme(text = element_text(family = 'serif'))

# Win histogram of vil_per_km2
wch <- df%>% ggplot(aes(x=winsorize(area_crop)), threshold =.01)+
  geom_histogram(bins=30)+
  ylab("Count")+
  xlab("Wisorise(Area Crop)")+
  theme_bw()+
  theme(text = element_text(family = 'serif'))

# IHS histogram of forest
lfh <- df%>% ggplot(aes(x=asinh(area_forest)))+
  geom_histogram(bins=30)+
  ylab("Count")+
  xlab("ihs(Area Forest)")+
  theme_bw()+
  theme(text = element_text(family = 'serif'))

# IHS histogram of vil_per_km2
lvh <- df%>% ggplot(aes(x=asinh(vil_per_km2)))+
  geom_histogram(bins=30)+
  ylab("Count")+
  xlab("ihs(Credit)")+
  theme_bw()+
  theme(text = element_text(family = 'serif'))

# IHS histogram of vil_per_km2
lch <- df%>% ggplot(aes(x=asinh(area_crop)))+
  geom_histogram(bins=30)+
  ylab("Count")+
  xlab("ihs(Area Crop)")+
  theme_bw()+
  theme(text = element_text(family = 'serif'))

hist <- ggarrange(ncol = 3, nrow = 1, fh,vh,ch)
hist


###############################################################
######### Maps ######################
plot(dft$geometry)
