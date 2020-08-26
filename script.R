getwd()




# load packages -----------------------------------------------------------

library(tools) 
library(USAboundaries)
library(sf)
library(janitor)
library(dataRetrieval)
library(leaflet)
library(leaflet.extras)
library(knitr)
library(shiny)
library(lubridate)
library(tidyverse)
library(tidylog)



# figure out USGS gages with longest periods or record for stream temperature nationwide: --------

## parameterCd == 00010 Temperature, water

## code recommendation from Laura DeCicco (@DeCiccoDonk) on 2019-08-12 ------

start_time <- Sys.time()

all_sites <- data.frame()
threshold <- 7300 # try 20 years of daily records for threshold
for(st in stateCd$STUSAB) {
  what_data <- whatNWISdata(stateCd = st,
                            parameterCd='00010')
  
  what_data_filtered <- what_data %>%
    filter(data_type_cd == 'dv', #daily values
           stat_cd == '00003', #daily mean
           count_nu > threshold) %>%
    
    arrange(desc(count_nu)) %>%
    
    select(site_no, station_nm, 
           dec_long_va, 
           dec_lat_va, 
           begin_date, 
           end_date, 
           count_nu)
  
  all_sites <- bind_rows(all_sites, what_data_filtered)
}

end_time <- Sys.time()

## run time
end_time - start_time # Time difference of  16.90203 mins for 20 yrs


# with threshold of 10 years, pulled 779 sites
# with threshold of 20 years, pulled 254 sites

all_sites_tb <- 
  all_sites %>% 
  as_tibble() %>% 
  mutate(count_yr = count_nu/365)

all_sites_tb
dim(all_sites_tb) # 842   8
summary(all_sites_tb)
length(unique(all_sites_tb$site_no)) # 842 but only 816 unique site nos (10 yrs); 254 but only 245 unique site nos (20 yrs)





# download USGS temp data -------------------------------------------------

# first get site info
site_numbers <- unique(all_sites_tb$site_no)

length(site_numbers) # 816; 245

# join state abbrev and name
head(stateCd)


# save site info to a variable so we can look at it later
site_info <- readNWISsite(site_numbers) %>% 
  as_tibble() %>% 
  select(site_no, station_nm, drain_area_va, dec_lat_va, dec_long_va, state_cd, county_cd, huc_cd, tz_cd) %>%
  left_join(stateCd, by=setNames('STATE', 'state_cd')) %>% # join state abbrev and name
  select(-STATENS) %>% 
  mutate(
    station_nm = tools::toTitleCase(tolower(station_nm)), # make station name title case
    station_nm = str_replace_all(station_nm, c(' c ' = ' C ', ' r ' = ' R '))) # capitalize C and R

site_info
dim(site_info) # 817; 245   11

#check for dups
site_info %>% 
  get_dupes(station_nm)





# #use data retrieval package:
# # define start and end dates
start_date <- "1900-01-01" # earliest available appears to be 1964-10-01
end_date <- Sys.Date() #through today



# # look at site info
site_info$station_nm
site_info$site_no

# #codes for parameters of interest : flow (many others available)
readNWISpCode(c('00060','00010'))
parameter_code <- "00010" # water temperature in C

# ### obtain usgs flow data 

# # #ADF retrieval of USGS data, use the readNWISdv function
start_time <- Sys.time()
gagedata <- readNWISdv(site_numbers, # trying all 816 at once... fingers crossed i got some chores lined up
                       parameter_code,
                       start_date, end_date)
end_time <- Sys.time()

## run time
end_time - start_time # Time difference of 8.431316 hours  -- but Error: cannot allocate vector of size 40.2 Mb; for 20y Time difference of 1.164481 hours



# The names of the columns are based on the parameter and statistic codes.
# In many cases, you can clean up the names with a convenience function renameNWISColumns:
head(gagedata)
dim(gagedata) # 2,424,654      51
sort(names(gagedata))
# since water temp is what we're after and the majority of the records are for Wtemp (94%), dropping everything else

temp_df <- 
  gagedata %>% 
  as_tibble() %>% 
  renameNWISColumns() %>% 
  select(agency_cd, site_no, Date, Wtemp, Wtemp_cd) %>% # select only necessary cols
  drop_na(Wtemp) %>%
  filter(Wtemp_cd == 'A') %>% # keep only approved data 
  filter(Wtemp > -1 & Wtemp < 40) %>%  # keep only believable temps (only 5 obs >-1 & <40)
  mutate(year = year(Date))


temp_df

summary(temp_df)
str(temp_df)
dim(temp_df) # 2,246,419       6
length(unique(temp_df$site_no)) # 233 unique sites after filtering

hist(temp_df$Wtemp)




# calculate yearly temps --------------------------------------------------


#first prepare site info df to join

site_info




Wtempstats <-
  temp_df %>% 
  group_by(site_no, year) %>% 
  summarize(
    N    = sum(!is.na(Wtemp)),
    min = min(Wtemp, na.rm = TRUE),
    max = max(Wtemp, na.rm = TRUE),
    median = median(Wtemp, na.rm = TRUE),
    mean = mean(Wtemp, na.rm = TRUE),
    sd   = sd(Wtemp, na.rm = TRUE),
    minDate = min(Date, na.rm = TRUE),
    maxDate = max(Date, na.rm = TRUE)) %>%
  
  mutate(no_months = N/30) %>%

  left_join(site_info, by='site_no') %>%

  filter(no_months >= 11) %>%  # keep only site-years with > 11 months of data
  
  ungroup() %>% 
  
  group_by(station_nm) %>%
  
  mutate(n_year = length(year),
         state_site = paste0(STUSAB, ': ', station_nm)) %>%
  
  filter(n_year >= 10) %>%  # keep only stations with >= 10 years of data
  
  arrange(STUSAB, station_nm, year) 



Wtempstats
summary(Wtempstats)
str(Wtempstats)
dim(Wtempstats) # 5007   23  after keeping only sites with >= 11 months & >= 10 years of data 
length(unique(Wtempstats$site_no)) # 224 unique sites after filtering
unique(Wtempstats$state_site)



# create rds and re-import so Rmd doesn't take forever and need parent files
saveRDS(Wtempstats, 'Wtempstats.rds')










# stations with longest PORs
Wtempstats %>%
  group_by(site_no, station_nm) %>%
  summarize(years = length(mean),
            minDate = min(minDate),
            maxDate = max(maxDate)) %>%
  arrange(-years)



 
# # example plots -----------------------------------------------------------

Wtempstats %>%  
  filter(station_nm == 'Truckee Rv Nr Sparks, Nv') %>% 

ggplot(aes(x=year, y=1, fill=mean)) +
  geom_bar(color=NA, width=1, stat="identity") +
  scale_fill_distiller(type= "div", palette="RdBu") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ggtitle('Truckee Rv Nr Sparks, Nv') +
  labs(fill="Mean Yearly Temperature (C)", x='Year') +
  theme_minimal() +
  theme(legend.position = "bottom", axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5), legend.key.width = unit(3,"line"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggsave('Truckee Rv Nr Sparks, Nv.png', width = 10, height = 7, units = "in", dpi = 300)




# # whilst cold systems in the great white north are very cold by comparison, but are still getting warmer recently

Wtempstats %>% 
  filter(station_nm == 'Terror R at Mouth Nr Kodiak Ak') %>% 

ggplot(aes(x=year, y=1, fill=mean)) +
  geom_bar(color=NA, width=1, stat="identity") +
  scale_fill_distiller(type= "div", palette="RdBu") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ggtitle('Terror R at Mouth Nr Kodiak Ak') +
  labs(fill="Mean Yearly Temperature (C)", x='Year') +
  theme_minimal() +
  theme(legend.position = "bottom", axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5), legend.key.width = unit(3,"line"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggsave('Terror R at Mouth Nr Kodiak Ak.png', width = 10, height = 7, units = "in", dpi = 300)





# warm system in the southern US that has definitely gotten warmer since ~ 1980

Wtempstats %>% 
  filter(station_nm == 'w Fk Trinity Rv at Grand Prairie, Tx') %>% 

ggplot(aes(x=year, y=1, fill=mean)) +
  geom_bar(color=NA, width=1, stat="identity") +
  scale_fill_distiller(type= "div", palette="RdBu") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ggtitle('W Fk Trinity Rv at Grand Prairie, TX') +
  labs(fill="Mean Yearly Temperature (C)", x='Year') +
  theme_minimal() +
  theme(legend.position = "bottom", axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5), legend.key.width = unit(3,"line"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggsave('w Fk Trinity Rv at Grand Prairie, Tx.png', width = 10, height = 7, units = "in", dpi = 300)


# site directly downstream of a hypolimnetic reservoir that is remarkably consistent


Wtempstats %>% 
  filter(station_nm == 'West Branch Delaware River at Stilesville Ny') %>% 

ggplot(aes(x=year, y=1, fill=mean)) +
  geom_bar(color=NA, width=1, stat="identity") +
  scale_fill_distiller(type= "div", palette="RdBu") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ggtitle('West Branch Delaware River at Stilesville Ny') +
  labs(fill="Mean Yearly Temperature (C)", x='Year') +
  theme_minimal() +
  theme(legend.position = "bottom", axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5), legend.key.width = unit(3,"line"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggsave('West Branch Delaware River at Stilesville Ny.png', width = 10, height = 7, units = "in", dpi = 300)



### clear example of warming

Wtempstats %>% 
  filter(station_nm == 'Broad River Near Carlisle, Sc') %>% 

ggplot(aes(x=year, y=1, fill=mean)) +
  geom_bar(color=NA, width=1, stat="identity") +
  scale_fill_distiller(type= "div", palette="RdBu") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ggtitle('Broad River Near Carlisle, Sc') +
  labs(fill="Mean Yearly Temperature (C)", x='Year') +
  theme_minimal() +
  theme(legend.position = "bottom", axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5), legend.key.width = unit(3,"line"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggsave('Broad River Near Carlisle, Sc.png', width = 10, height = 7, units = "in", dpi = 300)



### clear example of warming

Wtempstats %>% 
  filter(station_nm == 'Madison River Bl Ennis Lake Nr Mcallister Mt') %>% 

ggplot(aes(x=year, y=1, fill=mean)) +
  geom_bar(color=NA, width=1, stat="identity") +
  scale_fill_distiller(type= "div", palette="RdBu") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ggtitle('Madison River Bl Ennis Lake Nr Mcallister Mt') +
  labs(fill="Mean Yearly Temperature (C)", x='Year') +
  theme_minimal() +
  theme(legend.position = "bottom", axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5), legend.key.width = unit(3,"line"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggsave('Madison River Bl Ennis Lake Nr Mcallister Mt.png', width = 10, height = 7, units = "in", dpi = 300)


### clear example of warming

Wtempstats %>% 
  filter(station_nm == 'Flathead River at Columbia Falls Mt') %>% 

ggplot(aes(x=year, y=1, fill=mean)) +
  geom_bar(color=NA, width=1, stat="identity") +
  scale_fill_distiller(type= "div", palette="RdBu") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ggtitle('Flathead River at Columbia Falls Mt') +
  labs(fill="Mean Yearly Temperature (C)", x='Year') +
  theme_minimal() +
  theme(legend.position = "bottom", axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5), legend.key.width = unit(3,"line"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggsave('Flathead River at Columbia Falls Mt.png', width = 10, height = 7, units = "in", dpi = 300)


# header image

Wtempstats %>% 
  filter(station_nm == 'Flathead River at Columbia Falls Mt') %>% 
  
  ggplot(aes(x=year, y=1, fill=mean)) +
  geom_bar(color=NA, width=1, stat="identity") +
  scale_fill_distiller(type= "div", palette="RdBu") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_minimal() +
  theme(legend.position = "none", axis.title.y = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        plot.title = element_blank(), axis.title = element_blank())

ggsave('header.png', width = 10, height = 0.5, units = "in", dpi = 300)


