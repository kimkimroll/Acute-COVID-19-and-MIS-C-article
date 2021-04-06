

###################################################################kim
#MAPS- acute covid
library(tidyverse)

cov_dat <- read_csv("data/Site_number_by_state_8_13_2020_kw.csv")
#cov_dat <- read_csv("data/Cleaned_dataset_all_pcr_antibody_pos2020-09-04.csv")

cov_dat1 <- cov_dat %>% rename (site_id = 'Site #',
                                names = 'State') %>%
                        mutate (abb = state.abb[match(cov_dat$State, state.name)],
                                 ) 
cov_dat1$site_id <- as.character(cov_dat1$site_id)


library(usmap)
library(maptools)
library(ggplot2)


#downloaded shape files from census.gov
#kshape <- readShapePoly("tl_2019_us_state/tl_2019_us_state.shp")

#kplot <-plot(kshape)

fips_st <- read_csv("data/state-geocodes-v2017.csv")
fips_st_acute <- fips_st %>%
  rename(fips = 'State (FIPS)') %>%
         mutate (abb = state.abb[match(fips_st$Name, state.name)],
) 


k_acute <- cov_dat1 %>% merge(fips_st_acute, by = c("abb"), all.y = TRUE)

k_acute <- k_acute %>%
  mutate(
    site_id = str_pad(site_id, 3, pad = "0")
  )
k_acute <- k_acute %>%
  rename(state = "Name")

library(tidyverse)
library(here)
library(eeptools)
library(janitor)
library(stringr)
#read in case data

dat_raw_acute <- read_csv("data/Cleaned_dataset_kim_acute_ddimer2020-09-01.csv")

#replace NA's with 0
dat_raw_acute = dat_raw_acute %>% dplyr::mutate(mis = replace_na(mis, 0))

#select only acute and sars_pcr_pos data
#dat_raw_acute <-dat_raw_acute %>% filter(mis == 0) %>% filter (sars_pcr_pos == 1) 
dat_raw_acute<-dat_raw_acute %>% mutate(
  
  age_cat_table = ifelse(age < 1, 1, ifelse(age < 5, 2, ifelse(age < 13, 3, ifelse(age < 22, 4, 5)))),
  

) %>% filter (age_cat_table == 1 | age_cat_table == 2 | age_cat_table == 3 | age_cat_table == 4)



dat_map <- dat_raw_acute %>%
  mutate(site_id = substring(id_short, 0, 3)
  ) 
dat_map1 <- dat_map %>%
  mutate(
    site_id = as.character(site_id)
  )

kmap_acute <- merge(k_acute, dat_map1, by = c("site_id"), all.x = TRUE)
kmap_acute <- kmap_acute %>%
#  rename(
#    fips = 'State (FIPS)'
#  ) %>%
  mutate(
    abb = as.character(abb)
  )


acute_k = kmap_acute %>%
  group_by(
    #fips, 
    names) %>%
  summarise(
    total_case = sum(sars_pcr_pos == 1, na.rm = TRUE)

  )
acute_k = acute_k %>%
      rename(state = 'names',
             value = 'total_case')
  
acute_k1 = merge(acute_k, k_acute, by = c("state"), all.y = TRUE)

#table_map <-  merge(fips_st_acute, acute_k, by = "fips", all.x = TRUE)     
#write.csv(table_map, paste0("data/kmap", Sys.Date(), ".csv", sep = ""))

#statebins maps----------------------------------------------------------------------------------01062021

library(statebins)
#dat <- data.frame(state=state.abb, value=0, stringsAsFactors=FALSE)
#dat[dat$state %in% c("AL", "NM", "SD"),]$value <- 1


  
#acute_k$state <- as.character(acute_k$state)  
#acute_k = acute_k[!(acute_k$state==""),] 
#acute_k = acute_k[!(acute_k$state=="DC"),] 
#acute_k = acute_k %>%
#  slice(1:(n()-1))
acute_k1$value<-as.numeric(acute_k1$value)
#acute_k1$value<-sub("NA", "0", acute_k1$value)

acute_k1 = acute_k1 %>%  mutate_if(is.numeric, ~replace(., . == 0, NA))
acute_k1$value[is.na(acute_k1$value)] <- 0
acute_k1$value<-as.numeric(acute_k1$value)

kbin<-statebins(
  acute_k1, 
  value_col = "value",
 # breaks = c('0', '40', '80', '100'),
 breaks = c(0, 20, 40, 60, 80, 100, 120),
 #labels = c("0-40", "0-40", "0-40", "40-80", "80-100", "100-120"),
  name = "# of cases",
  palette = "RdBu",
  font_size = 3,
  direction = -1,
  #round = TRUE
 ) +
 #labs(x="2001-2007") + 
  theme_statebins(legend_position="right")#+
  #facet_geo(~state, grid ="us_state_grid3")


kbin

ggsave("acute_map_kim2.png", dpi = 500, height = 8, width = 11 , units = "in")


#GEOFACET
library(geofacet)
kgeo<-ggplot(acute_k1, aes(x=state, y = value, fill=value))+
  geom_col(position = position_dodge()) +
               facet_geo(~ state)+
  #scale_fill_brewer(palette = "GnBu")+
  scale_y_continuous(breaks = c(20, 40, 60, 80, 100))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

kgeo
ggsave("acute_map_kim1_geofacet.png", dpi = 500, height = 8, width = 11 , units = "in")

##############################################################################################this works
kmap1 <- plot_usmap(regions = "states", data = acute_k, 
                    color = "black", 
                    labels = TRUE, 
                    label_color = "black",
                    values = "total_case"
                    ) + 
  #font("abb", size = 14, color = "red", face = "bold.italic")+
#  scale_fill_gradient2(low = "white", mid = "blue", midpoint = 5, high = "#2B7880",
  scale_fill_stepsn(#low= "white", high = "#2B7880",  
                         #values = c('0', '10', '25', '40', '55', '70'),
                         name = "# of cases", 
                        limits = c(0, 125), 
                         label = c('0', '', '20', '40', '60', '80', '125') , 
                         breaks = c(0, 1, 20, 40, 60, 80, 125),
                        #colors= c("white","#2E4A5B","#256B72","#2D8D7B", "#58AD76", "#96C96A"),
                    colors= c("#ffffff", "#cad2c5","#84a98c","#52796f","#354f52"),
                    guide = guide_colorsteps(even.steps = FALSE)
                      # guides(fill = guide_legend(nrow = 1))
                       
                         ) +
  #geom_label
  theme(legend.title = element_text(size = 10),
        legend.position = c(.60, .05),
        legend.direction = "horizontal") 

kmap1$layers[[2]]$aes_params$size <- 2
kmap1$layers[[2]]$aes_params$style <- "bold"

kmap1


ggsave("kmap_acute_map_k.png", dpi = 500, height = 8, width = 11 , units = "in")




###########################################################################################09-01-2020

#this works
