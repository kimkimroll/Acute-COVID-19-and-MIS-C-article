library(gridExtra)

library(tidyverse)
library(here)
library(eeptools)
library(janitor)
library(stringr)
library(ggplot2)

library(ggpol)

#"\\cdc.gov\project\CCID_NCIRD_DVD_PPLB\_PMDDL\Kim\R\taskforce\data\Cleaned_dataset_kim2020-08-19.csv"


dat_raw_acute <- read_csv("data/Cleaned_dataset_all_pcr_antibody_pos2020-09-04.csv")

#replace NA's with 0
dat_raw_acute = dat_raw_acute %>% dplyr::mutate(mis = replace_na(mis, 0))

#select only acute and sars_pcr_pos data
#dat_raw_acute <-dat_raw_acute %>% filter(mis == 0) %>% filter (sars_pcr_pos == 1) 

#select only '0' data
dat_raw_acute <-dat_raw_acute %>% 
  mutate( 
    bmi_er = as.numeric(bmi_er),
    bmi_er = if_else(underlying_conds_obesity2 == '0', round(bmi_er, digits = 1), 0),
    mus_jo = if_else(muscle_ache == 'Yes' | joint_pain == 'Yes', 'Yes', 'No'),
    covt_sym = if_else(#covid_toes == 'Yes' |
                        lymph_nodes == 'Yes' |
                         cracked_lips == 'Yes' | 
                         erythema == 'Yes' |
                         edema == 'Yes' |
                         conjunc == 'Yes' |
                         periungual == 'Yes',
                       'Yes', 'No'),
    symp_other = if_else(
      eye_p == 'Yes' |
        lymph_nodes == 'Yes' ,
      #             peri_gang == 'Yes', 
      'Yes', 'No'),
    symp_t = if_else(
      fever == 'Yes' |
        chills == 'Yes' |
        Cough == 'Yes' |
        short_breath ==  'Yes' |
        chest_p == 'Yes' |
        ab_pain == 'Yes' |
        nausea == 'Yes' |
        vomiting == 'Yes' |
        diarrhea == 'Yes' |
        fatigue_t == 'Yes' |
        atl_aware == 'Yes' |
        Headache == 'Yes' |
        mus_jo == 'Yes' |
        skin_rash == 'Yes' |
        lsmell == 'Yes' |
        ltaste == 'Yes' |
        Wheezing == 'Yes' |
        sore_t == 'Yes' |
        rhin_cong == 'Yes' |
        new_dif == 'Yes' |
        Headache == 'Yes' |
        muscle_ache == 'Yes' |
        joint_pain == 'Yes' |
        Seizure == 'Yes' |
        eye_p == 'Yes' |
        conjunc == 'Yes' |
        skin_rash == 'Yes' |
        #            lymph_nodes == 'Yes' |
        covid_toes == 'Yes' |
        erythema == 'Yes' |
        edema == 'Yes' |
        periungual == 'Yes' |
        peri_gang == 'Yes' |
        cracked_lips == 'Yes' , 'Yes', 'No')
    
  ) %>%
  #filter(mis == 0) %>% 
  filter (#sars_pcr_pos == 1 & 
    sars_pos == 'Antibody' | 
      sars_pos == 'PCR' 
  ) %>%
  filter(
    symp_t == 'Yes'
  )

#select only  sars_pcr_pos data
#dat_raw_acute <-dat_raw_acute %>% filter (sars_pcr_pos == 1)


#set "-6" to NA
#dat_raw_acute[dat_raw_acute == "-6"]<-NA




#sort into AGE categories
#dat_raw_acute<-dat_raw_acute %>% mutate(
  
#  age_cat_table = ifelse(age < 1, 1, ifelse(age < 5, 2, ifelse(age < 13, 3, ifelse(age < 22, 4, 5)))),
  
#) %>% filter (age_cat_table == 1 | age_cat_table == 2 | age_cat_table == 3 | age_cat_table == 4
#              )
#%>% filter(mis == 0)
#%>% filter(mis == 0)


#write_csv(dat_raw_acute, paste0("data/Cleaned_dataset_kim_acute_l", Sys.Date(), ".csv", sep = "") )
#8-27-2020


#as.Date for admission and onset date
dat_raw_acute$admission_date<-as.Date(dat_raw_acute$admission_date, "%m/%d/%Y")
dat_raw_acute$onset_date<-as.Date(dat_raw_acute$onset_date, "%m/%d/%Y")
#as numeric data / 365.2




# count final number of patients included in table 1
total = tally(dat_raw_acute)

#table_n_acute$iqr_m[table_n_acute$iqr_m < 0]<-0

table_k_acute_mis1 <- dat_raw_acute %>% 
  filter(mis == 1) %>%
  #group_by (age_cat_table) %>%

  
  summarise(overall = n(),
            "Constitutional" = sum(fatigue_t == 'Yes' | 
                                     mus_jo == 'Yes' |
                                     chills == 'Yes' |
                                     new_dif == 'Yes' |
                                   fever == 'Yes' , na.rm = TRUE), 
            "   Fever" = sum(fever == 'Yes', na.rm = TRUE),
            "   Fatigue" = sum(fatigue_t == 'Yes', na.rm = TRUE),
            "   Muscle ache/Joint pain" = sum(mus_jo == 'Yes', na.rm = TRUE),
            "   Chills/rigors" = sum(chills == 'Yes', na.rm=TRUE),
            "   Difficulty walking or crawling" = sum(new_dif == 'Yes', na.rm = TRUE), 
            "Gastrointestinal" = sum(nausea == 'Yes' |
                                       vomiting == 'Yes' |
                                       ab_pain == 'Yes' |
                                       diarrhea == 'Yes', na.rm = TRUE),           
            "   Nausea/loss of appetite" = sum(nausea == 'Yes', na.rm = TRUE),
            "   Vomiting" =  sum(vomiting == 'Yes', na.rm = TRUE),
            "   Abdominal pain " = sum(ab_pain == 'Yes', na.rm = TRUE),
            "   Diarrhea" = sum(diarrhea == 'Yes', na.rm = TRUE),            
            "Upper Respiratory" = sum(rhin_cong == 'Yes' |
                                        sore_t == 'Yes', na.rm = TRUE),
            "   Rhinorrhea/congestion" = sum(rhin_cong == 'Yes', na.rm = TRUE),
            "   Sore Throat" = sum(sore_t == 'Yes', na.rm = TRUE),
            "Lower Respiratory" = sum(Cough == 'Yes' |
                                        short_breath == 'Yes' |
                                        chest_p == 'Yes'|
                                        Wheezing == 'Yes'| 
                                        low_chest == 'Yes', na.rm=TRUE),
            "   Cough" = sum(Cough == 'Yes', na.rm=TRUE),
            "   Shortness of breath" = sum(short_breath == 'Yes', na.rm=TRUE),
            "   Chest pain" = sum(chest_p == 'Yes', na.rm=TRUE),
            "   Wheezing" = sum(Wheezing == 'Yes', na.rm=TRUE),
            "   Lower chest wall indrawing" = sum(low_chest == 'Yes', na.rm = TRUE),            
            "Neurologic" = sum(Headache == 'Yes' |
                                 atl_aware == 'Yes' |
                                 seizures == 'Yes' |
                               lsmell == 'Yes' |
                               ltaste == 'Yes', na.rm = TRUE),            
            "   Headache" = sum(Headache == 'Yes', na.rm = TRUE),
            "   Altered awareness/confusion" = sum(atl_aware == 'Yes', na.rm = TRUE),
            "   Seizures" = sum(seizures == 'Yes', na.rm = TRUE),
            "   Loss of smell" = sum(lsmell == 'Yes', na.rm = TRUE),
            "   Loss of taste" = sum(ltaste == 'Yes', na.rm = TRUE),  
            "Mucocutaneous" = sum(skin_rash == 'Yes' |
                                    covt_sym == 'Yes', na.rm = TRUE),
            "   Rash" = sum(skin_rash == 'Yes', na.rm = TRUE),
            "   Kawasaki disease symptoms*" = sum(covt_sym == 'Yes', na.rm = TRUE)
                        

  ) 

table_k_acute_mis1 <- t(table_k_acute_mis1)



#percent
table_k_denom_mis1 <- dat_raw_acute %>% 
  filter(mis == 1) %>%
  #group_by(age_cat_table) %>%
  summarise(overall = n(),
            "Constitutional" = overall,
            "   Fever" = overall,
            "   Fatigue" = overall,
            "   Muscle ache/Joint pain" = overall,
            "   Chills/rigors" = overall, 
            "   Difficulty walking or crawling" = overall,
            "Gastrointestinal" = overall,            
            "   Nausea/loss of appetite" = overall,
            "   Vomiting" =  overall,
            "   Abdominal pain " = overall,
            "   Diarrhea" = overall,        
            "Upper Respiratory" = overall,
            "   Rhinorrhea/congestion" = overall,
            "   Sore Throat" = overall,
            "Lower Respiratory" = overall,
            "   Cough" = overall,
            "   Shortness of breath" = overall,
            "   Chest pain" = overall, 
            "   Wheezing" = overall,
            "   Lower chest wall indrawing" = overall,            
            "Neurologic" = overall,           
            "   Headache" = overall,
            "   Altered awareness/confusion" = overall,
            "   Seizures" = overall,
            "   Loss of smell" = overall,
            "   Loss of taste" = overall,
            "Mucocutaneous" = overall,
            "   Rash" = overall,
            "   Kawasaki disease symptoms*" = overall
            
            
            
  ) 


table_k_denom_mis1 <- t(table_k_denom_mis1)


table_perc_mis1t <- round(table_k_acute_mis1/table_k_denom_mis1 * 100, digits = 0)[-1,]

table_perc_mis1<- as.data.frame(table_perc_mis1t)

#table_perc_mis1$mis <- 1
colnames(table_perc_mis1) <- c("percentage")


############################################second mis 0 data
table_k_acute_mis0 <- dat_raw_acute %>% 
  filter(mis == 0) %>%
  #group_by (age_cat_table) %>%
  
  
  summarise(overall = n(),
            "Constitutional" = sum(fatigue_t == 'Yes' | 
                                     mus_jo == 'Yes' |
                                     chills == 'Yes' |
                                     new_dif == 'Yes' |
                                     fever == 'Yes' , na.rm = TRUE), 
            "   Fever" = sum(fever == 'Yes', na.rm = TRUE),
            "   Fatigue" = sum(fatigue_t == 'Yes', na.rm = TRUE),
            "   Muscle ache/Joint pain" = sum(mus_jo == 'Yes', na.rm = TRUE),
            "   Chills/rigors" = sum(chills == 'Yes', na.rm=TRUE),
            "   Difficulty walking or crawling" = sum(new_dif == 'Yes', na.rm = TRUE), 
            "Gastrointestinal" = sum(nausea == 'Yes' |
                                       vomiting == 'Yes' |
                                       ab_pain == 'Yes' |
                                       diarrhea == 'Yes', na.rm = TRUE),           
            "   Nausea/loss of appetite" = sum(nausea == 'Yes', na.rm = TRUE),
            "   Vomiting" =  sum(vomiting == 'Yes', na.rm = TRUE),
            "   Abdominal pain " = sum(ab_pain == 'Yes', na.rm = TRUE),
            "   Diarrhea" = sum(diarrhea == 'Yes', na.rm = TRUE),            
            "Upper Respiratory" = sum(rhin_cong == 'Yes' |
                                        sore_t == 'Yes', na.rm = TRUE),
            "   Rhinorrhea/congestion" = sum(rhin_cong == 'Yes', na.rm = TRUE),
            "   Sore Throat" = sum(sore_t == 'Yes', na.rm = TRUE),
            "Lower Respiratory" = sum(Cough == 'Yes' |
                                        short_breath == 'Yes' |
                                        chest_p == 'Yes'|
                                        Wheezing == 'Yes'| 
                                        low_chest == 'Yes', na.rm=TRUE),
            "   Cough" = sum(Cough == 'Yes', na.rm=TRUE),
            "   Shortness of breath" = sum(short_breath == 'Yes', na.rm=TRUE),
            "   Chest pain" = sum(chest_p == 'Yes', na.rm=TRUE),
            "   Wheezing" = sum(Wheezing == 'Yes', na.rm=TRUE),
            "   Lower chest wall indrawing" = sum(low_chest == 'Yes', na.rm = TRUE),            
            "Neurologic" = sum(Headache == 'Yes' |
                                 atl_aware == 'Yes' |
                                 seizures == 'Yes' |
                                 lsmell == 'Yes' |
                                 ltaste == 'Yes', na.rm = TRUE),            
            "   Headache" = sum(Headache == 'Yes', na.rm = TRUE),
            "   Altered awareness/confusion" = sum(atl_aware == 'Yes', na.rm = TRUE),
            "   Seizures" = sum(seizures == 'Yes', na.rm = TRUE),
            "   Loss of smell" = sum(lsmell == 'Yes', na.rm = TRUE),
            "   Loss of taste" = sum(ltaste == 'Yes', na.rm = TRUE),  
            "Mucocutaneous" = sum(skin_rash == 'Yes' |
                                    covt_sym == 'Yes', na.rm = TRUE),
            "   Rash" = sum(skin_rash == 'Yes', na.rm = TRUE),
            "   Kawasaki disease symptoms*" = sum(covt_sym == 'Yes', na.rm = TRUE)
            
            
  ) 

table_k_acute_mis0 <- t(table_k_acute_mis0)



#percent
table_k_denom_mis0 <- dat_raw_acute %>% 
  filter(mis == 0) %>%
  #group_by(age_cat_table) %>%
  summarise(overall = n(),
            "Constitutional" = overall,
            "   Fever" = overall,
            "   Fatigue" = overall,
            "   Muscle ache/Joint pain" = overall,
            "   Chills/rigors" = overall, 
            "   Difficulty walking or crawling" = overall,
            "Gastrointestinal" = overall,            
            "   Nausea/loss of appetite" = overall,
            "   Vomiting" =  overall,
            "   Abdominal pain " = overall,
            "   Diarrhea" = overall,        
            "Upper Respiratory" = overall,
            "   Rhinorrhea/congestion" = overall,
            "   Sore Throat" = overall,
            "Lower Respiratory" = overall,
            "   Cough" = overall,
            "   Shortness of breath" = overall,
            "   Chest pain" = overall, 
            "   Wheezing" = overall,
            "   Lower chest wall indrawing" = overall,            
            "Neurologic" = overall,           
            "   Headache" = overall,
            "   Altered awareness/confusion" = overall,
            "   Seizures" = overall,
            "   Loss of smell" = overall,
            "   Loss of taste" = overall,
            "Mucocutaneous" = overall,
            "   Rash" = overall,
            "   Kawasaki disease symptoms*" = overall
            
            
            
  ) 


table_k_denom_mis0 <- t(table_k_denom_mis0)

table_perc_mis0t <- round(table_k_acute_mis0/table_k_denom_mis0 * 100, digits = 0)[-1,]


table_perc_mis0<- as.data.frame(table_perc_mis0t)

#table_perc_mis0$mis <- 0
colnames(table_perc_mis0) <- c("percentage")

dat_plot_comb <- full_join(table_perc_mis0, table_perc_mis1, by = "mis")


colnames(table_perc_comb) <- c("Age <1 years", "Age 1-5 years", "Age 6-12 years", "Age 13-21 years")



#table_perc_card <- rownames(table_k_acute_mis1)

#table_perc_k <- data.frame(lapply(table_perc_card, function(x) gsub("0 [(]NaN[])]", " ", x)), stringsAsFactors = FALSE)

#table_perc_card <- data.frame(lapply(table_perc_card, function(x) gsub("0 [(]NaN[])]", " ", x)), stringsAsFactors = FALSE)

#colnames(table_combined_card) <- c("Overall")


  



source('scripts/epicurve_theme.R')
dat_plot_acute_mis1 = as.data.frame(table_perc_mis1) %>% 
  rownames_to_column() %>%
  
  #gather("percentage", 0:1)  %>%
  mutate(#`Age Group` = factor(`Age Group`, levels = c("Age <1 years", "Age 1-5 years", "Age 6-12 years", "Age 13-21 years")),
         #`mis` = factor(mis ==, levels = c(0, 0, 0, 0, 1, 1, 1, 1)),
         rowname = factor(rowname, levels = c(
           "Constitutional",
           "   Fever",
           "   Fatigue",
           "   Muscle ache/Joint pain",
           "   Chills/rigors",
           "   Difficulty walking or crawling", 
           "Gastrointestinal",          
           "   Nausea/loss of appetite",
           "   Vomiting", 
           "   Abdominal pain ", 
           "   Diarrhea",         
           "Upper Respiratory",
           "   Rhinorrhea/congestion", 
           "   Sore Throat",
           "Lower Respiratory",
           "   Cough",
           "   Shortness of breath",
           "   Chest pain", 
           "   Wheezing", 
           "   Lower chest wall indrawing",       
           "Neurologic",    
           "   Headache",
           "   Altered awareness/confusion", 
           "   Seizures",
           "   Loss of smell",
           "   Loss of taste", 
           "Mucocutaneous",
           "   Rash",
           "   Kawasaki disease symptoms*"
           
           
         )))


dat_plot_acute_mis0 = as.data.frame(table_perc_mis0) %>% 
  rownames_to_column() %>%
  
  #gather("Age Group", "percentage", 2:5)  %>%
  mutate(#`Age Group` = factor(`Age Group`, levels = c("Age <1 years", "Age 1-5 years", "Age 6-12 years", "Age 13-21 years")),
         #`mis` = factor(mis ==, levels = c(0, 0, 0, 0, 1, 1, 1, 1)),
    rowname = factor(rowname, levels = c(
      "Constitutional",
      "   Fever",
      "   Fatigue",
      "   Muscle ache/Joint pain",
      "   Chills/rigors",
      "   Difficulty walking or crawling", 
      "Gastrointestinal",          
      "   Nausea/loss of appetite",
      "   Vomiting", 
      "   Abdominal pain ", 
      "   Diarrhea",         
      "Upper Respiratory",
      "   Rhinorrhea/congestion", 
      "   Sore Throat",
      "Lower Respiratory",
      "   Cough",
      "   Shortness of breath",
      "   Chest pain", 
      "   Wheezing", 
      "   Lower chest wall indrawing",       
      "Neurologic",    
      "   Headache",
      "   Altered awareness/confusion", 
      "   Seizures",
      "   Loss of smell",
      "   Loss of taste", 
      "Mucocutaneous",
      "   Rash",
      "   Kawasaki disease symptoms*"
      
      
    )))


graph_report_ka1 = 
  
  ggplot() +
  
  geom_bar(data = dat_plot_acute_mis1, aes(x= rowname, y = -percentage, fill = `rowname`), 
           position = position_dodge(-.9), 
           stat = 'identity',
           show.legend=FALSE
           ) +
#  geom_bar(data = dat_plot_acute_mis0, aes(x= rowname, y = -percentage , fill = `Age Group`), 
#           position = 'dodge', 
#           stat = 'identity'
#  ) +
  
#  scale_fill_gradient(low = "white", high = "#006d77", name = "Percent", limits = c(0, 100)) +


      scale_fill_manual(values = c(
        "   Kawasaki disease symptoms*" = "#7C7C7E",
        "   Rash"= "#7C7C7E",
        "Mucocutaneous"= "#7C7C7E",
        "   Loss of taste"= "#403A82",
        "   Loss of smell"= "#403A82",
        "   Seizures"= "#403A82",
        "   Altered awareness/confusion"= "#403A82", 
        "   Headache"= "#403A82",
        "Neurologic"= "#403A82",
        "   Lower chest wall indrawing"= "#FDFA5C",    
        "   Wheezing"= "#FDFA5C",
        "   Chest pain"= "#FDFA5C",
        "   Shortness of breath"= "#FDFA5C",
        "   Cough"= "#FDFA5C",
        "Lower Respiratory"= "#FDFA5C",
        "   Sore Throat"= "#BFD6F9",
        "   Rhinorrhea/congestion"= "#BFD6F9",
        "Upper Respiratory"= "#BFD6F9",
        "   Diarrhea"= "#FA8124",
        "   Abdominal pain "= "#FA8124",
        "   Vomiting"= "#FA8124",
        "   Nausea/loss of appetite"= "#FA8124",
        "Gastrointestinal"= "#FA8124",
        "   Difficulty walking or crawling"= "royalblue1",
        "   Chills/rigors"= "royalblue1",
        "   Muscle ache/Joint pain"= "royalblue1",
        "   Fatigue"= "royalblue1",
        "   Fever"= "royalblue1",
        "Constitutional"= "royalblue1"
    )) +
                                            
  
  
  geom_text(data = dat_plot_acute_mis1, aes(x = rowname, y = -percentage, label=ifelse(percentage > 0, percentage, ""), fill = rowname), vjust = 0.5, hjust = 1.25, position=position_dodge(width=0.9), size = 3) + 
  
  
  scale_x_discrete(limits = c(
    "   Kawasaki disease symptoms*",
    "   Rash",
    "Mucocutaneous",
    "   Loss of taste",
    "   Loss of smell",
    "   Seizures",
    "   Altered awareness/confusion", 
    "   Headache",
    "Neurologic",     
    "   Lower chest wall indrawing",    
    "   Wheezing", 
    "   Chest pain",
    "   Shortness of breath",
    "   Cough",
    "Lower Respiratory",
    "   Sore Throat",
    "   Rhinorrhea/congestion",
    "Upper Respiratory",
    "   Diarrhea",  
    "   Abdominal pain ", 
    "   Vomiting",     
    "   Nausea/loss of appetite",
    "Gastrointestinal",  
    "   Difficulty walking or crawling",
    "   Chills/rigors",
    "   Muscle ache/Joint pain",
    "   Fatigue",
    "   Fever",
    "Constitutional"
    ))+
  #epicurve_theme +
  scale_y_continuous(labels = c(100, 75, 50, 25, 0), 
                     breaks = c(-100, -75, -50, -25, 0),
                     limits = c(-100, 0) ) +
  theme_minimal()+
  
  theme(
    plot.title = element_text(hjust = 1),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
        legend.position = "none") +
  #ylim(-50, 0) + 
  
  labs(y = "Percentage",
       x = " ",
       subtitle = paste0(""),
       caption = "")+
  
  # Title
  ggtitle("MIS-C patients") +
  
  #butterfly effect

  coord_flip() 


graph_report_ka1

graph_report_ka0 = 
  
  ggplot() +
  
#  geom_bar(data = dat_plot_acute_mis1, aes(x= rowname, y = percentage, fill = `Age Group`), 
#           position = 'dodge', 
#           stat = 'identity'
#  ) +
    geom_bar(data = dat_plot_acute_mis0, aes(x= rowname, y = percentage , fill = `rowname`), 
             position = position_dodge(-.9), 
             stat = 'identity',
             show.legend=FALSE
    ) +
  
  #scale_fill_gradient(low = "white", high = "#006d77", name = "Percent", limits=c(0,100)) +  
  scale_fill_manual(values = c(
    "   Kawasaki disease symptoms*" = "#7C7C7E",
    "   Rash"= "#7C7C7E",
    "Mucocutaneous"= "#7C7C7E",
    "   Loss of taste"= "#403A82",
    "   Loss of smell"= "#403A82",
    "   Seizures"= "#403A82",
    "   Altered awareness/confusion"= "#403A82", 
    "   Headache"= "#403A82",
    "Neurologic"= "#403A82",
    "   Lower chest wall indrawing"= "#FDFA5C",    
    "   Wheezing"= "#FDFA5C",
    "   Chest pain"= "#FDFA5C",
    "   Shortness of breath"= "#FDFA5C",
    "   Cough"= "#FDFA5C",
    "Lower Respiratory"= "#FDFA5C",
    "   Sore Throat"= "#BFD6F9",
    "   Rhinorrhea/congestion"= "#BFD6F9",
    "Upper Respiratory"= "#BFD6F9",
    "   Diarrhea"= "#FA8124",
    "   Abdominal pain "= "#FA8124",
    "   Vomiting"= "#FA8124",
    "   Nausea/loss of appetite"= "#FA8124",
    "Gastrointestinal"= "#FA8124",
    "   Difficulty walking or crawling"= "royalblue1",
    "   Chills/rigors"= "royalblue1",
    "   Muscle ache/Joint pain"= "royalblue1",
    "   Fatigue"= "royalblue1",
    "   Fever"= "royalblue1",
    "Constitutional"= "royalblue1"
    ))+

  
  
geom_text(data = dat_plot_acute_mis0, aes(x = rowname, y = percentage, label=ifelse(percentage > 0, percentage, ""), fill = -rowname), vjust = 0.5, hjust = -0.25, position=position_dodge(width=0.9), size = 3) + 
  
  
  scale_x_discrete(limits = c(
    "   Kawasaki disease symptoms*",
    "   Rash",
    "Mucocutaneous",
    "   Loss of taste",
    "   Loss of smell",
    "   Seizures",
    "   Altered awareness/confusion", 
    "   Headache",
    "Neurologic",     
    "   Lower chest wall indrawing",    
    "   Wheezing", 
    "   Chest pain",
    "   Shortness of breath",
    "   Cough",
    "Lower Respiratory",
    "   Sore Throat",
    "   Rhinorrhea/congestion",
    "Upper Respiratory",
    "   Diarrhea",  
    "   Abdominal pain ", 
    "   Vomiting",     
    "   Nausea/loss of appetite",
    "Gastrointestinal",  
    "   Difficulty walking or crawling",
    "   Chills/rigors",
    "   Muscle ache/Joint pain",
    "   Fatigue",
    "   Fever",
    "Constitutional"

    )
  )+
    #epicurve_theme +
  #scale_y_continuous(trans = "reverse") +
  theme_minimal()+
  
  theme(
      axis.title.y=element_blank(),
      axis.text.y=element_text(size = 10, face= c(
        "plain",
        "plain",
        "bold", 
        "plain",
        "plain",
        "plain",
        "plain",
        "plain",
        "bold",
        "plain",
        "plain",
        "plain",
        "plain",  
        "plain",                                        
        "bold",
        "plain",
        "plain",
        "bold",
        "plain",
        "plain",
        "plain",
        "plain",
        "bold",
        "plain",
        "plain",
        "plain",
        "plain",
        "plain",
        "bold"
        
        
        
        
        ), hjust = 0.5),
      axis.ticks.y=element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
        legend.position = "none") +
  ylim(0, 100) + 
  
  labs(y = "Percentage",
       x = " ",
       subtitle = paste0(""),
       caption = "")+
  
  # Title
  ggtitle("Non MIS-C patients") +

  
  #butterfly effect
  # facet_grid(rows = vars(mis))+
  #dir = "h", 
  #scales = "free",
  #reverse_num = TRUE) +
  coord_flip() 
  
graph_report_ka0


#arrange pyramid

k_acute_pyramid = ggarrange(graph_report_ka1, graph_report_ka0, ncol = 2, common.legend = TRUE, legend = "bottom", align = "h")
#k_acute_pyramid = annotate_figure(k_acute_pyramid,
#                                  top = text_grob("Pyramid plot", color = "black", face = "bold", size = 14)
#)


k_acute_pyramid

ggsave('figures/Figure1_pyramid.png', plot = k_acute_pyramid, dpi = 500, height = 10, width = 16)

ggsave('figures/Figure1_pyramid.pdf', plot = k_acute_pyramid, dpi = 1500, height = 12, width = 14)
  
  
  
  
  
  
  
  
  
  

