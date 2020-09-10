library(gridExtra)

library(tidyverse)
library(here)
library(eeptools)
library(janitor)
library(stringr)
library(ggplot2)
library(ggpubr)
library(ggpol)

library(ggplot2)

#"\\cdc.gov\project\CCID_NCIRD_DVD_PPLB\_PMDDL\Kim\R\taskforce\data\Cleaned_dataset_kim2020-08-19.csv"


dat_raw_acute <- read_csv("data/Cleaned_dataset_all_pcr_antibody_pos2020-09-04_bmi.csv")

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
                        lymp_nodes == 'Yes' |
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
        cracked_lips == 'Yes' , 'Yes', 'No'),
 
      sev_anemia = ifelse(hb_min >= 0 & hb_min <=7 & age < 5, 1, ifelse(hb_min >= 0 & hb_min <=8 & age >= 5, 1, 0)),
      
      lymph_neut = ifelse(lymphocyte_abs <1 & lymphocyte_abs >0 | neutrophil_abs < 0.5 & neutrophil_abs >0, 1, 0),
      hema_dys2 = ifelse(major_hema == 'Yes' | 
                           sev_anemia == 1 |
                           lymph_neut == 1 |
                           platelet_min < 50, 1, 0),
      hema_inv2 = ifelse(hema_dys2 == 1 |
                           deep_vein_thromb == 'Yes' |
                           pulm_emb == 'Yes' |
                           hemolysis == 'Yes' |
                           bleeding == 'Yes' |
                           isch_extremeity == 'Yes', 1, 0),
    
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
dat_raw_acute<-dat_raw_acute %>% mutate(
  
  age_cat_table = ifelse(age <= 12, 1, ifelse(age <21, 2, 3)),
  
) %>% filter (age_cat_table == 1 | age_cat_table == 2)

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
  group_by (age_cat_table) %>%

  
  mutate(
    pleur_eff = ifelse(pleur_eff1 == 'Yes' | pleur_eff2 == 'Yes', 1, ifelse(is.na(pleur_eff1) & is.na(pleur_eff2), NA, 0)),
    
    asth = ifelse(corticosteroids == 'Yes' | bronchospasm == 'Yes', 'Yes', 'No'),
    pulm_x = ifelse(pulm_infil_xray== 'Yes' | pulm_infil_xray_lab == 'Yes', 1, ifelse(is.na(pulm_infil_xray) & is.na(pulm_infil_xray_lab), NA, 0)),
    resp_dys = ifelse(pulm_infil_xray == 'Yes' |  
                        pulm_infil_xray_lab == 'Yes' |
                        lower_resp_inf == 'Yes' |
                        bronchospasm == 'Yes'|
                        pneumothorax  == 'Yes' | 
                        pleur_eff == 'Yes' |  
                        chest_tube== 'Yes' |  
                        pulm_hemorrhage == 'Yes' | 
                        ecmo == TRUE , 1, 0),
    
      gi_symp2 = ifelse(ab_pain == 'Yes' | nausea == 'Yes' | vomiting == 'Yes' | diarrhea == 'Yes', 'Yes', 'No'),
      gi_inv3 = ifelse(appendicitis == 'Yes' |
                         hepatitis == 'Yes' |
                         pancreatitis == 'Yes',
                       'Yes', 'No'),
    
    # ecmo
    #emco = ifelse(ecmo = 'Yes' | ecmo2 == 'Yes' | ecmo == 'Yes', 1, ifelse(is.na(emco) & is.na(emco2) & is.na(emco3), NA,0)),
    #ecmo = any(ecmo == 'Yes' | ecmo2 == 'Yes' | ecmo3 == 'Yes'),
    
    
    
  ) %>% 
  summarise(overall = n(),
            "Severe respiratory complications" = sum(resp_inv == 1, na.rm = TRUE),
            "Any respiratory support" = sum(resp_insuff_fail == 1, na.rm = TRUE),
            "Invasive mechanical ventilation" = sum(mech_vent == 1, na.rm = TRUE),
            "Pulmonary infiltrates on chest radiograph" = sum(pulm_x == 1, na.rm = TRUE),
            "Lower respiratory infection" = sum(lower_resp_inf == 'Yes', na.rm = TRUE),
            "Asthma exacerbation" = sum(asth == 'Yes', na.rm = TRUE),
            "Pleural effusion" = sum(pleur_eff == 1, na.rm = TRUE),
            "Pediatric Acute Respiratory Distress Syndrome" = sum(crit_pards == 'Yes', na.rm = TRUE),
            "Cardiovascular complications" = sum(cardiac_inv == 1, na.rm = TRUE),
            "Pericarditis or pericardial effusion" = sum(pericarditis == 'Yes', na.rm = TRUE),
            "Ejection fraction <30%" = sum(ejec_frac_min < 30, na.rm = TRUE),
            "Ejection fraction 30 to <55%" = sum(ejec_frac_min >= 30 & ejec_frac_min < 55, na.rm = TRUE),
            "Coronary artery aneurysm*" = sum(zscore == 1, na.rm = TRUE),
            "Arrythmia" = sum(dysrhythmia == 'Yes', na.rm = TRUE),
            "Vasopressor support or vasoactive infusions" = sum(shock == 'Yes' | vaso_inf_24 == 'Yes' | vaso_inf == 'Yes' | vasopressors == 1, na.rm = TRUE),
            "ECMO" = sum(ecmo == TRUE, na.rm = TRUE),
            "Hematologic complications" = sum(hema_inv2 == 1, na.rm = TRUE),
            "Neurologic complications" = sum(neuro_inv == 1, na.rm = TRUE),
            "Gastrointestinal complications" = sum(gi_inv3 == 'Yes', na.rm = TRUE)
            
            #"Pneumothorax or other signs of barotrauma" = sum(pneumothorax == 'Yes', na.rm = TRUE),
            #"Pulmonary hemorrhage" = sum(pulm_hemorrhage == 'Yes', na.rm = TRUE),
            #"Chest-tube or drainage required" = sum(chest_tube == 'Yes', na.rm = TRUE),
            #"Severe bronchospasm requiring continuous bronchodilators" = sum(bronchospasm == 'Yes', na.rm = TRUE)
                        

  ) 

table_k_acute_mis1 <- t(table_k_acute_mis1)



#percent
table_k_denom_mis1 <- dat_raw_acute %>% 
  filter(mis == 1) %>%
  group_by(age_cat_table) %>%
  summarise( overall = n(),
    "Severe respiratory complications" = overall,
             "Any respiratory support" = overall,
             "Invasive mechanical ventilation" = overall,
             "Pulmonary infiltrates on chest radiograph" = overall,
             "Lower respiratory infection" = overall,
             "Asthma exacerbation" = overall,
             "Pleural effusion" = overall,
             "Pediatric Acute Respiratory Distress Syndrome" = overall,
    "Cardiovascular complications" = overall,
    "Pericarditis or pericardial effusion" = overall,
    "Ejection fraction <30%" = overall,
    "Ejection fraction 30 to <55%" = overall,
    "Coronary artery aneurysm*" = overall,
    "Arrythmia" = overall,
    "Vasopressor support or vasoactive infusions" = overall,
    "ECMO" = overall,
    "Hematologic complications" = overall,
    "Neurologic complications" = overall,
    "Gastrointestinal complications" = overall
  ) 


table_k_denom_mis1 <- t(table_k_denom_mis1)


table_perc_mis1t <- round(table_k_acute_mis1/table_k_denom_mis1 * 100, digits = 0)[-1,]

table_perc_mis1<- as.data.frame(table_perc_mis1t)

#table_perc_mis1$mis <- 1
colnames(table_perc_mis1) <- c("Age 0 - 12 years", "Age 13-21 years")


############################################second mis 0 data
table_k_acute_mis0 <- dat_raw_acute %>% 
  filter(mis == 0) %>%
  group_by (age_cat_table) %>%
  
  
  mutate(
    pleur_eff = ifelse(pleur_eff1 == 'Yes' | pleur_eff2 == 'Yes' | crit_pards == 'Yes', 1, ifelse(is.na(pleur_eff1) & is.na(pleur_eff2), NA, 0)),
    asth = ifelse(corticosteroids == 'Yes' | bronchospasm == 'Yes', 'Yes', 'No'),
    pulm_x = ifelse(pulm_infil_xray== 'Yes' | pulm_infil_xray_lab == 'Yes', 1, ifelse(is.na(pulm_infil_xray) & is.na(pulm_infil_xray_lab), NA, 0)),
    resp_dys = ifelse(pulm_infil_xray == 'Yes' |  
                        pulm_infil_xray_lab == 'Yes' |
                        lower_resp_inf == 'Yes' |
                        bronchospasm == 'Yes'|
                        pneumothorax  == 'Yes' | 
                        pleur_eff == 'Yes' |  
                        chest_tube== 'Yes' |  
                        pulm_hemorrhage == 'Yes' | 
                        ecmo == TRUE , 1, 0),
      gi_symp2 = ifelse(ab_pain == 'Yes' | nausea == 'Yes' | vomiting == 'Yes' | diarrhea == 'Yes', 'Yes', 'No'),
      gi_inv3 = ifelse(appendicitis == 'Yes' |
                         hepatitis == 'Yes' |
                         pancreatitis == 'Yes',
                       'Yes', 'No')
    # ecmo
    #emco = ifelse(ecmo = 'Yes' | ecmo2 == 'Yes' | ecmo == 'Yes', 1, ifelse(is.na(emco) & is.na(emco2) & is.na(emco3), NA,0)),
    #ecmo = any(ecmo == 'Yes' | ecmo2 == 'Yes' | ecmo3 == 'Yes'),
    
    
    
  ) %>% 
  summarise(overall = n(),
    "Severe respiratory complications" = sum(resp_inv == 1, na.rm = TRUE),
            "Any respiratory support" = sum(resp_insuff_fail == 1, na.rm = TRUE),
            "Invasive mechanical ventilation" = sum(mech_vent == 1, na.rm = TRUE),
            "Pulmonary infiltrates on chest radiograph" = sum(pulm_x == 1, na.rm = TRUE),
            "Lower respiratory infection" = sum(lower_resp_inf == 'Yes', na.rm = TRUE),
            "Asthma exacerbation" = sum(asth == 'Yes', na.rm = TRUE),
            "Pleural effusion" = sum(pleur_eff == 1, na.rm = TRUE),
    "Pediatric Acute Respiratory Distress Syndrome" = sum(crit_pards == 'Yes', na.rm = TRUE),
    "Cardiovascular complications" = sum(cardiac_inv == 1, na.rm = TRUE),
    "Pericarditis or pericardial effusion" = sum(pericarditis == 'Yes', na.rm = TRUE),
    "Ejection fraction <30%" = sum(ejec_frac_min < 30, na.rm = TRUE),
    "Ejection fraction 30 to <55%" = sum(ejec_frac_min >= 30 & ejec_frac_min < 55, na.rm = TRUE),
    "Coronary artery aneurysm*" = sum(zscore == 1, na.rm = TRUE),
    "Arrythmia" = sum(dysrhythmia == 'Yes', na.rm = TRUE),
    "Vasopressor support or vasoactive infusions" = sum(shock == 'Yes' | vaso_inf_24 == 'Yes' | vaso_inf == 'Yes' | vasopressors == 1, na.rm = TRUE),
    "ECMO" = sum(ecmo == TRUE, na.rm = TRUE),
    "Hematologic complications" = sum(hema_inv == 1, na.rm = TRUE),
    "Neurologic complications" = sum(neuro_inv == 1, na.rm = TRUE),
    "Gastrointestinal complications" = sum(gi_inv3 == 'Yes', na.rm = TRUE)
            #"Pneumothorax or other signs of barotrauma" = sum(pneumothorax == 'Yes', na.rm = TRUE),
            #"Pulmonary hemorrhage" = sum(pulm_hemorrhage == 'Yes', na.rm = TRUE),
            #"Chest-tube or drainage required" = sum(chest_tube == 'Yes', na.rm = TRUE),
            #"Severe bronchospasm requiring continuous bronchodilators" = sum(bronchospasm == 'Yes', na.rm = TRUE)

            
            
  ) 

table_k_acute_mis0 <- t(table_k_acute_mis0)



#percent
table_k_denom_mis0 <- dat_raw_acute %>% 
  filter(mis == 0) %>%
  group_by(age_cat_table) %>%
            summarise( overall = n(),
              "Severe respiratory complications" = overall,
                       "Any respiratory support" = overall,
                       "Invasive mechanical ventilation" = overall,
                       "Pulmonary infiltrates on chest radiograph" = overall,
                       "Lower respiratory infection" = overall,
                       "Asthma exacerbation" = overall,
                       "Pleural effusion" = overall,
                       "Pediatric Acute Respiratory Distress Syndrome" = overall,
              "Cardiovascular complications" = overall,
              "Pericarditis or pericardial effusion" = overall,
              "Ejection fraction <30%" = overall,
              "Ejection fraction 30 to <55%" = overall,
              "Coronary artery aneurysm*" = overall,
              "Arrythmia" = overall,
              "Vasopressor support or vasoactive infusions" = overall,
              "ECMO" = overall,
              "Hematologic complications" = overall,
              "Neurologic complications" = overall,
              "Gastrointestinal complications" = overall
                       #"Pneumothorax or other signs of barotrauma" = sum(total),
                       #"Pulmonary hemorrhage" = sum(total),
                       #"Chest-tube or drainage required" = sum(total),
                       #"Severe bronchospasm requiring continuous bronchodilators" = sum(total)
            
            
            
  ) 


table_k_denom_mis0 <- t(table_k_denom_mis0)

table_perc_mis0t <- round(table_k_acute_mis0/table_k_denom_mis0 * 100, digits = 0)[-1,]


table_perc_mis0<- as.data.frame(table_perc_mis0t)

#table_perc_mis0$mis <- 0
colnames(table_perc_mis0) <- c("Age 0 - 12 years", "Age 13-21 years")

dat_plot_comb <- full_join(table_perc_mis0, table_perc_mis1, by = "mis")


colnames(table_perc_comb) <- c("Age <1 years", "Age 1-5 years", "Age 6-12 years", "Age 13-21 years")



#table_perc_card <- rownames(table_k_acute_mis1)

#table_perc_k <- data.frame(lapply(table_perc_card, function(x) gsub("0 [(]NaN[])]", " ", x)), stringsAsFactors = FALSE)

#table_perc_card <- data.frame(lapply(table_perc_card, function(x) gsub("0 [(]NaN[])]", " ", x)), stringsAsFactors = FALSE)

#colnames(table_combined_card) <- c("Overall")


source('scripts/epicurve_theme.R')
dat_plot_acute_mis1 = as.data.frame(table_perc_mis1) %>% 
  rownames_to_column() %>%
  
  gather("Age Group", "percentage", 2:3)  %>%
  mutate(`Age Group` = factor(`Age Group`, levels = c("Age 0 - 12 years", "Age 13-21 years")),
         #`mis` = factor(mis ==, levels = c(0, 0, 0, 0, 1, 1, 1, 1)),
         rowname = factor(rowname, levels = c(
           "Gastrointestinal complications", 
           "Neurologic complications",
           "Hematologic complications",
           "ECMO",
           "Vasopressor support or vasoactive infusions",
           "Arrythmia" ,
           "Coronary artery aneurysm*",
           "Ejection fraction 30 to <55%",
           "Ejection fraction <30%",
           "Pericarditis or pericardial effusion",
           "Cardiovascular complications",
           "Severe respiratory complications",
           "Any respiratory support",
           "Invasive mechanical ventilation",
           "Pulmonary infiltrates on chest radiograph",
           "Lower respiratory infection",
           "Asthma exacerbation",
           "Pleural effusion",
           "Pediatric Acute Respiratory Distress Syndrome"
           #"Pneumothorax or other signs of barotrauma" = sum(total),
           #"Pulmonary hemorrhage" = sum(total),
           #"Chest-tube or drainage required" = sum(total),
           #"Severe bronchospasm requiring continuous bronchodilators" = sum(total)
           
           
         )))


dat_plot_acute_mis0 = as.data.frame(table_perc_mis0) %>% 
  rownames_to_column() %>%
  
  gather("Age Group", "percentage", 2:3)  %>%
  mutate(`Age Group` = factor(`Age Group`, levels = c("Age 0 - 12 years", "Age 13-21 years")),
         #`mis` = factor(mis ==, levels = c(0, 0, 0, 0, 1, 1, 1, 1)),
    rowname = factor(rowname, levels = c(
      "Gastrointestinal complications", 
      "Neurologic complications",
      "Hematologic complications",
      "ECMO",
      "Vasopressor support or vasoactive infusions",
      "Arrythmia" ,
      "Coronary artery aneurysm*",
      "Ejection fraction 30 to <55%",
      "Ejection fraction <30%",
      "Pericarditis or pericardial effusion",
      "Cardiovascular complications",
      "Severe respiratory complications",
      "Any respiratory support",
      "Invasive mechanical ventilation",
      "Pulmonary infiltrates on chest radiograph",
      "Lower respiratory infection",
      "Asthma exacerbation",
      "Pleural effusion",
      "Pediatric Acute Respiratory Distress Syndrome"
      #"Pneumothorax or other signs of barotrauma" = sum(total),
      #"Pulmonary hemorrhage" = sum(total),
      #"Chest-tube or drainage required" = sum(total),
      #"Severe bronchospasm requiring continuous bronchodilators" = sum(total)
      
      
    )))


graph_report_ka1 = 
  
  ggplot() +
  
  geom_bar(data = dat_plot_acute_mis1, aes(x= rowname, y = -percentage, fill = `Age Group`), 
           position = 'dodge',
           stat = 'identity'
           ) +
#  geom_bar(data = dat_plot_acute_mis0, aes(x= rowname, y = -percentage , fill = `Age Group`), 
#           position = 'dodge', 
#           stat = 'identity'
#  ) +
  
  #scale_fill_gradient(low = "white", high = "#5f0f40", name = "Percent") +
  
  scale_fill_manual("Age Group", values = c("Age 0 - 12 years" = "#ef8354",
                                            "Age 13-21 years" = "#bfc0c0"
                                            ),
                    labels = c(paste0("Age 0 - 12 years (N=", sum(dat_raw_acute$age_cat_table == 1, na.rm = TRUE), ")", sep = ""),
                               paste0("Age 13-21 years (N=", sum(dat_raw_acute$age_cat_table == 2, na.rm = TRUE), ")", sep = ""))) + 
  
  
  geom_text(data = dat_plot_acute_mis1, aes(x = rowname, y = -percentage, label=ifelse(percentage > 0, percentage, ""), fill = `Age Group`), vjust = 0.5, hjust = 1.25, position=position_dodge(width=0.9), size = 3) + 
  
  
  scale_x_discrete(limits = c(
    "Gastrointestinal complications", 
    "Neurologic complications",
    "Hematologic complications",
    "ECMO",
    "Vasopressor support or vasoactive infusions",
    "Arrythmia" ,
    "Coronary artery aneurysm*",
    "Ejection fraction 30 to <55%",
    "Ejection fraction <30%",
    "Pericarditis or pericardial effusion",
    "Cardiovascular complications",
    "Pediatric Acute Respiratory Distress Syndrome",
    "Pleural effusion",
    "Asthma exacerbation",
    "Lower respiratory infection",
    "Pulmonary infiltrates on chest radiograph",
    "Invasive mechanical ventilation",
    "Any respiratory support",
    "Severe respiratory complications")
  )+
  #epicurve_theme +
  scale_y_continuous(labels = c(100, 75, 50, 25, 0), limits = c(-100, 0) ) +
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
    geom_bar(data = dat_plot_acute_mis0, aes(x= rowname, y = percentage , fill = `Age Group`), 
             position = position_dodge(.9), 
             stat = 'identity'
    ) +
  
  #scale_fill_gradient(low = "white", high = "#5f0f40", name = "Percent") +
  
  scale_fill_manual("Age Group", values = c("Age 13-21 years" = "#bfc0c0",
                                            "Age 0 - 12 years" = "#ef8354"
                                            ),
  labels = c(paste0("Age 13-21 years (N=", sum(dat_raw_acute$age_cat_table == 2, na.rm = TRUE), ")", sep = ""),
             paste0("Age 0 - 12 years (N=", sum(dat_raw_acute$age_cat_table == 1, na.rm = TRUE), ")", sep = "")),
  limits = c("Age 13-21 years",
             "Age 0 - 12 years"
             
             )
  ) + 
  #guides(fill = guide_legend(reverse = TRUE))+
  
  
  geom_text(data = dat_plot_acute_mis0, aes(x = rowname, y = percentage, label=ifelse(percentage > 0, percentage, ""), fill = `Age Group`), vjust = 0.5, hjust = -0.25, position=position_dodge(width=0.9), size = 3) + 
  
  
  scale_x_discrete(limits = c(
    "Gastrointestinal complications", 
    "Neurologic complications",
    "Hematologic complications",
    "ECMO",
    "Vasopressor support or vasoactive infusions",
    "Arrythmia" ,
    "Coronary artery aneurysm*",
    "Ejection fraction 30 to <55%",
    "Ejection fraction <30%",
    "Pericarditis or pericardial effusion",
    "Cardiovascular complications",
    "Pediatric Acute Respiratory Distress Syndrome",
    "Pleural effusion",
    "Asthma exacerbation",
    "Lower respiratory infection",
    "Pulmonary infiltrates on chest radiograph",
    "Invasive mechanical ventilation",
    "Any respiratory support",
    "Severe respiratory complications")
  )+
    #epicurve_theme +
  #scale_y_continuous(trans = "reverse") +
  theme_minimal()+
  
  theme(
      axis.title.y=element_blank(),
      axis.text.y=element_text(size = 10, face= c(
        "bold",
        "bold",
        "bold",
        "plain",
        "plain",
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
        "plain",
        "plain",
        "bold"), hjust = 0.5),
      axis.ticks.y=element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
        legend.position = "none") +
  #ylim(0, 100) + 
  
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

ggsave('figures/Figure_2B_pyramid_hema2.png', plot = k_acute_pyramid, dpi = 500, height = 10, width = 16)

ggsave('figures/Figure_2B_pyramid_hema2.pdf', plot = k_acute_pyramid, dpi = 1500, height = 12, width = 14)
  
  
  
  
  
  
  
  
  
  

