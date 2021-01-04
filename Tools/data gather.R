library(tidyverse)
library(openxlsx)


excursions_conv <- function(n){
  
  x = ifelse(n <= 11, 2, qbinom(0.90, n, 0.10, lower.tail = TRUE)+1 )
  return(x)
}

make_char <- function(df){
  df_2 <- df %>%
    mutate(count_excursion = as.character(count_excursion),
           Pollu_ID = as.character(Pollu_ID)) 
  
  
}

rename_param <- function(df){
  df2 <- df %>%
    mutate(Pollu_ID = as.character(Pollu_ID)) %>%
    left_join(Pollutants, by = "Pollu_ID") %>%
    mutate(Char_Name = ifelse(!is.na(Pollutant_DEQ.WQS), Pollutant_DEQ.WQS, Char_Name )) %>%
    select(AU_ID, MLocID, Pollu_ID, Char_Name, count_excursion )
}

con <- DBI::dbConnect(odbc::odbc(), "IR 2018")



Pollutants <- DBI::dbReadTable(con, 'LU_Pollutant') %>%
  mutate(Pollu_ID = as.character(Pollu_ID)) %>%
  select(-SSMA_TimeStamp) %>%
  mutate(Pollutant_DEQ.WQS = trimws(Pollutant_DEQ.WQS, which = "right"))

load("E:/Documents/2018-2020_IR_Database/data/IR_data.Rdata")


z_DO_cont_spawn <- DO_cont_spawn %>%
  mutate(abs_min = ifelse(IRResultNWQSunit < 9, 1, 0)) %>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,Excursion) %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = sum(Excursion)) %>%
  mutate(Period = "Spawn") %>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,Period, count_excursion ) %>%
  make_char()


z_DO_cont_yearround <- DO_cont_yearround %>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,Excursion) %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = sum(Excursion)) %>%
  mutate(Period = "Year round") %>%
  select(AU_ID,MLocID, Pollu_ID, Char_Name,Period, count_excursion ) %>%
  make_char()


z_DO_estuary_yearround <- DO_estuary_yearround %>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,excursion) %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = sum(excursion)) %>%
  mutate(Period = "Year round") %>%
  select(AU_ID,MLocID, Pollu_ID, Char_Name,Period, count_excursion ) %>%
  make_char()

z_DO_inst_yearround <- DO_inst_yearround %>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,Excursion) %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = sum(Excursion)) %>%
  mutate(Period = "Year round") %>%
  select(AU_ID,MLocID, Pollu_ID, Char_Name,Period, count_excursion ) %>%
  make_char()

z_DO_instant_spawn <- DO_instant_spawn %>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,Excursion) %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = sum(Excursion)) %>%
  mutate(Period = "Spawn") %>%
  select(AU_ID,MLocID, Pollu_ID, Char_Name,Period, count_excursion ) %>%
  make_char()


z_temp_year_round <- temp %>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,year_round_Excursion) %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = sum(year_round_Excursion)) %>%
  mutate(Period = "Year round") %>%
  select(AU_ID,MLocID, Pollu_ID, Char_Name,Period, count_excursion ) %>%
  make_char()




z_temp_spawning <- temp %>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,Spawn_Excursion.1) %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = sum(Spawn_Excursion.1)) %>%
  mutate(Period = "Spawn") %>%
  select(AU_ID,MLocID, Pollu_ID, Char_Name,Period, count_excursion ) %>%
  make_char()


z_Tox_AL_Ammonia <- Tox_AL_Ammonia%>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,excursion) %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = sum(excursion)) %>%
  select(AU_ID,MLocID, Pollu_ID, Char_Name, count_excursion ) %>%
  make_char() %>%
  rename_param() %>%
  mutate(Char_Name = paste0(Char_Name, "- Aquatic Life"))
z_Tox_AL_CU <- Tox_AL_CU %>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,excursion) %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = sum(excursion)) %>%
  select(AU_ID,MLocID, Pollu_ID, Char_Name, count_excursion ) %>%
  make_char() %>%
  rename_param() %>%
  mutate(Char_Name = paste0(Char_Name, "- Aquatic Life"))

Z_Tox_AL_Hardness_Metals <- Tox_AL_Hardness_Metals %>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,excursion) %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = sum(excursion)) %>%
  select(AU_ID,MLocID, Pollu_ID, Char_Name, count_excursion ) %>%
  make_char() %>%
  rename_param() %>%
  mutate(Char_Name = paste0(Char_Name, "- Aquatic Life"))

z_Tox_AL_Hardness_Metals <- Tox_AL_Hardness_Metals %>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,excursion) %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = sum(excursion)) %>%
  select(AU_ID,MLocID, Pollu_ID, Char_Name, count_excursion ) %>%
  make_char() %>%
  rename_param() %>%
  mutate(Char_Name = paste0(Char_Name, "- Aquatic Life"))

z_Tox_AL_Others <- Tox_AL_Others %>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,excursion) %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = sum(excursion)) %>%
  select(AU_ID,MLocID, Pollu_ID, Char_Name, count_excursion ) %>%
  make_char() %>%
  rename_param() %>%
  mutate(Char_Name = paste0(Char_Name, "- Aquatic Life"))

z_Tox_AL_Penta <- Tox_AL_Penta %>%
  select(AU_ID, MLocID, Pollu_ID, Char_Name,excursion) %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = sum(excursion)) %>%
  select(AU_ID,MLocID, Pollu_ID, Char_Name, count_excursion ) %>%
  make_char() %>%
  rename_param() %>%
  mutate(Char_Name = paste0(Char_Name, "- Aquatic Life"))



# Bacteria ----------------------------------------------------------------


z_bacteria_coast_contact <- bacteria_coast_contact %>%
  group_by(AU_ID) %>%
  # list out the maxium geometric mean per AU
  mutate(OWRD_Basin = first(OWRD_Basin), 
            Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
            # maximum percentage of results within geomean groups with more 10 samples that are above criteria 
            max.perc_above_crit_10 =  ifelse(!all(is.na(perc_above_crit_10)),max(perc_above_crit_10, na.rm = TRUE),NA),
            # maximum percentage of results within geomean groups with more 5 samples that are above criteria 
            max.n_samples_greater_perc_crit = ifelse(all(is.na(n_samples_greater_perc_crit)), NA, max(n_samples_greater_perc_crit, na.rm= TRUE)),
            # percent of samples that do not have 5 or more samples in each 90 day period. 
            #Used to determine if a 90 day geomean is even possible for cat 3 or cat 3b
            perc.insuff = sum(less_5)/n(),
            #Maximum value of AU group. Used for 3 or 3b
            max.value  = max(Result_cen),
            SS_Crit = max(SS_Crit),
            Geomean_Crit = max(Geomean_Crit),
            Perc_Crit = max(Perc_Crit)) %>%
  mutate(excursion = ifelse(Result_cen > 130, 1,0),
         IR_method = case_when(!is.na(Max_Geomean) & Max_Geomean > Geomean_Crit ~ "geomean",
                               !is.na(max.perc_above_crit_10) & max.perc_above_crit_10 > 0.10 ~ "percent",
                               !is.na(max.n_samples_greater_perc_crit) & max.n_samples_greater_perc_crit >= 2 ~ "percent",
                               TRUE ~ "Not impaired"
         )) %>%
  ungroup() %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = case_when(first(IR_method) == "geomean" ~ "Contributes to geomean",
                                        first(IR_method) == "percent" ~ as.character(sum(excursion)),
                                        TRUE ~ "Not Impaired"
                                        ))%>%
  make_char()



z_bacteria_fresh_contact <- bacteria_fresh_contact %>%
  mutate(excursion = ifelse(Result_cen > SS_Crit, 1,0)) %>%
  group_by(AU_ID) %>%
  # list out the maxium geometric mean per AU
  mutate(OWRD_Basin = first(OWRD_Basin), 
            Max_Geomean = ifelse(!all(is.na(geomean)),max(geomean, na.rm = TRUE),NA),
            max.value  = max(Result_cen),
            num_Samples = as.numeric(n()),
            num_ss_excursions = as.numeric(sum(Result_cen > SS_Crit)),
            critical_excursions = excursions_conv(num_Samples),
            SS_Crit = max(SS_Crit),
            Geomean_Crit = max(Geomean_Crit)) %>%
  mutate(IR_method = case_when(!is.na(Max_Geomean) & Max_Geomean > Geomean_Crit ~ "geomean",
                               num_Samples >= 5 & num_ss_excursions > critical_excursions ~ "percent",
                               TRUE ~  "Not impaired"
                               )) %>%
  ungroup() %>%
  group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
  summarise(count_excursion = case_when(first(IR_method) == "geomean" ~ "Contributes to geomean",
                                        first(IR_method) == "percent" ~ as.character(sum(excursion)),
                                        TRUE ~ "Not Impaired"
  ))%>%
  make_char()
  
  

  z_bacteria_Shell_harvest <-  bacteria_Shell_harvest %>%
    mutate(excursion = ifelse(Result_cen > Perc_Crit, 1, 0)) %>%
    group_by(AU_ID, OWRD_Basin) %>%
    mutate(num_samples = n(),
              median = ifelse(num_samples >= 5, median(Result_cen), NA ),
              num_exceed = sum(perc_exceed),
              Perc_Crit = first(Perc_Crit),
              SS_Crit = first(SS_Crit)
    ) %>%
    mutate(IR_method = case_when(!is.na(median) & median > SS_Crit ~ "geomean",
                                 (num_samples >= 10 & num_exceed/num_samples > 0.10) |
                                   (num_samples >= 5 & num_samples <= 9 &  num_exceed >= 1 ) ~ "percent",
                                 TRUE ~ "Not Impaired")) %>%
    ungroup() %>%
    group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
    summarise(count_excursion = case_when(first(IR_method) == "geomean" ~ "Contributes to median",
                                          first(IR_method) == "percent" ~ as.character(sum(excursion)),
                                          TRUE ~ "Not Impaired"
    ))%>%
    make_char()
    

# chl ---------------------------------------------------------------------

z_chl <- chl %>%
    mutate(excursion = ifelse(Result_cen > Chla_Criteria, 1, 0)) %>%
    group_by(AU_ID, Chla_Criteria) %>%
    mutate(OWRD_Basin = first(OWRD_Basin),
              Char_Name = first(Char_Name),
              num_samples = as.numeric(n()),
              num_ss_excursions = as.numeric(sum(Result_cen > Chla_Criteria)),
              critical_excursions = excursions_conv(n()),
              max_result = max(Result_cen),
              max_mo_avg = max(monthaverage),
              max_3_mo_avg = max(avg.3.mo, na.rm = TRUE)) %>%
    mutate(max_3_mo_avg = ifelse(is.infinite(max_3_mo_avg), NA, max_3_mo_avg )) %>%
    mutate(IR_method = case_when(!is.na(max_3_mo_avg) & max_3_mo_avg > Chla_Criteria ~ "Month average")) %>%
    ungroup() %>%
    group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
    summarise(count_excursion = case_when(first(IR_method) == "Month average" ~ "Contributes to 3 month average",
                                          TRUE ~ as.character(sum(excursion))))%>%
    make_char()
  

# pH ----------------------------------------------------------------------

z_pH <- pH %>%
    group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
    summarise(pH_high_excursions = sum(pH_excursion_high),
              pH_low_excursions = sum(pH_excursion_low)) %>%
    mutate(count_excursion = paste(pH_high_excursions, "high pH excursions;", pH_low_excursions, "low pH excursions")) %>%
    select(AU_ID,MLocID, Pollu_ID, Char_Name, count_excursion )%>%
    make_char()
    
  

# tox_HH ------------------------------------------------------------------

  z_Tox_HH <- Tox_HH %>%
    select(AU_ID, MLocID, Pollu_ID, Char_Name) %>%
    group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
    summarise(count_excursion = "Contributes to geomean") %>%
    mutate(Pollu_ID = as.character(Pollu_ID)) %>%
    left_join(Pollutants, by = "Pollu_ID") %>%
    mutate(Char_Name = ifelse(!is.na(Pollutant_DEQ.WQS), Pollutant_DEQ.WQS, Char_Name )) %>%
    mutate(Char_Name = paste0(Char_Name, "- Human Health")) %>%
    select(AU_ID,MLocID, Pollu_ID, Char_Name, count_excursion )%>%
    make_char()
  
  
  z_Tox_HH_Hg_tissue <- Tox_HH_Hg_tissue %>%
    mutate(Pollu_ID = "109") %>%
    select(AU_ID, MLocID, Pollu_ID, Char_Name) %>%
    group_by(AU_ID,MLocID, Pollu_ID, Char_Name) %>%
    summarise(count_excursion = "Contributes to geomean") %>%
    mutate(Pollu_ID = as.character(Pollu_ID)) %>%
    left_join(Pollutants, by = "Pollu_ID") %>%
    mutate(Char_Name = ifelse(!is.na(Pollutant_DEQ.WQS), Pollutant_DEQ.WQS, Char_Name )) %>%
    mutate(Char_Name = paste0(Char_Name, "- Human Health")) %>%
    select(AU_ID,MLocID, Pollu_ID, Char_Name, count_excursion )%>%
    make_char()
    
    
  
 
# Put it together ---------------------------------------------------------


all_together <- bind_rows(z_DO_cont_yearround,z_DO_estuary_yearround,
                          z_DO_inst_yearround, z_DO_instant_spawn,
                          z_temp_spawning, z_temp_year_round,z_Tox_AL_Ammonia, 
                          z_Tox_AL_CU, #z_Tox_AL_Hardness_Metals, 
                          Z_Tox_AL_Hardness_Metals, 
                          z_Tox_AL_Others, z_bacteria_coast_contact,
                          z_bacteria_fresh_contact, z_bacteria_Shell_harvest,
                          z_chl,z_pH, z_Tox_HH,  z_Tox_HH_Hg_tissue)


data_wide <- all_together %>%
  distinct(AU_ID, MLocID,Pollu_ID,Period, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(assessment = ifelse(!is.na(Period), 
                             paste0(Char_Name, "- ", Period, " excursions"), 
                             paste(Char_Name, "excursions" ))) %>%
  select(-Pollu_ID, -Char_Name, -Period) %>%
  mutate(AU_Type = case_when(grepl("SR", AU_ID) ~ "River and Stream",
                             grepl("WS", AU_ID) ~ "Watershed",
                             grepl("LK", AU_ID) ~ "Lake/Reservoir",
                             grepl("CL", AU_ID) ~ "Coastline",
                             grepl("OC", AU_ID) ~ "Ocean",
                             grepl("EB", AU_ID) ~ "Estuary/Bay",
                             )) %>%
  pivot_wider(names_from = assessment, values_from = count_excursion) 


con <- DBI::dbConnect(odbc::odbc(), "STATIONS")

stations_db <- tbl(con, "VWStationsFinal") %>%
  select(OrgID, MLocID, Lat_DD, Long_DD, GNIS_Name, Reachcode) %>%
  collect()



data_wide <- data_wide %>%
  left_join(stations_db)

save(data_wide, file = "data/wide_data.Rdata")




