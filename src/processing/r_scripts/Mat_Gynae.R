# Operative laparoscopy------------------------------------------------------

hvlc_gynae_op_filter<- function(input_df){
  
  input_df %>%

  filter(Age >= 17, 
          (Main_Specialty_Code %in% c(500,502) | Treatment_Function_Code %in% c(502,503)),
          (Admission_Method %in% c(11,12,13) | Patient_Classification == 2),
          (substr(Der_Procedure_All, 1,4) %in% c("Q201",
                                                "Q362",
                                                "Q381",
                                                "Q382",
                                                "Q388",
                                                "Q389",
                                                "Q413",
                                                "Q521",
                                                "Q522")|
           substr(Der_Procedure_All, 1,3) %in% c("Q39",
                                                 "Q49",
                                                 "Q50",
                                                 "T42")),
         !grepl("Q383", Der_Procedure_All),
         !grepl("C51|C52|C53|C54|C55|C56|C57|C796|D06|D070|D071|D072|D073", Der_Diagnosis_All)
  )
}

hvlc_gynae_op_flag <- function(input_df){
  
  input_df %>%
    mutate("Operative_laparoscopy_flag" = case_when(
      Age >= 17 &
          (Main_Specialty_Code %in% c(500,502) | Treatment_Function_Code %in% c(502,503)) &
          (Admission_Method %in% c(11,12,13) | Patient_Classification == 2) &
          (substr(Der_Procedure_All, 1,4) %in% c("Q201",
                                                "Q362",
                                                "Q381",
                                                "Q382",
                                                "Q388",
                                                "Q389",
                                                "Q413",
                                                "Q521",
                                                "Q522")|
           substr(Der_Procedure_All, 1,3) %in% c("Q39",
                                                 "Q49",
                                                 "Q50",
                                                 "T42")) &
         !grepl("Q383", Der_Procedure_All) &
         !grepl("C51|C52|C53|C54|C55|C56|C57|C796|D06|D070|D071|D072|D073", Der_Diagnosis_All) ~ 1,
      TRUE ~ 0
    )
    )%>%
    arrange(desc(Operative_laparoscopy_flag))
  }

# Laparoscopic hysterectomy------------------------------------------------------           

hvlc_gynae_lh_filter <- function(input_df){
  
  input_df %>%

  filter(Age >= 17, 
         (Main_Specialty_Code %in% c(500,502) | Treatment_Function_Code %in% c(502,503)),
         (Admission_Method %in% c(11,12,13) | Patient_Classification == 2),
         grepl("Q07|Q08", Der_Procedure_All),
         grepl("Y751|Y752", Der_Procedure_All),
         !grepl("C51|C52|C53|C54|C55|C56|C57|C796|D06|D070|D071|D072|D073", Der_Diagnosis_All)
  )
}

hvlc_gynae_lh_flag <- function(input_df){
  
  input_df %>%
    mutate("Laparoscopic_hysterectomy_flag" = case_when(
      Age >= 17 &
      (Main_Specialty_Code %in% c(500,502) | Treatment_Function_Code %in% c(502,503)) &
      (Admission_Method %in% c(11,12,13) | Patient_Classification == 2) &
      grepl("Q07|Q08", Der_Procedure_All) &
      grepl("Y751|Y752", Der_Procedure_All) &
      !grepl("C51|C52|C53|C54|C55|C56|C57|C796|D06|D070|D071|D072|D073", Der_Diagnosis_All) ~ 1,
      TRUE ~ 0
    )
    )%>%
    arrange(desc(Laparoscopic_hysterectomy_flag))
  }
      
# Endometrial ablation------------------------------------------------------           

hvlc_gynae_eb_filter <- function(input_df){
  
  input_df %>%

  filter(Age >= 17, 
         (Main_Specialty_Code %in% c(500,502) | Treatment_Function_Code %in% c(502,503)),
         (Admission_Method %in% c(11,12,13) | Patient_Classification == 2),
         grepl("Q162|Q163|Q164|Q165|Q166|Q176|Q177", Der_Procedure_All),
         !grepl("C51|C52|C53|C54|C55|C56|C57|C796|D06|D070|D071|D072|D073", Der_Diagnosis_All)
  )
}

hvlc_gynae_eb_flag <- function(input_df){
  
  input_df %>%
    mutate("Endometrial_ablation_flag" = case_when(
      Age >= 17 &
      (Main_Specialty_Code %in% c(500,502) | Treatment_Function_Code %in% c(502,503)) &
      (Admission_Method %in% c(11,12,13) | Patient_Classification == 2) &
      grepl("Q162|Q163|Q164|Q165|Q166|Q176|Q177", Der_Procedure_All) &
      !grepl("C51|C52|C53|C54|C55|C56|C57|C796|D06|D070|D071|D072|D073", Der_Diagnosis_All) ~ 1,
      TRUE ~ 0
    )
    )%>%
    arrange(desc(Endometrial_ablation_flag))
}
    
# Hysteroscopy------------------------------------------------------

hvlc_gynae_hyst_filter <- function(input_df){
  
  input_df %>%

  filter(Age >= 17, 
         (Main_Specialty_Code %in% c(500,502) | Treatment_Function_Code %in% c(502,503)),
         (substr(Der_Procedure_All, 1,4) %in% c("Q161",
                                                "Q167",
                                                "Q168",
                                                "Q169",
                                                "Q171",
                                                "Q172",
                                                "Q173",
                                                "Q174",
                                                "Q175",
                                                "Q178",
                                                "Q179")|
            substr(Der_Procedure_All, 1,3) %in% c("Q18")),
         !grepl("Y751|Y752|Y755|Q413", Der_Procedure_All),
         !grepl("C51|C52|C53|C54|C55|C56|C57|C796|D06|D070|D071|D072|D073", Der_Diagnosis_All)
  )
}

hvlc_gynae_hyst_flag <- function(input_df){
  
  input_df %>%
    mutate("Hysteroscopy_flag" = case_when(
      Age >= 17 &
      (Main_Specialty_Code %in% c(500,502) | Treatment_Function_Code %in% c(502,503)) &
      (substr(Der_Procedure_All, 1,4) %in% c("Q161",
                                             "Q167",
                                             "Q168",
                                             "Q169",
                                             "Q171",
                                             "Q172",
                                             "Q173",
                                             "Q174",
                                             "Q175",
                                             "Q178",
                                             "Q179")|
         substr(Der_Procedure_All, 1,3) %in% c("Q18")) &
      !grepl("Y751|Y752|Y755|Q413", Der_Procedure_All) &
      !grepl("C51|C52|C53|C54|C55|C56|C57|C796|D06|D070|D071|D072|D073", Der_Diagnosis_All) ~ 1,
      TRUE ~ 0
    )
    )%>%
    arrange(desc(Hysteroscopy_flag))
  }

# Vaginal hysterectomy and/or vaginal wall repair------------------------------------------------------

hvlc_gynae_vag_hyst_filter <- function(input_df){
  
  input_df %>%

  filter(Age >= 17, 
         (Main_Specialty_Code %in% c(500,502) | Treatment_Function_Code %in% c(502,503)),
         grepl("Q08|P22|P23|P24", Der_Procedure_All),
         !grepl("C51|C52|C53|C54|C55|C56|C57|C796|D06|D070|D071|D072|D073", Der_Diagnosis_All)
  )
}

hvlc_gynae_vag_hyst_flag <- function(input_df){
  
  input_df %>%
    mutate("Vaginal_hysterectomy_and/or_vaginal_wall_repair_flag" = case_when(
      Age >= 17 &
      (Main_Specialty_Code %in% c(500,502) | Treatment_Function_Code %in% c(502,503)) &
      grepl("Q08|P22|P23|P24", Der_Procedure_All) &
      !grepl("C51|C52|C53|C54|C55|C56|C57|C796|D06|D070|D071|D072|D073", Der_Diagnosis_All) ~ 1,
      TRUE ~ 0
    )
    )%>%
    arrange(desc(Vaginal_hysterectomy_and/or_vaginal_wall_repair_flag))
  }
