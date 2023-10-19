

# Laparoscopic cholecystectomy --------------------------------------------

hvlc_gs_lc_filter <- function(input_df){
  
  input_df %>%
    filter(Age >= 17,
           (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2),
           substr(Der_Procedure_All, 1,4) %in% c("J181",
                                                 "J183"),
           grepl("Y752", Der_Procedure_All),
           !grepl("J182", Der_Procedure_All)
    )
  
}

hvlc_gs_lc_flag <- function(input_df){
  
  input_df %>%
    mutate("Laparoscopic_cholecystectomy_flag" = case_when(
      Age >= 17 &
      (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2) &
      substr(Der_Procedure_All, 1,4) %in% c("J181",
                                            "J183") &
      grepl("Y752", Der_Procedure_All) &
      !grepl("J182", Der_Procedure_All) ~ 1,
      TRUE ~ 0
    )
    ) %>%
    arrange(desc(Laparoscopic_cholecystectomy_flag))
}


# Primary inguinal hernia repair ------------------------------------------

hvlc_gs_pihr_filter <- function(input_df){
  
  input_df %>%
    filter(Age >= 17,
           (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2),
           grepl("T201|T202|T204|T208|T209", Der_Procedure_All),
           !grepl("Y713|Y716|Y717", Der_Procedure_All),
           !grepl("C56|C570", Der_Diagnosis_All)
    )
  
}


hvlc_gs_pihr_flag <- function(input_df){
  
  input_df %>%
    mutate("Primary_inguinal_hernia_repair_flag" = case_when(
      Age >= 17 &
      (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2) &
      grepl("T201|T202|T203|T204|T208|T209", Der_Procedure_All) &
      !grepl("Y713|Y716|Y717", Der_Procedure_All) &
      !grepl("C56|C570", Der_Diagnosis_All) ~ 1,
      TRUE ~ 0
    )
    ) %>%
    arrange(desc(Primary_inguinal_hernia_repair_flag))
}


# Para-umbilical hernia ---------------------------------------------------

hvlc_gs_puh_filter <- function(input_df){
  
  input_df %>%
    filter(Age >= 17,
           (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2),
           substr(Der_Procedure_All, 1,4) %in% c("T240",
                                                 "T241",
                                                 "T242",
                                                 "T243",
                                                 "T245",
                                                 "T246",
                                                 "T247",
                                                 "T248",
                                                 "T249")
    )
  
}


hvlc_gs_puh_flag <- function(input_df){
  
  input_df %>%
    mutate("Para_umbilical_hernia_flag" = case_when(
      Age >= 17 &
      (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2) &
      substr(Der_Procedure_All, 1,4) %in% c("T240",
                                            "T241",
                                            "T242",
                                            "T243",
                                            "T245",
                                            "T246",
                                            "T247",
                                            "T248",
                                            "T249") ~ 1,
      TRUE ~ 0
    )
    ) %>%
    arrange(desc(Para_umbilical_hernia_flag))
}
