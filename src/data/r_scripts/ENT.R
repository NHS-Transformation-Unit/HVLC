
# Endo sinus surgery ------------------------------------------------------

hvlc_ent_endo_sinus_filter <- function(input_df){
  
  input_df %>%
    filter(Age >= 17,
           (Main_Specialty_Code == 120 | Treatment_Function_Code %in% c(120, 215)),
           (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2),
           substr(Der_Procedure_All, 1,4) %in% c("E133",
                                                 "E142",
                                                 "E081",
                                                 "E148",
                                                 "E132",
                                                 "E143"),
           grepl("Y761", Der_Procedure_All),
           !grepl("C0|C10|C11|C12|C13|C14|C30|C31|C32", Der_Diagnosis_All)
    )
  
}

hvlc_ent_endo_sinus_flag <- function(input_df){
  
  input_df %>%
    mutate("Endo_sinus_Flag" = case_when(
      Age >= 17 &
        (Main_Specialty_Code == 120 | Treatment_Function_Code %in% c(120, 215)) &
        (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2) &
        substr(Der_Procedure_All, 1,4) %in% c("E133",
                                              "E142",
                                              "E081",
                                              "E148",
                                              "E132",
                                              "E143") &
        grepl("Y761", Der_Procedure_All) &
        !grepl("C0|C10|C11|C12|C13|C14|C30|C31|C32", Der_Diagnosis_All) ~ 1,
      TRUE ~ 0
    )
    ) %>%
    arrange(desc(Endo_sinus_Flag))
  
}


# Tonsillectomy -----------------------------------------------------------

hvlc_ent_tonsillectomy_filter <- function(input_df){
  
  input_df %>%
    filter(Age >= 17,
           (Main_Specialty_Code == 120 | Treatment_Function_Code %in% c(120, 215)),
           (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2),
           substr(Der_Procedure_All, 1,4) %in% c("F341",
                                                 "F342",
                                                 "F343",
                                                 "F344",
                                                 "F345",
                                                 "F347",
                                                 "F348",
                                                 "F349"),
           !grepl("D345", Der_Procedure_All),
           !grepl("F346", Der_Procedure_All),
           !grepl("C0|C10|C11|C12|C13|C14|C30|C31|C32", Der_Diagnosis_All)
    ) 
  
  
}

hvlc_ent_tonsillectomy_flag <- function(input_df){
  
  input_df %>%
    mutate("Tonsillectomy_Flag" = case_when(
      Age >= 17 &
        (Main_Specialty_Code == 120 | Treatment_Function_Code %in% c(120, 215)) &
        (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2) &
        substr(Der_Procedure_All, 1,4) %in% c("F341",
                                              "F342",
                                              "F343",
                                              "F344",
                                              "F345",
                                              "F347",
                                              "F348",
                                              "F349") &
        !grepl("D345", Der_Procedure_All) &
        !grepl("F346", Der_Procedure_All) &
        !grepl("C0|C10|C11|C12|C13|C14|C30|C31|C32", Der_Diagnosis_All) ~ 1,
      TRUE ~ 0)
    ) %>%
    arrange(desc(Tonsillectomy_Flag))
  
}


# Myringoplasty -----------------------------------------------------------

hvlc_ent_myringoplasty_filter <- function(input_df){
  
  input_df %>%
    filter(Age >= 17,
           (Main_Specialty_Code == 120 | Treatment_Function_Code %in% c(120, 215)),
           (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2),
           substr(Der_Procedure_All, 1,4) %in% c("D141",
                                                 "D142",
                                                 "D148",
                                                 "D149"),
           !grepl("C0|C10|C11|C12|C13|C14|C30|C31|C32", Der_Diagnosis_All)
    )
  
}

hvlc_ent_myringoplasty_flag <- function(input_df){
  
  input_df %>%
    mutate("myringoplasty_Flag" = case_when(
      Age >= 17 &
        (Main_Specialty_Code == 120 | Treatment_Function_Code %in% c(120, 215)) &
        (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2) &
        substr(Der_Procedure_All, 1,4) %in% c("D141",
                                              "D142",
                                              "D148",
                                              "D149") &
        !grepl("C0|C10|C11|C12|C13|C14|C30|C31|C32", Der_Diagnosis_All) ~ 1,
      TRUE ~ 0)
    ) %>%
    arrange(desc(myringoplasty_Flag))
  
}


# Septoplasty and Turbinate -----------------------------------------------

hvlc_ent_septo_turb_filter <- function(input_df){
  
  input_df %>%
    filter(Age >= 17,
           (Main_Specialty_Code == 120 | Treatment_Function_Code %in% c(120, 215)),
           (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2),
           substr(Der_Procedure_All, 1,4) %in% c("E036",
                                                 "E041",
                                                 "E042",
                                                 "E043",
                                                 "E044",
                                                 "E045",
                                                 "E046",
                                                 "E047",
                                                 "E048",
                                                 "E049"),
           !grepl("C0|C10|C11|C12|C13|C14|C30|C31|C32", Der_Diagnosis_All)
    )
  
}

hvlc_ent_septo_turb_flag <- function(input_df){
  
  input_df %>%
    mutate("Septo_Turb_Flag" = case_when(
      Age >= 17 &
      (Main_Specialty_Code == 120 | Treatment_Function_Code %in% c(120, 215)) &
      (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2) &
      substr(Der_Procedure_All, 1,4) %in% c("E036",
                                            "E041",
                                            "E042",
                                            "E043",
                                            "E044",
                                            "E045",
                                            "E046",
                                            "E047",
                                            "E048",
                                            "E049") &
      !grepl("C0|C10|C11|C12|C13|C14|C30|C31|C32", Der_Diagnosis_All) ~ 1,
      TRUE ~ 0)
    ) %>%
    arrange(desc(Septo_Turb_Flag))
  
}


# Septorhinoplasty --------------------------------------------------------

hvlc_ent_septorhino_filter <- function(input_df){
  
  input_df %>%
    filter(Age >= 17,
           (Main_Specialty_Code == 120 | Treatment_Function_Code %in% c(120, 215)),
           (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2),
           substr(Der_Procedure_All, 1,4) %in% c("E023",
                                                 "E024",
                                                 "E073"),
           !grepl("C0|C10|C11|C12|C13|C14|C30|C31|C32", Der_Diagnosis_All)
    )
  
}

hvlc_ent_septorhino_flag <- function(input_df){
  
  input_df %>%
    mutate("Septohino_Flag" = case_when(
      Age >= 17 &
        (Main_Specialty_Code == 120 | Treatment_Function_Code %in% c(120, 215)) &
        (Admission_Method %in% c(11, 12, 13) | Patient_Classification == 2) &
        substr(Der_Procedure_All, 1,4) %in% c("E023",
                                              "E024",
                                              "E073") &
        !grepl("C0|C10|C11|C12|C13|C14|C30|C31|C32", Der_Diagnosis_All) ~ 1,
      TRUE ~ 0)
    ) %>%
    arrange(desc(Septohino_Flag))
  
}
