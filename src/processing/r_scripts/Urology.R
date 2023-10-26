# Bladder outflow obstruction------------------------------------------------------

hvlc_urology_boo_filter <- function(input_df){

  input_df %>%
  filter(Age >= 17, 
         (Main_Specialty_Code == 101 | Treatment_Function_Code %in% c(101,211)),
         (Admission_Method %in% c(11,12,13) | Patient_Classification == 2),
         substr(Der_Procedure_All, 1,4) %in% c("M651",
                                               "M653",
                                               "M654",
                                               "M655",
                                               "M656",
                                               "M658",
                                               "M659",
                                               "M662",
                                               "M681",
                                               "M683",
                                               "M688",
                                               "M689")
  )
}

hvlc_urology_boo_flag <- function(input_df){
  
  input_df %>%
    mutate("Bladder_outflow_obstruction_flag"=case_when(
      Age >= 17 &
      (Main_Specialty_Code == 101 | Treatment_Function_Code %in% c(101,211)) &
      (Admission_Method %in% c(11,12,13) | Patient_Classification == 2) &
      substr(Der_Procedure_All, 1,4) %in% c("M651",
                                            "M653",
                                            "M654",
                                            "M655",
                                            "M656",
                                            "M658",
                                            "M659",
                                            "M662",
                                            "M681",
                                            "M683",
                                            "M688",
                                            "M689") ~ 1,
      TRUE~0
    )
    ) %>%
    arrange(desc(Bladder_outflow_obstruction_flag))
}

         
# Bladder tumour resection (TURBT) ------------------------------------------------------
         
hvlc_urology_btr_filter <- function(input_df){
  
  input_df %>%
    
  filter(Age >= 17, 
         (Main_Specialty_Code == 101 | Treatment_Function_Code %in% c(101,211)),
         (Admission_Method %in% c(11,12,13) | Patient_Classification == 2),
         substr(Der_Procedure_All, 1,4) %in% c("M421")   
  )
}

hvlc_urology_btr_flag <- function(input_df){
  
  input_df %>%
    mutate("Bladder_tumour_resection_flag" = case_when(
      Age >= 17 &
      (Main_Specialty_Code == 101 | Treatment_Function_Code %in% c(101,211)) &
      (Admission_Method %in% c(11,12,13) | Patient_Classification == 2) &
      substr(Der_Procedure_All, 1,4) %in% c("M421") ~ 1,
      TRUE~0
      )
    ) %>%
    arrange(desc(Bladder_tumour_resection_flag))
}

         
# Cystoscopy plus (WE NEED EXTRACTS OF OUTPATIENT DATA AS WELL FOR THIS PATHWAY)

hvlc_urology_cyst_filter <- function(input_df){
  
  input_df %>%
  filter(Age >= 17, 
         (Main_Specialty_Code == 101 | Treatment_Function_Code %in% c(101,211)),
         (Admission_Method %in% c(11,12,13) | Patient_Classification == 2),
         substr(Der_Procedure_All, 1,4) %in% c("M451",
                                               "M452",
                                               "M453",
                                               "M454",
                                               "M455",
                                               "M458",
                                               "M459",
                                               "M441",
                                               "M442",
                                               "M763",
                                               "M764",
                                               "M792",
                                               "M814")
  )
}

hvlc_urology_cyst_flag <- function(input_df){
  
  input_df %>%
    mutate("Cystoscopy_plus_flag" = case_when(
      Age >= 17 &
      (Main_Specialty_Code == 101 | Treatment_Function_Code %in% c(101,211)) &
      (Admission_Method %in% c(11,12,13) | Patient_Classification == 2)&
      substr(Der_Procedure_All, 1,4) %in% c("M451",
                                            "M452",
                                            "M453",
                                            "M454",
                                            "M455",
                                            "M458",
                                            "M459",
                                            "M441",
                                            "M442",
                                            "M763",
                                            "M764",
                                            "M792",
                                            "M814") ~ 1,
      TRUE ~ 0
    )
    ) %>%
    arrange(desc(Cystoscopy_plus_flag))
}

         
# Ureteroscopy and stent management (WE NEED EXTRACTS OF OUTPATIENT DATA AS WELL FOR THIS PATHWAY)      
         
hvlc_urology_usm_filter <- function(input_df){
  
  input_df %>%
    
  filter(Age >= 17, 
         (Main_Specialty_Code == 101 | Treatment_Function_Code %in% c(101,211)),         
         (Admission_Method %in% c(11,12,13) | Patient_Classification == 2),
         substr(Der_Procedure_All, 1,4) %in% c("M071",
                                               "M072",
                                               "M078",
                                               "M079",
                                               "M271",
                                               "M272",
                                               "M273",
                                               "M274",
                                               "M275",
                                               "M277",
                                               "M278",
                                               "M279",
                                               "M306",
                                               "M281",
                                               "M282",
                                               "M283",
                                               "M288",
                                               "M289",
                                               "M274",
                                               "M275",
                                               "M292",
                                               "M293",
                                               "M295")
  )
}

hvlc_urology_usm_flag<- function(input_df){
  
  input_df %>%
    mutate("Ureteroscopy_and_stent_management_flag"= case_when(
      Age >= 17 &
      (Main_Specialty_Code == 101 | Treatment_Function_Code %in% c(101,211)) &
      (Admission_Method %in% c(11,12,13) | Patient_Classification == 2) &
      substr(Der_Procedure_All, 1,4) %in% c("M071",
                                            "M072",
                                            "M078",
                                            "M079",
                                            "M271",
                                            "M272",
                                            "M273",
                                            "M274",
                                            "M275",
                                            "M277",
                                            "M278",
                                            "M279",
                                            "M306",
                                            "M281",
                                            "M282",
                                            "M283",
                                            "M288",
                                            "M289",
                                            "M274",
                                            "M275",
                                            "M292",
                                            "M293",
                                            "M295")~1,
      TRUE~0
    )
    )  %>%
    arrange(desc(Ureteroscopy_and_stent_management_flag))
  
}

#  Minor peno-scrotal surgery
        
hvlc_urology_mpss_filter <- function(input_df){
  
  input_df %>%
  filter(Age >= 17, 
         (Main_Specialty_Code == 101 | Treatment_Function_Code %in% c(101,211)),         
         (Admission_Method %in% c(11,12,13) | Patient_Classification == 2),
         substr(Der_Procedure_All, 1,4) %in% c("N303",
                                               "N284",
                                               "N113",
                                               "N111",
                                               "N112",
                                               "N114",
                                               "N118",
                                               "N115",
                                               "N119",
                                               "N116",
                                               "M731",
                                               "N092",
                                               "N132",
                                               "N082",
                                               "N099",
                                               "N089",
                                               "N093",
                                               "N098",
                                               "N083",
                                               "N088",
                                               "N094",
                                               "N081",
                                               "N084",
                                               "N091",
                                               "N321",
                                               "T193",
                                               "N191",
                                               "N198",
                                               "N199")
  )
}

hvlc_urology_mpss_flag <- function(input_df){
  
  input_df %>%
    mutate("Minor_peno_scrotal_surgery_flag"= case_when(
      Age >= 17 &
      (Main_Specialty_Code == 101 | Treatment_Function_Code %in% c(101,211)) &         
      (Admission_Method %in% c(11,12,13) | Patient_Classification == 2) &
      substr(Der_Procedure_All, 1,4) %in% c("N303",
                                            "N284",
                                            "N113",
                                            "N111",
                                            "N112",
                                            "N114",
                                            "N118",
                                            "N115",
                                            "N119",
                                            "N116",
                                            "M731",
                                            "N092",
                                            "N132",
                                            "N082",
                                            "N099",
                                            "N089",
                                            "N093",
                                            "N098",
                                            "N083",
                                            "N088",
                                            "N094",
                                            "N081",
                                            "N084",
                                            "N091",
                                            "N321",
                                            "T193",
                                            "N191",
                                            "N198",
                                            "N199") ~1,
      TRUE ~ 0
    )
    )%>%
    arrange(desc(Minor_peno_scrotal_surgery_flag))
  
}

      
      
      
 
        