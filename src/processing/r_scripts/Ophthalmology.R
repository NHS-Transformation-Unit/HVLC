# Low complexity cataract surgery ------------------------------------------------------

hvlc_ophth_lccs_filter <- function(input_df){
  
  input_df %>%
    filter(Age >= 17, 
         Admission_Method %in% c(11,12,13) | Patient_Classification == 2,
         (grepl("C71|C73|C74", Der_Procedure_All) & grepl("C751|C754|C758|C759", Der_Procedure_All)),
         !grepl("C647|C776|C792|C793|C795|C796|C797|C801|C802|C803|C804|C805|C806|C808|C809", Der_Procedure_All),
         !grepl("F00|F01|F02|F03|G30|F051|G310|G311|F70|F71|F72|F73|F78|F79|
                H200|H201|H202|H208|H209|H220|H221|H300|H301|H302|H308|H309|H320", Der_Diagnosis_All)
    )
  
}

hvlc_ophth_lccs_flag <- function(input_df){
  
  input_df %>%
    mutate("Low_complexity_cataract_surgery_flag" = case_when(
      Age >= 17 &
        (Admission_Method %in% c(11,12,13) | Patient_Classification == 2) &
      ((grepl("C71|C73|C74", Der_Procedure_All) & grepl("C751|C754|C758|C759", Der_Procedure_All))) &
      !grepl("C647|C776|C792|C793|C795|C796|C797|C801|C802|C803|C804|C805|C806|C808|C809", Der_Procedure_All) &
      !grepl("F00|F01|F02|F03|G30|F051|G310|G311|F70|F71|F72|F73|F78|F79|
                H200|H201|H202|H208|H209|H220|H221|H300|H301|H302|H308|H309|H320", Der_Diagnosis_All) ~ 1,
      TRUE ~ 0
    )
    ) %>%
    arrange(desc(Low_complexity_cataract_surgery_flag))
  
}




