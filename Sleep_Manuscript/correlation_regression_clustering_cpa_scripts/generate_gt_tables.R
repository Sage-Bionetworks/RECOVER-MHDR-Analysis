#install.packages(c("gt", "webshot2"))
library(gt)
library(webshot2)

iv_order = c("Sleep Efficiency",
             "Sleep Duration",
             "Minutes in Deep Sleep",
             "Minutes in REM Sleep",
             "REM Sleep Breathing Rate",
             "SpO2",
             "Heart Rate Variability",
             "Resting Heart Rate",
             "SD of Sleep Duration",
             "SD of Mid-Sleep",
             "REM Onset Latency",
             "REM Fragmentation Index",
             "Minutes in Light Sleep",
             "WASO")  

clinical_names = c("Smell/taste", 
                   "Post-exertional malaise", 
                   "Chronic cough", 
                   "Brain fog", 
                   "Thirst", 
                   "Palpitations",
                   "Chest pain", 
                   "Fatigue", 
                   "Dizziness", 
                   "Gastrointestinal symptoms", 
                   "Head pain", 
                   "Shortness of breath", 
                   "Sleep apnea", 
                   "Sleep disturbance", 
                   "Global physical health", 
                   "Global mental health"
)

clinical_names_filename = c("Smell_taste", 
                            "Post-exertional_malaise", 
                            "Chronic_cough", 
                            "Brain_fog", 
                            "Thirst", 
                            "Palpitations",
                            "Chest_pain", 
                            "Fatigue", 
                            "Dizziness", 
                            "Gastrointestinal_symptoms", 
                            "Head_pain", 
                            "Shortness_of_breath", 
                            "Sleep_apnea", 
                            "Sleep_disturbance", 
                            "Global_physical_health", 
                            "Global_mental_health"
)


## generate GT tables for unscaled regression results for all outcomes
resall_unscaled = read.csv("regression_results_allmodels_original.csv")

m_select = c(1:3,6)

for(i in 1:(length(clinical_names)-2)){
  
  for(k in m_select){
    
    eff <- resall_unscaled[resall_unscaled$model==k & resall_unscaled$dv==clinical_names[i], 
                           c("iv", "nobs", "Estimate", "Std.Error", "t.value", "p.value.adjusted", "sig", "lower", "upper")]
    
    colnames(eff) = c("iv", "nobs", "Estimate", "Std. Error", "z-value", "p-value (adjusted)", "sig", "lower bound (95% CI)", "upper bound (95% CI)")
    
    # Arrange rows based on iv_order
    eff = eff[order(match(eff$iv, iv_order)), ]
    
    eff_table <- eff %>%
      gt() %>%
      tab_header(
        title = ifelse(k==6, paste("Dependent Variable:", clinical_names[i], "(Results based on the full model)"),
                       paste("Dependent Variable:", clinical_names[i], "(Results based on model", k, ")")
        )
      ) %>%
      tab_source_note(
        source_note = "significance codes (sig): 0.001 '***',  0.01 '**',  0.05 '*'"
      ) %>%
      fmt_number(
        columns = c("Estimate", 
                    "Std. Error", 
                    "z-value", 
                    "lower bound (95% CI)",
                    "upper bound (95% CI)"),
        decimals = 3
      ) %>%
      fmt_scientific(
        columns = "p-value (adjusted)",
        decimals = 3
      ) %>%
      tab_style( ## prevent column names from being split in two lines
        style = cell_text(whitespace = "nowrap"),
        locations = cells_column_labels()          
      )
    
    gtsave(eff_table, 
           filename = paste0("gt_table_logistic_regression_", 
                             clinical_names_filename[i], "_model", k, "_original.png"))
  }
  
}

for(i in (length(clinical_names)-1):length(clinical_names) ){
  
  for(k in m_select){
    
    eff <- resall_unscaled[resall_unscaled$model==k & resall_unscaled$dv==clinical_names[i], 
                           c("iv", "nobs", "Estimate", "Std.Error", "t.value", "p.value.adjusted", "sig", "lower", "upper")]
    
    colnames(eff) = c("iv", "nobs", "Estimate", "Std. Error", "t-value", "p-value (adjusted)", "sig", "lower bound (95% CI)", "upper bound (95% CI)")
    
    # Arrange rows based on iv_order
    eff = eff[order(match(eff$iv, iv_order)), ]
    
    eff_table <- eff %>%
      gt() %>%
      tab_header(
        title = ifelse(k==6, paste("Dependent Variable:", clinical_names[i], "(Results based on the full model)"),
                       paste("Dependent Variable:", clinical_names[i], "(Results based on model", k, ")")
        )
      ) %>%
      tab_source_note(
        source_note = "significance codes (sig): 0.001 '***',  0.01 '**',  0.05 '*'"
      ) %>%
      fmt_number(
        columns = c("Estimate", 
                    "Std. Error", 
                    "t-value", 
                    "lower bound (95% CI)",
                    "upper bound (95% CI)"),
        decimals = 3
      ) %>%
      fmt_scientific(
        columns = "p-value (adjusted)",
        decimals = 3
      ) %>%
      tab_style( ## prevent column names from being split in two lines
        style = cell_text(whitespace = "nowrap"),
        locations = cells_column_labels()          
      )
    
    gtsave(eff_table, 
           filename = paste0("gt_table_linear_regression_", 
                             clinical_names_filename[i], "_model", k, "_original.png"))
  }
  
}


## generate GT tables for scaled regression results only for promis_global_ph/mh 
res2_scaled = read.csv("regression_results_model2_scaled.csv")
res3_scaled = read.csv("regression_results_model3_scaled.csv")
resall_scaled = rbind(res2_scaled, res3_scaled)

library(magick)
for(i in (length(clinical_names)-1):length(clinical_names) ){
    
    eff1 <- resall_scaled[resall_scaled$model==2 & resall_scaled$dv==clinical_names[i], 
                           c("iv", "nobs", "Estimate", "Std.Error", "t.value", "p.value.adjusted", "sig", "lower", "upper")]
    
    eff2 <- resall_scaled[resall_scaled$model==3 & resall_scaled$dv==clinical_names[i], 
                          c("iv", "nobs", "Estimate", "Std.Error", "t.value", "p.value.adjusted", "sig", "lower", "upper")]
    
    colnames(eff1) = c("iv", "nobs", "Estimate", "Std. Error", "t-value",  "p-value (adjusted)", "sig", "lower bound (95% CI)", "upper bound (95% CI)")
    colnames(eff2) = colnames(eff1)
    
    # Arrange rows based on iv_order
    eff1 = eff1[order(match(eff1$iv, iv_order)), ]
    eff2 = eff2[order(match(eff2$iv, iv_order)), ]
    
    eff_table1 <- eff1 %>%
      gt() %>%
      tab_header(
        title = paste("Dependent Variable:", clinical_names[i], "(Results based on model 2)"),
      ) %>%
      tab_source_note(
        source_note = "significance codes (sig): 0.001 '***',  0.01 '**',  0.05 '*'"
      ) %>%
      fmt_number(
        columns = c("Estimate", 
                    "Std. Error", 
                    "t-value", 
                    "lower bound (95% CI)",
                    "upper bound (95% CI)"),
        decimals = 3
      ) %>%
      fmt_scientific(
        columns = "p-value (adjusted)",
        decimals = 3
      ) %>%
      tab_style( ## prevent column names from being split in two lines
        style = cell_text(whitespace = "nowrap"),
        locations = cells_column_labels()          
      )
    
    eff_table2 <- eff2 %>%
      gt() %>%
      tab_header(
        title = paste("Dependent Variable:", clinical_names[i], "(Results based on model 3)"),
      ) %>%
      tab_source_note(
        source_note = "significance codes (sig): 0.001 '***',  0.01 '**',  0.05 '*'"
      ) %>%
      fmt_number(
        columns = c("Estimate", 
                    "Std. Error", 
                    "t-value", 
                    "lower bound (95% CI)",
                    "upper bound (95% CI)"),
        decimals = 3
      ) %>%
      fmt_scientific(
        columns = "p-value (adjusted)",
        decimals = 3
      ) %>%
      tab_style( ## prevent column names from being split in two lines
        style = cell_text(whitespace = "nowrap"),
        locations = cells_column_labels()          
      )

    # Define temporary file names
    file1 <- tempfile(fileext = ".png")
    file2 <- tempfile(fileext = ".png")
    
    # Save tables as PNG images
    gtsave(eff_table1, file1)
    gtsave(eff_table2, file2)
    
    # Read images using magick
    img1 <- image_read(file1)
    img2 <- image_read(file2)
    
    # Combine images side by side
    combined_img <- image_append(c(img1, img2), stack = FALSE)
    
    # Save the final combined image
    image_write(combined_img, paste0("gt_table_linear_regression_", 
                                     clinical_names_filename[i], "_scaled.png"))
    
    # Delete temporary files
    unlink(c(file1, file2))
  
}



