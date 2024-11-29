# Server
server <- function(input, output, session) {
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    data <- vacation_data
    
    if (input$season_filter != "Semua") {
      data <- data[data$Musim_Favorit == input$season_filter, ]
    }
    
    data <- data[data$Umur >= input$age_filter[1] & data$Umur <= input$age_filter[2], ]
    
    if (input$location_filter != "Semua") {
      data <- data[data$Lokasi == input$location_filter, ]
    }
    
    return(data)
  })
  
  # Plot 1: Pie Chart (Total dan Persentase Pantai vs Gunung)
  output$preference_pie <- renderPlot({
    data <- filtered_data()
    counts <- table(data$Preferensi_Liburan)
    pct <- round(100 * counts / sum(counts), 1)
    
    # Atur margin agar plot lebih besar
    par(mar = c(.25, .25, .25, .25))  # Atur margin (bawah, kiri, atas, kanan)
    
    pie3D(counts, 
          labels = paste(names(counts), ":", counts, " ", " (", pct, "%)", sep=""),
          col = brewer.pal(5, input$color_scheme)[1:2], 
          main = "", 
          cex.main = 2,    # Ukuran judul lebih besar
          cex.lab = 1.5,   # Ukuran label lebih besar
          cex.axis = 1.5,  # Ukuran axis lebih besar
          radius = 3,       # Radius pie lebih besar
          shade = 0.8,
          height=0.3
    )
  })
  
  
  # Plot 2: Stacked Bar Plot - Pendidikan
  output$education_preference <- renderPlot({
    data <- filtered_data()
    education_data <- data %>% 
      group_by(Pendidikan, Preferensi_Liburan) %>%
      tally() %>%
      group_by(Preferensi_Liburan) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(education_data, aes(x = Preferensi_Liburan, y = n, fill = Pendidikan)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
      xlab("") +
      ylab("Jumlah Orang") +
      theme_minimal() +
      scale_fill_brewer(palette = input$color_scheme) +
      theme(text = element_text(size = 14))
  })
  
  # Plot 3: Stacked Bar Plot - Jenis Kelamin
  output$gender_preference <- renderPlot({
    data <- filtered_data()
    
    # Gender per preference (Beach and Mountain)
    gender_data <- data %>% 
      group_by(Jenis_Kelamin, Preferensi_Liburan) %>%
      tally() %>%
      group_by(Preferensi_Liburan) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(gender_data, aes(x = Preferensi_Liburan, y = n, fill = Jenis_Kelamin)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
      xlab("") +
      ylab("Jumlah Orang") +
      theme_minimal() +
      scale_fill_brewer(palette = input$color_scheme) +
      theme(text = element_text(size = 14))
  })
  
  # Plot 4: Boxplot - Frek. Liburan
  output$vacation_frequency <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Preferensi_Liburan, y = Frekuensi_Liburan, fill = Preferensi_Liburan)) +
      geom_boxplot() +
      scale_fill_brewer(palette = input$color_scheme) +
      xlab("") +
      ylab("Frek. Liburan per Orang (per tahun)") +
      theme_minimal() +
      theme(text = element_text(size = 14))
  })
  
  # Plot 5: Scatter Plot - Penghasilan vs Anggaran Liburan
  output$income_budget <- renderPlot({
    data <- filtered_data()
    
    # Ambil warna yang dipilih dari input
    plot_colors <- brewer.pal(5, input$color_scheme)[1:2]
    
    ggplot(data, aes(x = Penghasilan, y = Anggaran_Liburan, color = Preferensi_Liburan)) +
      geom_point() +
      geom_smooth(data = subset(data, Preferensi_Liburan == "Pantai"), method = "lm", color = plot_colors[1], se = FALSE) +
      geom_smooth(data = subset(data, Preferensi_Liburan == "Gunung"), method = "lm", color = plot_colors[2], se = FALSE) +
      scale_color_manual(values = plot_colors) +
      xlab("Penghasilan ($)") +
      ylab("Anggaran Liburan ($)") +
      theme_minimal() +
      theme(text = element_text(size = 14))
  })
  
  # Plot 6: Boxplot - Frek Liburan
  output$vacation_frequency_comparison <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Preferensi_Liburan, y = Frekuensi_Liburan, fill = Preferensi_Liburan)) +
      geom_boxplot() +
      scale_fill_brewer(palette = input$color_scheme) +
      xlab('') + 
      ylab("Frek. Liburan per Orang (per tahun)") +
      theme_minimal() +
      theme(text = element_text(size = 14))
  })
  
  # Plot 7: Stacked Bar Plot - Musim Favorit
  output$favorite_season <- renderPlot({
    data <- filtered_data()
    season_data <- data %>% 
      group_by(Musim_Favorit, Preferensi_Liburan) %>%
      tally() %>%
      group_by(Musim_Favorit) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(season_data, aes(x = Musim_Favorit, y = n, fill = Preferensi_Liburan)) +
      geom_bar(stat = "identity", position = "stack") +
      geom_text(aes(label = paste0(round(pct, 1), "%")), position = position_stack(vjust = 0.5)) +
      xlab("") +
      ylab("Jumlah Orang") +
      theme_minimal() +
      coord_flip() +
      scale_fill_brewer(palette = input$color_scheme) +
      theme(text = element_text(size = 14))
  })
  
  # Plot 8: Boxplot - Umur
  output$age_preference <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Preferensi_Liburan, y = Umur, fill = Preferensi_Liburan)) +
      geom_boxplot() +
      scale_fill_brewer(palette = input$color_scheme) +
      xlab('') + 
      ylab("Umur") +
      theme_minimal() +
      theme(text = element_text(size = 14))
  })
  
  #==========================================#
  # Render Data Summary (Mean, Median, etc.)
  output$data_summary <- renderPrint({
    summary(filtered_data())  # Use the reactive dataset here
  })
  
  # Gender Preference Plot
  output$gender_preference <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Jenis_Kelamin, fill = Preferensi_Aktivitas)) +
      geom_bar(position = "dodge") +
      labs(title = "Preferensi Liburan Berdasarkan Jenis_Kelamin", x = "Jenis_Kelamin", y = "Count") +
      theme_minimal() +
      scale_fill_brewer(palette = input$color_scheme)
  })
  
  # Vacation Budget Plot
  output$vacation_budget <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Anggaran_Liburan, fill = Preferensi_Aktivitas)) +
      geom_histogram(binwidth = 1000, position = "identity", alpha = 0.7) +
      labs(title = "Distribusi Budget Liburan", x = "Budget Liburan", y = "Count") +
      theme_minimal() +
      scale_fill_brewer(palette = input$color_scheme)
  })
  
  # Data Distribution Plot
  output$data_distribution <- renderPlot({
    data <- filtered_data()
    ggplot(data, aes(x = Umur, y = Anggaran_Liburan)) +
      geom_point(aes(color = Preferensi_Aktivitas)) +
      labs(title = "Distribusi Data", x = "Umur", y = "Vacation Budget") +
      theme_minimal() +
      scale_color_brewer(palette = input$color_scheme)
  })
  
  # Correlation Plot (Correlation matrix)
  output$correlation_plot <- renderPlot({
    data <- filtered_data()
    
    # Select only numeric columns for correlation
    numeric_data <- data %>%
      select_if(is.numeric)
    
    # Calculate correlation matrix
    corr_matrix <- cor(numeric_data, use = "complete.obs")
    
    # Plot correlation matrix
    corrplot::corrplot(corr_matrix, method = "circle", type = "upper",
                       col = colorRampPalette(c("red", "white", "blue"))(200),
                       title = "Korelasi Antar Variabel")
  })
  
  # Interactive Summary Table
  output$summary_table <- DT::renderDataTable({
    DT::datatable(filtered_data(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  #=====================EDA============+++===+#
  
  # Update choices for numeric and categorical variables based on dataset
  observe({
    data <- filtered_data()
    
    # Update variable selections based on dataset columns
    updateSelectInput(session, "numeric_x", choices = names(data)[sapply(data, is.numeric)])
    updateSelectInput(session, "y_variable", choices = names(data))
    updateSelectInput(session, "categorical_x", choices = names(data)[sapply(data, is.factor)])
    updateSelectInput(session, "categorical_y", choices = names(data))
    updateSelectInput(session, "x_continuous", choices = names(data)[sapply(data, is.numeric)])
    updateSelectInput(session, "y_continuous", choices = names(data)[sapply(data, is.numeric)])
  })
  
  # a) X numerik vs Y (Boxplot/KDEplot + Uji Beda Nilai Tengah)
  output$boxplot_kde <- renderPlot({
    data <- filtered_data()
    req(input$numeric_x, input$y_variable)
    
    ggplot(data, aes_string(x = input$numeric_x, y = input$y_variable)) +
      geom_boxplot() +
      scale_fill_brewer(palette = input$color_scheme) +
      theme_minimal() +
      labs(title = paste("Boxplot of", input$numeric_x, "vs", input$y_variable))
  })
  
  # Uji Beda Nilai Tengah (T-test / ANOVA)
  output$median_test_output <- renderPrint({
    data <- filtered_data()
    
    # Ensure inputs are available
    req(input$numeric_x, input$y_variable)
    
    # Debug print: Check the inputs and data
    print(paste("numeric_x:", input$numeric_x))
    print(paste("y_variable:", input$y_variable))
    
    # Check if numeric_x is numeric and y_variable is categorical
    if (is.numeric(data[[input$numeric_x]]) && is.factor(data[[input$y_variable]])) {
      
      # Debug print: Check the structure of the selected columns
      print("Columns are numeric and factor. Proceeding with the test.")
      
      # If y_variable has 2 unique levels, use T-test
      if (length(unique(data[[input$y_variable]])) == 2) {
        test_result <- t.test(data[[input$numeric_x]] ~ data[[input$y_variable]])
      } else {
        # If y_variable has more than 2 levels, use ANOVA
        test_result <- aov(data[[input$numeric_x]] ~ data[[input$y_variable]], data = data)
        test_result <- summary(test_result)
      }
      # Return the result
      return(test_result)
    } else {
      # Debug: Print message if the inputs don't match the expected types
      print("Either numeric_x is not numeric or y_variable is not a factor.")
    }
  })
  
  # b) X kategorik vs Y (Barplot + Uji Chi-Square)
  output$categorical_x <- renderUI({
    selectInput("categorical_x", "Pilih Variabel X",
                choices = names(filtered_data())[sapply(filtered_data(), function(x) is.factor(x) || is.character(x))])
  })
  
  output$categorical_y <- renderUI({
    selectInput("categorical_y", "Pilih Variabel Y",
                choices = names(filtered_data())[sapply(filtered_data(), function(x) is.factor(x) || is.character(x))])
  })
  
  # Uji Chi-Square
  output$chi_square_test_output <- renderPrint({
    data <- filtered_data()
    req(input$categorical_x, input$categorical_y)
    
    if (!is.factor(data[[input$categorical_x]]) || !is.factor(data[[input$categorical_y]])) {
      data[[input$categorical_x]] <- as.factor(data[[input$categorical_x]])
      data[[input$categorical_y]] <- as.factor(data[[input$categorical_y]])
    }
    
    test_result <- chisq.test(table(data[[input$categorical_x]], data[[input$categorical_y]]))
    print(test_result)
  })
  
  # Output untuk Barplot
  output$barplot_output <- renderPlot({
    data <- filtered_data()
    req(input$categorical_x, input$categorical_y)
    
    ggplot(data, aes_string(x = input$categorical_x, fill = input$categorical_y)) +
      geom_bar(position = "dodge") +
      labs(title = paste(input$categorical_x, "vs", input$categorical_y),
           x = input$categorical_x, y = "Count") +
      theme_minimal() +
      scale_fill_brewer(palette = input$color_scheme) +  # Using color palette from input
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Optional: Adjust x-axis text for better visibility
  })
  
  # c) Scatter plot 2 Kontinu (Multikollinearitas)
  output$scatter_plot <- renderPlot({
    data <- filtered_data()
    req(input$x_continuous, input$y_continuous)
    
    ggplot(data, aes_string(x = input$x_continuous, y = input$y_continuous)) +
      geom_point(aes(color = input$y_continuous)) +
      theme_minimal() +
      scale_fill_brewer(palette = input$color_scheme) +
      labs(title = paste("Scatter Plot of", input$x_continuous, "vs", input$y_continuous))
  })
  
  # Update choices for predictors based on dataset
  observe({
    data <- filtered_data()
    
    # Update variable selections based on dataset columns
    updateSelectizeInput(session, "selected_predictors", choices = names(data)[sapply(data, is.numeric)], server = TRUE)
  })
  
  # Handle Select All and Unselect All buttons for predictors
  observeEvent(input$add_all, {
    data <- filtered_data()
    updateSelectizeInput(session, "selected_predictors", selected = names(data)[sapply(data, is.numeric)])
  })
  
  observeEvent(input$remove_all, {
    updateSelectizeInput(session, "selected_predictors", selected = NULL)
  })
  
  ########## Menu Item: Regresi Logistik #################
  # Reactive value untuk menyimpan dataset
  data <- reactiveVal()
  
  # Dataset contoh
  example_data <- reactive({
    vacation_data <- vacation_data %>%
      mutate(Preferensi_Liburan = ifelse(Preferensi_Liburan== "Pantai", 0, 1))
  })
  
  
  # Muat dataset berdasarkan pilihan
  observeEvent(input$load_data, {
    if (input$data_source == "example") {
      data(example_data())
    } else if (!is.null(input$file)) {
      # Pengecekan ukuran file terlebih dahulu
      if (input$file$size > 10 * 1024^2) { # 10 MB
        showModal(modalDialog(
          title = "Error",
          "Ukuran file melebihi batas maksimum 10 MB.",
          easyClose = TRUE
        ))
      } else {
        req(input$file) # Validasi file hanya jika ukuran sesuai
        uploaded_data <- read.csv(input$file$datapath)
        data(uploaded_data)
      }
    }
  })
  
  # Tampilkan Data
  output$data_table <- renderDataTable({
    req(data())
    
    if(input$close_data_table){
      NULL
    }
    
    else{
      datatable(data(), options = list(scrollX = TRUE,
                                       scrollY = '200px'  # Set height of the table to make it scrollable
      )
      )
    }
  })
  
  
  # Output untuk memilih peubah respons
  output$select_response <- renderUI({
    req(data())  # Pastikan data tersedia
    
    # Filter variabel yang memiliki 2 nilai unik
    biner_vars <- names(data())[sapply(data(), function(x) {
      length(unique(na.omit(x))) == 2
    })]
    
    if (length(biner_vars) == 0) {
      # Jika tidak ada variabel biner, tampilkan pesan
      return(tags$div("Tidak ada peubah dengan dua nilai unik dalam data."))
    }
    
    # Dropdown untuk memilih variabel respon
    selectInput(
      "response_var", 
      "Pilih Peubah Respon (Biner)", 
      choices = sort(biner_vars)
    )
  })
  
  
  
  # Output untuk memilih peubah prediktor
  output$select_predictors <- renderUI({
    req(data(), input$response_var)
    # Hilangkan peubah yang dipilih sebagai respons dari daftar prediktor
    available_predictors <- sort(setdiff(names(data()), input$response_var))
    selectInput("predictor_vars", "Pilih Peubah Prediktor",
                choices = available_predictors, multiple = TRUE)
  })
  
  # Button
  observeEvent(input$remove_all, {
    updateSelectizeInput(session,"predictor_vars",choices=sort(setdiff(names(data()), input$response_var)), 
                         selected=NULL, options = list(placeholder="Pilih Setidaknya Satu Prediktor")
    )
  })
  observeEvent(input$add_all, {
    updateSelectizeInput(session,"predictor_vars",choices= sort(setdiff(names(data()), input$response_var)),
                         selected=names(data()))
  })
  
  # Jalankan model regresi logistik
  model_result <- eventReactive(input$fit_model, {
    req(input$response_var, input$predictor_vars)
    formula <- as.formula(
      paste(input$response_var, "~", paste(input$predictor_vars, collapse = "+"))
    )
    glm(formula, data = data(), family = binomial)
  })
  
  
  
  # Logistic Regression Tab
  output$logistic_summary <- renderDT({
    req(model_result()) 
    logistic_model <- model_result()
    
    # Cek format koefisien
    exponentiate <- ifelse(input$coef_format == "odds_ratio", TRUE, FALSE)
    
    # Cek Tingkat Kepercayaan
    conf_level <- as.numeric(input$confidence_level)
    
    summary_table <- tidy_parameters(
      logistic_model,
      conf.int = TRUE,
      conf.level = conf_level,
      ci_method = "wald",
      exponentiate = exponentiate
    ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))  # Round to 2 decimals
    
    colnames(summary_table) <- c('Variabel', 'Dugaan Koef.', 'Galat Baku', 'Tingkat Kepercayaan', 'Batas Bawah', 'Batas Atas',
                                 'statistik','db', 'Nilai-p')
    summary_table <- summary_table[, -c(7, 8)]
    
    
    # Create the datatable with scrollable and expandable options
    datatable(summary_table,
              options = list(
                pageLength = 20,  # Set initial number of rows displayed
                scrollY = '400px',  # Set height of the table to make it scrollable
                scroller = TRUE,  # Enable scroller for expandable rows
                dom = 'Bfrtip',  # Allows the use of buttons (optional)
                searching = FALSE
              ),
              class = 'display',
              style = 'bootstrap',
              rownames = FALSE)
  })
  
  # Function to download CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("regresi_logistik_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(model_result()) 
      logistic_model <- model_result()
      
      # Cek format koefisien
      exponentiate <- ifelse(input$coef_format == "odds_ratio", TRUE, FALSE)
      
      # Cek Tingkat Kepercayaan
      conf_level <- as.numeric(input$confidence_level)
      
      summary_table <- tidy_parameters(
        logistic_model, 
        conf.int = TRUE, 
        conf.level = conf_level,
        ci_method = "wald", 
        exponentiate = exponentiate
      ) %>% 
        mutate(across(where(is.numeric), ~ round(.x, 3)))  # Round to 2 decimals
      
      colnames(summary_table) <- c('Variabel', 'Dugaan Koef.', 'Galat Baku', 'Tingkat Kepercayaan', 'Batas Bawah', 'Batas Atas', 
                                   'statistik','db', 'Nilai-p')
      summary_table <- summary_table[, -c(7, 8)]
      
      write.csv(summary_table, file, row.names = FALSE)
    }
  )
  
  
  
  # Output Pemeriksaan Multikolinearitas
  output$vif_table <- renderDT({
    req(model_result(), input$predictor_vars)
    
    logistic_model <- model_result()
    
    # Periksa apakah checkbox multikol diaktifkan
    if (input$multikol) {
      # Periksa apakah lebih dari satu prediktor dipilih
      if (length(input$predictor_vars) > 1) {
        # Coba hitung VIF, pastikan model valid
        vif_calc <- tryCatch({
          vif(logistic_model)
        }, error = function(e) {
          NULL
        })
        
        # Validasi hasil VIF
        if (!is.null(vif_calc)) {
          if (class(vif_calc)[1] == "matrix") {
            # Jika terdapat setidaknya 1 prediktor kategorik
            vif_df <- data.frame(
              Variabel = row.names(vif_calc),
              GVIF = round(vif_calc[, 1], 2),
              row.names = NULL
            )
          } else {
            # Jika semua prediktor adalah numerik
            vif_df <- data.frame(
              Variabel = attr(terms(logistic_model), "term.labels"),
              VIF = round(vif_calc, 2)
            )
          }
        } else {
          vif_df <- data.frame(Peringatan = "Tidak dapat menghitung VIF. Periksa model.")
        }
        
      } else {
        # Jika kurang dari dua variabel dipilih
        vif_df <- data.frame(Peringatan = "Minimal pilih dua variabel prediktor.")
      }
    } else {
      # Jika checkbox tidak diaktifkan
      vif_df <- NULL
    }
    
    # Tampilkan tabel
    datatable(vif_df, 
              options = list(
                pageLength = 20,  # Set initial number of rows displayed
                scrollY = '180px',  # Set height of the table to make it scrollable
                scroller = TRUE,  # Enable scroller for expandable rows
                dom = 'Bfrtip',  # Allows the use of buttons (optional)
                searching = FALSE
              ),
              class = 'display',
              style = 'bootstrap',
              rownames = FALSE)
  })
  
  
  
  
  # Output Nilai AIC BIC Deviance
  output$aic_deviance <- renderPrint({
    req(model_result(), input$predictor_vars)
    
    if (input$eval_model){
      req(input$predictor_vars)  # Ensure predictors are selected
      
      logistic_model <- model_result()
      
      # Menghitung metrik model logistik
      model_metrics <- list(
        "AIC" = round(AIC(logistic_model), 2),
        "BIC" = round(BIC(logistic_model), 2),
        "Residual Deviance" = round(logistic_model$deviance, 2)
      )
      
      
      # Menyusun HTML untuk menampilkan metrik dalam bentuk card
      output_text <- ""
      for (metric_name in names(model_metrics)) {
        output_text <- paste0(output_text, 
                              "<div style='background-color: #0A3981; padding: 10px; margin: 10px 0; border-radius: 10px; color: white; font-family: \"Arial\", sans-serif; text-align: center;'>",
                              "<h4 style='font-size: 20px;'>", metric_name, "</h4>",
                              "<p style='font-size: 25px; font-weight: bold;'>", round(model_metrics[[metric_name]], 2), "</p>",
                              "</div>")
      }
      
      HTML(output_text)
      
    }
    
    
    
  })
  
  
  # Output Confusion Matrix (Buat Agar otomatis Ganti Value Variabel Respon)
  output$conf_matrix <- renderPlot({
    req(model_result(), input$predictor_vars)
    
    if(input$conf_mat){
      req(input$predictor_vars)  # Ensure predictors are selected
      
      logistic_model <- model_result()
      
      preds <- predict(logistic_model, type = "response")
      threshold <- 0.5
      predicted <- ifelse(preds > threshold, "Gunung", "Pantai")
      actual <- vacation_data$Preferensi_Liburan
      
      actual <- factor(actual, levels = c("Pantai", "Gunung"))
      predicted <- factor(predicted, levels = c("Pantai", "Gunung"))
      
      # Menghasilkan confusion matrix
      cm <- confusionMatrix(predicted, actual)
      
      
      output$conf_matrix_acc <- renderUI({
        if(input$conf_mat & length(input$predictor_vars) > 0){
          
          cm_akurasi <- list(
            Accuracy = cm$overall['Accuracy'],
            Kappa = cm$overall['Kappa'],
            Sensitivity = cm$byClass['Sensitivity'],
            Specificity = cm$byClass['Specificity'],
            `Balanced Accuracy` = cm$byClass['Balanced Accuracy']
          )
          
          # Menyusun progress bar untuk menampilkan metrik
          left_cols <- ""
          right_cols <- ""
          progress_value <- ""
          
          for (i in seq_along(cm_akurasi)) {
            metric_name <- names(cm_akurasi)[i]
            progress_value <- round(cm_akurasi[[metric_name]] * 100) # Mengubah nilai menjadi persen
            progress_bar <- paste0(
              "<div style='margin: 10px 0;'>",
              "<h5 style='color: #2C3E50;'>", metric_name, "</h5>",
              "<div style='background-color: #ECF0F1; border-radius: 5px;'>",
              "<div style='background-color: #27AE60; width: ", progress_value, "%; height: 10px; border-radius: 5px;'></div>",
              "</div>",
              "<p style='font-size: 14px; font-weight: bold; color: #2C3E50;'>", progress_value, "%</p>",
              "</div>"
            )
            
            # Menyusun progress bar ke dalam kolom kiri atau kanan
            if (i <= 3) {
              left_cols <- paste0(left_cols, progress_bar)
            } else {
              right_cols <- paste0(right_cols, progress_bar)
            }
          }
          
          # Mengatur layout menjadi dua kolom hanya jika input$conf_mat TRUE
          output_text <- paste0(
            "<div style='display: flex; justify-content: space-between;'>",
            "<div style='width: 48%;'>", left_cols, "</div>",
            "<div style='width: 48%;'>", right_cols, "</div>",
            "</div>"
          )
          
          HTML(output_text)
        } else {
          # Jika input$conf_mat tidak dicentang, tidak menampilkan apa-apa
          HTML("")
        }
      })
      
      # Menyimpan confusion matrix dalam bentuk data frame
      conf_matrix_df <- as.data.frame(cm$table)
      conf_matrix_df$Amatan <- conf_matrix_df$Freq
      
      #  plot confusion
      p <- ggplot(conf_matrix_df, aes(x = Reference, y = Prediction, fill = Amatan)) +
        geom_tile(color = "white") +  # Membuat kotak dengan warna
        scale_fill_gradient(low = "#D4EBF8", high = "#1F509A") +  # Skala warna
        geom_text(aes(label = Amatan), color = "black", size = 5, fontface = "bold") +  # Menambahkan label teks
        theme_minimal(base_size = 16) +  # Menggunakan tema minimal dengan font lebih besar
        labs(
          title = 'Kinerja Model',
          x = "Aktual",
          y = "Prediksi"
        ) +   #coord_fixed(ratio=0.8) +  # Memastikan kotak tetap berbentuk persegi
        guides(fill = "none") +  # Menghapus colorbar
        theme(
          axis.text.x = element_text(angle = 0, hjust = 1, size = 16, family = "Arial", color = "#2C3E50"),  # Menyesuaikan font X axis
          axis.text.y = element_text(size = 16, family = "Arial", color = "#2C3E50"),  # Menyesuaikan font Y axis
          plot.title = element_text(size = 20, family = "Arial", face = "bold", color = "#2980B9"),  # Judul besar dan bold
          plot.subtitle = element_text(size = 16, family = "Arial", face = "italic", color = "#7F8C8D"),  # Subtitle dengan gaya italic
          plot.background = element_rect(fill = "#ECF0F1", color = "#BDC3C7", linewidth = 2)  # Latar belakang plot
        )
      p
      # grid.arrange(p,
      #              widths = unit(3, "in"),
      #              heights = unit(3, "in"))
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
}