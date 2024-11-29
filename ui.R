# Bedanya dengan tes 2 terbaru.R adlh ini pakai indo

library(shiny)
library(rsconnect)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotrix)
library(DT)  # Untuk menampilkan tabel interaktif
library(stringr)
library(broom.helpers)
library(car)
library(caret)
library(ggplot2)
library(gridExtra)


# Read Data
data_url<- "https://raw.githubusercontent.com/erdanisaaghnia/evd/refs/heads/main/data_mod.csv"
vacation_data <- read.csv(data_url)
#vacation_data <- vacation_data[vacation_data$Umur==32,]
#glimpse(vacation_data)

# Convert character columns to factors
vacation_data$Jenis_Kelamin <- factor(vacation_data$Jenis_Kelamin)
vacation_data$Pendidikan <- factor(vacation_data$Pendidikan)
vacation_data$Preferensi_Aktivitas <- factor(vacation_data$Preferensi_Aktivitas)
vacation_data$Lokasi <- factor(vacation_data$Lokasi)
vacation_data$Musim_Favorit <- factor(vacation_data$Musim_Favorit)
vacation_data$Preferensi_Liburan <- factor(vacation_data$Preferensi_Liburan)

# Check the structure again to ensure the changes
str(vacation_data)

# UI
ui <- dashboardPage(
  dashboardHeader(title = tags$span(
    "Preferensi Liburan", 
    style = "
        font-weight: bold;
        font-size: 20px;
        color: white;
        #font-style: italic; /* Mengatur teks miring */
        font-family: 'Comic Sans MS', cursive, sans-serif; /* Menggunakan font berbeda */
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                     -2px -2px 4px rgba(0, 0, 0, 0.5), 
                     2px -2px 4px rgba(0, 0, 0, 0.5), 
                     -2px 2px 4px rgba(0, 0, 0, 0.5);
      ")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pengantar", tabName = "logistic_material", icon = icon("book")),
      menuItem("Overview", tabName = "overview", icon = icon("chart-pie")),
      selectInput("season_filter", "Filter Berdasarkan Musim Favorit:",
                  choices = c("Semua", "Panas", "Dingin", "Semi", "Gugur"), selected = "Semua"),
      sliderInput("age_filter", "Filter Berdasarkan Rentang Umur:",
                  min = min(vacation_data$Umur), max = max(vacation_data$Umur),
                  value = c(min(vacation_data$Umur), max(vacation_data$Umur)), step = 1),
      selectInput("location_filter", "Filter berdasarkan Lokasi Tinggal:",
                  choices = c("Semua", "Perkotaan", "Pinggiran Kota", "Pedesaan"), selected = "Semua"),
      selectInput("color_scheme", "Pilih Skema Warna:", 
                  choices = c("Set1", "Set2", "Set3", "Paired", "Dark2"), selected = "Paired"),
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Regresi Logistik", tabName = "logistic_regression", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    
    # Tambahkan CSS langsung di bagian `tags$head`
    tags$head(
      tags$style(HTML(
        "
        .content-wrapper {
          background-image: url(bg.jpg);
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
          min-height: 100%;
          padding-bottom: 20px;
          position: relative;
        }
        /* Membuat lapisan transparan di atas gambar */
        .content-wrapper::after {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          background-color: rgba(255, 255, 255, 0.2); /* transparansi 40% */
          z-index: 1;
        }
        
        /* CSS untuk font sidebar*/
      .main-sidebar .sidebar-menu a {
        font-size: 20px;
        font-family: 'Comic Sans MS', cursive, sans-serif;
        font-weight: bold;
        color: #FFFFFF !important;
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);
      }
      .main-sidebar .sidebar-menu a:hover {
        color: #FFD700 !important;
        text-shadow: 2px 2px 6px rgba(255, 215, 0, 0.8);
      }
      .main-sidebar .sidebar .sidebar-menu {
        padding-top: 20px;
      }
        
        /* Adjust the sidebar width */
        .main-sidebar {
          width: 250px;
        }
        
        .content-wrapper {
          margin-left: 250px;
          z-index: 2;
        }
        /* Make sure text and plots stay on top of the background */
        .box {
          z-index: 3;
          position: relative;
        }
        
        /* Box styling with 3D effect */
        .box {
          z-index: 3;
          position: relative;
          border-radius: 15px; /* Rounded corners */
          box-shadow: 0px 8px 16px rgba(0, 0, 0, 0.15), 0px 3px 6px rgba(0, 0, 0, 0.75); /* 3D shadow effect */
          background-color: #ffffff; /* White background */
          border: 1px solid #ddd; /* Light border */
          transition: all 0.3s ease; /* Smooth transition for hover effect */
        }
        
        .box:hover {
          box-shadow: 0px 16px 32px rgba(0, 0, 0, 0.2), 0px 6px 12px rgba(0, 0, 0, 0.15); /* Shadow intensifies on hover */
          transform: translateY(-4px); /* Slight lift effect */
        }

        .box .box-header {
          border-top-left-radius: 15px;
          border-top-right-radius: 15px;
          background-color: #007bff; /* Blue header background */
          color: white; /* White text */
        }
        
        .box .box-body {
          padding: 15px;
        }
        
        
        /* Animasi saat buka app*/
        .box {
        opacity: 0;
        animation: fadeIn 1s forwards;
        }
        
        @keyframes fadeIn {
        0% { opacity: 0; }
        100% { opacity: 1; }
        }
        
      /* Menambah Animasi Bayangan dan Hover di Tombol*/
      .btn-3d {
      background-color: #4CAF50;
      border: none;
      color: white;
      padding: 15px 32px;
      text-align: center;
      font-size: 16px;
      cursor: pointer;
      border-radius: 12px;
      box-shadow: 0 6px 15px rgba(0, 0, 0, 1);
      transition: all 0.3s ease;
      }
      
      .btn-3d:hover {
      box-shadow: 0 12px 30px rgba(0, 0, 0, 0.7);
      transform: translateY(-5px);
      }
      
      /* Efek aktif pada item menu */
        .main-sidebar .sidebar-menu li.active a {
          background-color: #2F4F4F; /* Warna hijau gelap untuk menu yang aktif */
          color: white !important;
        }
        
      /* Membuat Gradasi pada Header Box */
      .skin-blue .box .box-header {
        background: linear-gradient(to right,   #4682B4, #5F9EA0) !important; /* Gradasi dari hijau ke biru */
        color: white !important; /* Mengubah warna teks menjadi putih */
      }
      
      /* Custom Button */
        .custom-button {
        color: black;
        border: none;
        border-radius: 8px;
        padding: 10px 15px;
        font-size: 14px;
        cursor: pointer;
        transition: background-color 0.3s, transform 0.2s;
      }
      .custom-button:hover {
        background-color: #ADD8E6;
        transform: scale(1.05);
      }


        "
      ))
    ),
    
    tabItems(
      # PART I: Materi Regresi Logistik
      tabItem(tabName = "logistic_material",
              fluidRow(
                box(title = tags$strong("Materi Regresi Logistik"), width = 12, solidHeader = TRUE, status = "primary",
                    p("Regresi logistik digunakan untuk memodelkan probabilitas suatu kejadian berdasarkan prediktor atau fitur-fitur yang ada. Model ini sering digunakan dalam analisis klasifikasi biner, seperti prediksi apakah suatu hasil adalah '1' atau '0'."),
                    p("Model regresi logistik menghitung kemungkinan terjadinya suatu peristiwa dengan menghasilkan nilai antara 0 dan 1 menggunakan fungsi logit."),
                    p("Langkah-langkah dalam regresi logistik meliputi persiapan data, pemilihan prediktor, membangun model, serta evaluasi model menggunakan metrik seperti AIC dan p-value untuk menilai signifikansi variabel.")
                )
              ),
              fluidRow(
                box(title = "Tentang Dataset Contoh : Preferensi Pengunungan vs Pantai", width = 12, solidHeader = TRUE, status = "primary",
                    p("Dataset ini digunakan untuk menggali preferensi publik antara dua jenis liburan favorit: pegunungan dan pantai. Melalui analisis mendalam terhadap data demografi dan gaya hidup, pengguna dapat menemukan pola unik yang memengaruhi pilihan liburan. Visualisasi yang menarik membantu mengungkap apakah seseorang cenderung memilih suasana pegunungan yang menenangkan atau pesona pantai yang menyegarkan berdasarkan karakteristik mereka."),
                    h4("Struktur Dataset:"),
                    p("Jumlah Instance: 52444"),
                    p("Jumlah Fitur: 13"),
                    h4("Tentang File ini"),
                    p("Fitur-fitur dalam dataset ini meliputi:"),
                    tags$ul(
                      tags$li("Usia: Usia individu (numerik)."),
                      tags$li("Jenis Kelamin: Identitas gender individu (kategorikal: laki-laki, perempuan, non-biner)."),
                      tags$li("Penghasilan: Pendapatan tahunan individu (numerik)."),
                      tags$li("Pendidikan: Tingkat pendidikan tertinggi yang dicapai (kategorikal: SMA, sarjana, master, doktor)."),
                      tags$li("Frekuensi Liburan: Jumlah liburan yang diambil per tahun (numerik)."),
                      tags$li("Aktivitas yang Disukai: Aktivitas yang disukai individu saat liburan (kategorikal: hiking, berenang, ski, berjemur)."),
                      tags$li("Anggaran Liburan: Anggaran yang dialokasikan untuk liburan (numerik)."),
                      tags$li("Lokasi: Jenis tempat tinggal (kategorikal: perkotaan, pinggiran kota, pedesaan)."),
                      tags$li("Jarak ke Pegunungan: Jarak dari pegunungan terdekat (numerik, dalam mil)."),
                      tags$li("Jarak ke Pantai: Jarak dari pantai terdekat (numerik, dalam mil)."),
                      tags$li("Musim Favorit: Musim yang disukai untuk liburan (kategorikal: musim panas, musim dingin, musim semi, musim gugur)."),
                      tags$li("Pemilik Hewan Peliharaan: Menunjukkan apakah individu memiliki hewan peliharaan (biner: 0 = Tidak, 1 = Ya)."),
                      tags$li("Sensitivitas Lingkungan: Menunjukkan apakah individu memiliki kekhawatiran terhadap lingkungan (biner: 0 = Tidak, 1 = Ya).")
                    )
                )
              )
      ),
      
      tabItem(tabName = "overview",
              fluidRow(
                box(title = tags$span(
                  tags$img(src = "versus.png", height = "40px", width = "40px"),
                  tags$span(
                    "Pantai vs Gunung", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotOutput("preference_pie", height = "200px"), width = 4),
                
                box(title = tags$span(
                  tags$img(src = "fiat-money.png", height = "40px", width = "40px"),
                  tags$span(
                    "Penghasilan vs Budget Liburan", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotOutput("income_budget", height = "200px"), width = 4)
              ),
              
              
              fluidRow(
                box(title = tags$span(
                  tags$img(src = "education.png", height = "40px", width = "40px"),
                  tags$span(
                    "Tingkat Pendidikan", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotOutput("education_preference", height = "200px"), width = 4),
                
                box(title = tags$span(
                  tags$img(src = "calendar.png", height = "40px", width = "40px"),
                  tags$span(
                    "Frekuensi Liburan", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotOutput("vacation_frequency_comparison", height = "200px"), width = 4)
              ),
              
              
              fluidRow(
                box(title = tags$span(
                  tags$img(src = "gender-equality.png", height = "40px", width = "40px"),
                  tags$span(
                    "Jenis Kelamin", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotOutput("gender_preference", height = "200px"), width = 4),
                
                box(title = tags$span(
                  tags$img(src = "calendar.png", height = "40px", width = "40px"),
                  tags$span(
                    "Frekuensi Liburan", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotOutput("vacation_frequency", height = "200px"), width = 4)
              ),
              
              
              fluidRow(
                box(title = tags$span(
                  tags$img(src = "weather.png", height = "40px", width = "40px"),
                  tags$span(
                    "Musim Favorit", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotOutput("favorite_season", height = "200px"), width = 4),
                
                box(title = tags$span(
                  tags$img(src = "age-group.png", height = "40px", width = "40px"),
                  tags$span(
                    "Umur", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotOutput("age_preference", height = "200px"), width = 4)
              )
      ),
      
      # PART III: EDA
      tabItem(tabName = "eda",
              fluidRow(
                # Box for file upload
                box(title = "Pilih Dataset", status = "primary", solidHeader = TRUE,
                    radioButtons("dataset_choice", "Pilih Sumber Dataset:",
                                 choices = c("Data saat ini (Dashboard)" = "default",
                                             "Unggah Data" = "file"),
                                 selected = "default"),
                    conditionalPanel(
                      condition = "input.dataset_choice == 'file'",
                      fileInput("data_file", "Pilih File CSV", accept = ".csv"),
                      p("Pilih file CSV yang berisi data Anda. Setelah data diunggah, Anda akan dapat menggunakan data ini untuk analisis lebih lanjut.")
                    ),
                    width = 12
                )
              ),
              
              # a) X numerik vs Y (Boxplot/KDEplot + Uji Beda Nilai Tengah)
              fluidRow(
                box(title = "Boxplot X Numerik vs Y", status = "primary", solidHeader = TRUE,
                    width = 6,
                    selectInput("numeric_x", "Pilih variabel numerik untuk X:", choices = NULL),
                    selectInput("y_variable", "Pilih variabel kategorik Y:", choices = NULL),
                    plotOutput("boxplot_kde", height = "300px")
                ),
                
                # b) X kategorik vs Y (Barplot)
                # Box untuk Barplot
                box(title = "Barplot X Kategorik vs Y", status = "primary", solidHeader = TRUE, width = 6,
                    # RenderUI untuk memilih variabel X
                    uiOutput("categorical_x"),
                    # RenderUI untuk memilih variabel Y
                    uiOutput("categorical_y"),
                    plotOutput("barplot_output", height = "300px")  # Menampilkan barplot
                )
              ),
              
              fluidRow(
                # UI for the test result output
                box(
                  title = "Uji Beda Nilai Tengah (T-test / ANOVA)", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 6,
                  verbatimTextOutput("median_test_output")  # Ensure this output ID is correctly referenced
                ),
                
                # UI for the test result output
                box(
                  title = "Uji Chi-Square", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 6,
                  verbatimTextOutput("chi_square_test_output"),  # Menampilkan hasil uji Chi-Square
                ),
                
                
                # c) Scatter plot 2 Kontinu 
                box(title = "Scatter Plot (2 variabel kontinu)", status = "primary", solidHeader = TRUE,
                    width = 12,
                    selectInput("x_continuous", "Pilih variabel kontinu untuk X:", choices = NULL),
                    selectInput("y_continuous", "Pilih variabel kontinu untuk Y:", choices = NULL),
                    plotOutput("scatter_plot", height = "250px")
                )
              )
      ),
      
      
      # Tab Regresi Logistik
      tabItem(tabName = "logistic_regression",
              fluidRow(
                box(title = tags$span(
                  tags$span(
                    "Pilih Dataset", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), width = 4,
                radioButtons("data_source", "Sumber Data",
                             choices = c("Data saat ini (Dashboard)" = "example",
                                         "Unggah Data" = "upload")),
                conditionalPanel(
                  condition = "input.data_source == 'upload'",
                  fileInput("file", "Unggah File CSV", accept = ".csv")
                ),
                actionButton("load_data", "Muat Data"),
                checkboxInput('close_data_table', 'Tutup Tampilan Data', value=F)
                ),
                
                box(title = tags$span(
                  tags$span(
                    "Dataset", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), width = 8,dataTableOutput("data_table")
                )
                
                
              ),
              
              fluidRow(
                box(title = tags$span(
                  tags$span(
                    "Pengaturan", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), width = 4,
                uiOutput("select_response"),
                uiOutput("select_predictors"),
                # Tombol Select All dan Unselect All
                actionButton(inputId = "add_all", 
                             label = HTML('<i class="fas fa-check"></i> Pilih Semua'),
                             class = "custom-button"),
                actionButton(inputId = "remove_all", 
                             label = HTML('<i class="fas fa-times"></i> Batal Pilih Semua'),
                             class = "custom-button"),
                
                radioButtons("coef_format", "Format Koefisien", 
                             choices = c("Asli (Log-odds)" = "original", "Odds-Ratio" = "odds_ratio"),
                             inline = TRUE),
                selectInput("confidence_level", "Pilih Tingkat Kepercayaan",
                            choices = c("0.90", "0.95", "0.99"), selected = "0.95"),
                
                # Evaluasi Model
                tags$p("Multikolinearitas dan Evaluasi Model",
                       style = "
                  color: black; 
                  font-size: 14px; 
                  font-weight: bold; 
                  text-align: left;
                  margin-top: 30px;
                  margin-bottom: 0px;"),
                
                checkboxInput('multikol', 'Cek Multikolinearitas', value=F),
                checkboxInput('eval_model', 'AIC-BIC/Deviance', value=F),
                checkboxInput('conf_mat', 'Confusion Matrix', value=F),
                
                
                # Menambahkan margin vertikal antar tombol
                tags$style(HTML(
                  "
                  #selected_predictors 
                  {
                    margin-bottom: 1px;  /* Jarak kecil antara selected_predictors dan tombol */
                  }
                  
                  .custom-button {
                    margin-bottom: 30px;  /* Jarak kecil antara tombol add_all dan remove_all */
                  }
                  
                  #coef_format {
                    margin-bottom: 30px;  /* Jarak besar antara add_all/remove_all dan coef_format */
                  }
                  
                  #confidence_level
                  {
                    margin-top: 30px;  /* Jarak besar antara coef_format dan confidence_level */
                  }
                  
                "
                )),
                
                actionButton("fit_model", "Jalankan Model")
                
                ),
                
                box(title = tags$span(
                  tags$span(
                    "Hasil Regresi Logistik", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), width = 8,
                DT::dataTableOutput("logistic_summary"),
                downloadButton("download_csv", "Unduh CSV"))
              ),
              
              
              fluidRow(
                
                box(title = tags$span(
                  tags$span(
                    "Pemeriksaan Multikolinearitas", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), width = 4,
                DTOutput("vif_table")),
                
                box(
                  title = tags$span(
                    tags$span(
                      "Nilai AIC BIC Deviance", 
                      style = "color: white; font-weight: bold; font-size: 20px; text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5)"
                    )
                  ),
                  width = 4,
                  uiOutput("aic_deviance")
                ),
                
                
                box(
                  title = tags$span(
                    tags$span(
                      "Confusion Matrix", 
                      style = "color: white; font-weight: bold; font-size: 20px; text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5)"
                    )
                  ),
                  width = 4,
                  plotOutput("conf_matrix", height = "200px"),
                  uiOutput("conf_matrix_acc")
                )
                
                
              )
      )
    )
  )
)
