library(shiny)
library(readxl)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(dplyr)
library(jsonlite)
library(plotly)
library(sf)
library(rlang)


ui <- fluidPage(
  tags$head(tags$style(HTML(".sidebar, .well {background-color : lightblue;}"))),
  titlePanel("Analyse des elections presidentielles selon la loi de Benford"),
  sidebarLayout(
    sidebarPanel(
      selectInput("election_year", "Annee de l'election :", choices = c("1965", "1969", "1974", "1981", "1988",
                                                                        "1995", "2002", "2007", "2012", "2017", "2022")),
      selectInput("election_round", "Tour de l'election :", choices = c("tour 1", "tour 2")),
      uiOutput("candidate_list"),
      radioButtons("n_digits", "Nombre de chiffres pour la loi de Benford :", choices = c(1, 2), 
                   selected = 1),
      radioButtons("analysis_option", "Option d'analyse :", choices = c("Nationale", "Par regions"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graphique",
                 plotOutput("benford_plot"),
                 h3("Conditions prealables a remplir :"),
                 tags$div(style = "display: flex; align-items: center;",
                          tags$p("- Nombre d'observations :  ", style = "margin-right: 5px; line-height: 1;"),
                          tags$div(
                            style = "display: flex; flex-direction: row; align-items: center;",
                            tags$span(
                              textOutput("n_rows"), 
                              style = "margin-right: 5px; line-height: 1;"),
                            conditionalPanel(condition = "output.n_rows > 1000",
                                             tags$span("\U{2705}", style = "color: green;")
                            ),
                            conditionalPanel(
                              condition = "output.n_rows <= 1000",
                              tags$span("\U{274C}", style = "color: red;"))
                          )),
                 tags$div(style = "display: flex; align-items: center;",
                          tags$p("- Ordre de grandeur robuste (superieur a 3) : "),
                          textOutput("rom"),
                          conditionalPanel(condition = "output.rom > 3",
                                           tags$span("\U{2705}", style = "color: green;")
                          ),
                          conditionalPanel(
                            condition = "output.rom <= 3",
                            tags$span("\U{274C}", style = "color: red;"))),
                 h5("(Un ordre de grandeur robuste (OGR) entre 2,5 et 3 peut etre suffisant pour obtenir une distribution de Benford, mais pas en dessous de 2,5.)"),
                 tags$div(style = "display: flex; align-items: center;",
                          tags$p("- Asymetrie de la distribution vers la droite : Voir Histogramme")),
                 h3("Tests de conformite avec la loi de Benford :"),
                 h5(tags$i("(Voir 'Carte' pour les resultats par regions)")),
                 tags$div(style = "display: flex; align-items: center;",
                          tags$p("- Somme des carres des differences (SSD) : "),
                          tags$span(textOutput("ssd"), style = "margin-right: 10px;"),
                          uiOutput("ssd_status")),
                 tags$div(style = "display: flex; align-items: center;",
                          tags$p("- Ecart Moyen Absolu (MAD) : "),
                          tags$span(textOutput("mad"), style = "margin-right: 10px;"),
                          uiOutput("mad_status"))),
        tabPanel("Carte", 
                 leafletOutput("benford_map"),
                 tableOutput("ssd_table")),
        tabPanel("Histogramme", 
                 plotOutput("histogram"),
                 h5(tags$i("Source des donnees : Ministere de l'Interieur, data.gouv.fr.")),
                 h5(tags$i("References :")),
                 h5(tags$i("- Alex Ely Kossovsky, On the Mistaken Use of the Chi-Square Test in Benford's Law, 2021")),
                 h5(tags$i("- Alex Ely Kossovsky, Benford's Law: Theory, The General Law of Relative Quantities, And Forensic Fraud Detection Applications, 2015")),
                 h5(tags$i("- Mark J. Nigrini, Benford's Law: Applications for Forensic Accounting, Auditing, and Fraud Detection, 2012")),
                 h5(tags$i("- Steven J. Miller, Benford's Law: Theory and Application, 2015"))),
        tabPanel("Commentaires",
                 h5("Deux elements sont necessaires pour pouvoir observer un comportement suivant la loi de Benford : un ordre de grandeur robuste superieur a 3 (acceptable a partir de 2,5), et une distribution asymetrique avec une longue traine vers la droite."),
                 tags$br(),
                 h5("De 1965 a 2012, les donnees par circonscriptions sont utilisees, mais l'analyse de Benford ne peut pas etre appliquee : l'ordre de grandeur robuste est trop faible, et les distributions des donnees sont symetriques, suivant davantage une courbe normale."),
                 h5("De meme, en 2017, les resultats par cantons ont ete utilisees mais les conditions d'application ne sont pas remplies non plus. Pour le deuxieme tour, certains cantons ayant le meme nom (pour les plus grosses agglomerations) ont ete regroupes, mais l'ordre de grandeur reste trop bas."),
                 tags$br(),
                 h5("Seules les donnees de 2022 (donnees de chaque bureau de votes regroupees par villes) remplissent les conditions (a part pour certains candidats), et deux tests sont effectues pour verifier si les donnees sont conformes ou non avec la loi de Benford : la somme des carres des differences, et l'ecart moyen absolu."),
                 tags$br(),
                 h5("Par ailleurs, l'analyse regionale n'est valable que pour les plus gros candidats principalement (toujours pour 2022), et encore, pas sur l'ensemble des regions, l'ordre de grandeur robuste (OGR) etant trop bas dans certaines regions."),
                 tags$br(),
                 h5("Pour en savoir plus, voir references sous l'histogramme.")
                    )
      )
    )
  )
)




server <- function(input, output, session) {
  
  data <- reactive({
    # Charger les donnees des elections en fonction de l'annee et du tour selectionnes
    req(input$election_year)

    file_name <- paste0("cdsp_presi", input$election_year, "t", gsub("tour ", "", tolower(input$election_round)), 
                        "_circ.xls")
    read_csv(file_name, col_names = TRUE)
  })
  
  observeEvent(c(input$election_year, input$election_round), {
    
    req(input$election_year, data())
    
    new_candidates <- colnames(data())[8:length(colnames(data()))]
    updateSelectInput(session, "candidate", choices = new_candidates, selected = new_candidates[1])
  }, ignoreInit = TRUE)
  
  available_columns <- reactive({
    req(input$election_year, input$election_round, data())
    
    candidate_columns <- colnames(data())[8:length(colnames(data()))]
    
    return(candidate_columns)
  })
  
  
  
  output$n_rows <- renderText({ nrow(data()) })
  
  digits <- reactive({
    
    req(input$n_digits)
    
    if (input$n_digits == 1) {
      1:9
    } else {
      10:99
    }
  })
  
  benford <- reactive({
    
    req(digits())
    
    log10(1 + 1/digits())
  })
  
  cand_data <- reactive({
    req(input$election_year, input$candidate, input$n_digits)
    
    if (input$candidate %in% available_columns()) {
      candidate_data <- data() %>%
        select(all_of(input$candidate)) %>%
        filter(.data[[input$candidate]] > 0)
    } else {
      return(NULL)
    }
    # Compute Benford's Law expected frequencies
    digits <- if (input$n_digits == 1) {
      1:9
    } else {
      10:99
    }
    benford <- log10(1 + 1/digits)
    names(benford) <- digits
    
    # Create data.frame with all possible digits
    all_digits <- data.frame(Digit = digits)
    
    # Compute observed frequencies of first digit(s) in data
    if (input$n_digits == 1) {
      first_digits <- candidate_data %>%
        mutate(first_digit = floor(.data[[input$candidate]] / 10^(floor(log10(.data[[input$candidate]]))))) %>%
        count(first_digit) %>%
        mutate(freq = n/sum(n))
    } else {
      first_digits <- candidate_data %>%
        mutate(first_two_digits = floor(.data[[input$candidate]] / 10^(floor(log10(.data[[input$candidate]]))-1))) %>%
        count(first_two_digits) %>%
        mutate(freq = n/sum(n))
    }

    # Plot observed vs. expected frequencies
    plot_data <- all_digits %>%
      left_join(first_digits, by = c("Digit" = if(input$n_digits == 1) "first_digit" else "first_two_digits")) %>%
      rename(Observed = freq) %>%
      replace_na(list(Observed = 0)) %>%
      mutate(Expected = benford)

    
    return(plot_data)
  })
  
  
   output$candidate_list <- renderUI({
     
    req(input$election_year, input$election_round, data())
    
    selectInput("candidate", "Candidat :", choices = colnames(data())[8:length(colnames(data()))])
  })
   
  
  output$benford_plot <- renderPlot({
    
    req(input$candidate, cand_data())
    
    cand_data_long <- cand_data() %>%
      pivot_longer(cols = c(Observed, Expected), names_to = "Variable", values_to = "Value")

    if(input$n_digits == 1){
      
      cand_data_long %>%
        ggplot(aes(x = Digit, y = Value)) +
        geom_col(aes(fill = Variable), position = "dodge", width = 0.4, color = "black") +
        scale_fill_manual(values = c("Observed" = "lightblue", "Expected" = "blue")) +
        theme_minimal() +
        scale_x_continuous(
          breaks = 1:9,
          limits = c(0.5, 9.5),
          expand = c(0, 0)
        ) +
        ggtitle("Analyse nationale avec le premier chiffre significatif") +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "right")
      
    } else {
      
      cand_data_long %>%
        ggplot(aes(x = Digit, y = Value)) +
        geom_col(aes(fill = Variable), position = "dodge", width = 0.4) +
        scale_fill_manual(values = c("Observed" = "black", "Expected" = "blue")) +
        theme_minimal() +
        scale_x_continuous(
          breaks = seq(10, 100, by = 10),
          limits = c(9.5, 99.5),
          expand = c(0, 0)
        ) +
        ggtitle("Analyse nationale avec les deux premiers chiffres") +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "right")
    }

  })
  
  hist_data <- reactive({
    req(input$election_year, input$candidate)
    
    if (input$candidate %in% available_columns()) {

        candidat_hist_data <- data() %>%
          select(all_of(input$candidate)) %>%
          filter(.data[[input$candidate]] > 0)

    } else {
      return(NULL)
    }
    
    return(list(data = candidat_hist_data, col_name = input$candidate))
  })
  
  df_dpt_reg <- read_csv("df_dpt_reg.csv", col_names = TRUE)
  
  reg_data <- reactive({
    req(input$election_year, input$candidate, input$election_round, data())
    
    data_with_reg <- data() %>% left_join(df_dpt_reg, by = c("code_dpt" = "DEP"))

    return(data_with_reg)
  })
  
  output$histogram <- renderPlot({
    
    req(hist_data())
    
    n <- nrow(hist_data()$data)
    n_bins <- round(sqrt(n))
    
    ggplot(hist_data()$data, aes(x = !!sym(hist_data()$col_name))) + 
      geom_histogram(bins = n_bins) +
      theme_minimal()

  })
  
  rom_func <- function(data){
    rom <- log10(quantile(data, 0.99) / quantile(data, 0.01))
    return(rom)
  }
  
  rom_value <- reactive({
    req(input$election_year, input$candidate, hist_data())
    
    rom_res <- rom_func(as_vector(hist_data()$data))
    
    return(rom_res)
  })
  
  output$rom <- renderText({ 
    
    round(log10(quantile(
      as_vector(hist_data()$data), 0.99, na.rm = TRUE) / quantile(as_vector(hist_data()$data), 0.01, na.rm = TRUE)), 2) 
  })
  
  calcul_frequences <- function(data, n_digits) {
    if (n_digits == 1) {
      dgt <- floor(data / (10 ^ (floor(log10(data)))))
    } else {
      dgt <- floor(data / 10^(floor(log10(data))-1))
    }
    
    freqs <- table(factor(dgt, levels = digits()))
    return(freqs)
  }
  
  
  ssd_value <- reactive({
    
    req(input$election_year, input$candidate, hist_data(), benford())
    
    c_data <- hist_data()$data
    

    observed_counts <- calcul_frequences(c_data[[1]], input$n_digits)

    observed_proportions <- observed_counts / sum(observed_counts)
    
    # Differences entre les proportions observees et les proportions attendues
    differences <- (observed_proportions - benford())*100
    
    # Differences au carre
    squared_differences <- differences^2
    
    # Supprimer les NaN
    squared_differences_clean <- na.omit(squared_differences)
    
    # Somme des carres des differences (SSD)
    SSD <- sum(squared_differences_clean)
    
    return(SSD)
  })
  
  output$ssd <- renderText({
    
    req(input$candidate)
    
    round(ssd_value())
  })
  
  output$ssd_status <- renderUI({
    req(input$election_year, input$candidate,input$n_digits, ssd_value())
    
    status_text <- switch(
      input$n_digits,
      "1" = {
        if (ssd_value() < 2) {
          tags$span("\U{2705}", style = "color: green;")
        } else if (ssd_value() >= 100) {
          tags$span("\U{274C}", style = "color: red;")
        } else if (ssd_value() >= 2 & ssd_value() < 25) {
          tags$span("Acceptable")
        } else if (ssd_value() >= 25 & ssd_value() < 100) {
          tags$span("Legerement Benford")
        }
      },
      "2" = {
        if (ssd_value() < 2) {
          tags$span("\U{2705}", style = "color: green;")
        } else if (ssd_value() >= 50) {
          tags$span("\U{274C}", style = "color: red;")
        } else if (ssd_value() >= 2 & ssd_value() < 10) {
          tags$span("Acceptable")
        } else if (ssd_value() >= 10 & ssd_value() < 50) {
          tags$span("Legerement Benford")
        }
      }
    )
    
    return(status_text)
  })
  
  mad_value <- reactive({
    
    req(input$election_year, input$candidate, input$n_digits, hist_data(), benford())
    
    c_data <- hist_data()$data
    
    observed_counts_mad <- calcul_frequences(c_data[[1]], input$n_digits)
    observed_proportions <- observed_counts_mad / sum(observed_counts_mad)
    
    expected <- benford()
    
    differences <- (observed_proportions - expected)
    
    mad_value <- sum(abs(differences)) / length(expected)
    
    return(mad_value)
    
  })
  
  
  output$mad_status <- renderUI({
    req(input$election_year, input$candidate,input$n_digits, mad_value())
    
    status_text_mad <- switch(
      input$n_digits,
      "1" = {
        if (mad_value() < 0.006) {
          tags$span("\U{2705}", style = "color: green;")
        } else if (mad_value() >= 0.015) {
          tags$span("\U{274C}", style = "color: red;")
        } else if (mad_value() >= 0.006 & mad_value() < 0.012) {
          tags$span("Acceptable")
        } else if (mad_value() >= 0.012 & mad_value() < 0.015) {
          tags$span("Legerement Benford")
        }
      },
      "2" = {
        if (mad_value() < 0.0012) {
          tags$span("\U{2705}", style = "color: green;")
        } else if (mad_value() >= 0.0022) {
          tags$span("\U{274C}", style = "color: red;")
        } else if (mad_value() >= 0.0012 & mad_value() < 0.0018) {
          tags$span("Acceptable")
        } else if (mad_value() >= 0.0018 & mad_value() < 0.0022) {
          tags$span("Legerement Benford")
        }
      }
    )
    
    return(status_text_mad)
  })
  
  output$mad <- renderText({
    
    req(input$candidate)
    
    round(mad_value(), 5)
  })
  

  
  ssd_region <- function(data, n_digits) {
    observed_counts <- calcul_frequences(data, n_digits)
    observed_proportions <- observed_counts / length(data)
    
    # Differences entre les proportions observees et les proportions attendues
    differences <- (observed_proportions - benford()) * 100
    
    # Differences au carre
    squared_differences <- differences^2
    
    # Supprimer les NaN
    squared_differences_clean <- na.omit(squared_differences)
    
    # Somme des carres des differences (SSD)
    SSD <- sum(squared_differences_clean)
    
    return(SSD)
  }
  
  mad_benford_nigrini <- function(data, n_digits) {
    observed_counts <- calcul_frequences(data, n_digits)
    observed_proportions <- observed_counts / length(data)
    expected <- benford()
    
    MAD <-sum(abs(observed_proportions - expected)) / length(expected)
    
    return(MAD)
  }
  
  
  ssd_nat_region <- reactive({
    
    req(input$analysis_option, ssd_value(), reg_data(), input$n_digits)
    
    
    if(input$analysis_option == "Nationale"){
      
      national_data <- data.frame(Zone = "Nationale", SSD = ssd_value(), MAD = mad_value(), OGR = rom_value())
      
      # Ajout Paliers MAD
      national_data$Palier_MAD <- cut(
        national_data$MAD,
        breaks = c(0, 0.006, 0.012, 0.015, Inf),
        labels = c("Conforme", "Acceptable", "Legerement Benford", "Non conforme"),
        right = FALSE
      )
      
      return(national_data)
      
    } else {
      
      grouped_data <- reg_data() %>%
        group_by(nom_region)

      # Calculer la SSD et la MAD pour chaque region
      stats_by_region <- grouped_data %>% rename(Zone = nom_region) %>%
        summarise(
          SSD = ssd_region(.data[[input$candidate]], input$n_digits),
          MAD = mad_benford_nigrini(.data[[input$candidate]], input$n_digits),
          OGR = rom_func(as_vector(.data[[input$candidate]])),
          .groups = "drop"
        )
      
      # Creer un dataframe avec toutes les regions
      all_regions <- data.frame(Zone = (unique(df_dpt_reg$nom_region)))
      
      # Fusionner les donnees de SSD et MAD par region avec toutes les regions
      stats_by_region_complete <- merge(all_regions, stats_by_region, by = "Zone", all.x = TRUE)
      stats_by_region_complete <- na.omit(stats_by_region_complete)
      
      # Ajouter une colonne pour indiquer differents paliers en fonction du resultat MAD
      stats_by_region_complete$Palier_MAD <- cut(
        stats_by_region_complete$MAD,
        breaks = c(0, 0.006, 0.012, 0.015, Inf),
        labels = c("Conforme", "Acceptable", "Legerement Benford", "Non conforme"),
        right = FALSE
      )
      
      return(stats_by_region_complete)
    }
  })
  

  output$ssd_table <- renderTable({
    req(ssd_nat_region())
    ssd_nat_region()
  })
  
  # Charger les donnees de la carte de France
  france_geojson <- st_read("france_geojson.shp")
  
  dpt_reg_geo  <- france_geojson %>% left_join(df_dpt_reg, by = c("code" = "REG"))

  france_geojson_with_color <- reactive({
    req(france_geojson, dpt_reg_geo, ssd_nat_region(), input$analysis_option, input$n_digits)
    
    # Jointure
    joined_data <- merge(dpt_reg_geo, ssd_nat_region(),
                         by.x = "nom_region", by.y = "Zone", all.x = TRUE)

    # Remplacer les NA par la valeur nationale de SSD pour l'option nationale
    if (input$analysis_option == "Nationale") {
      joined_data$SSD[is.na(joined_data$SSD)] <- ssd_nat_region()[[2]][1]
    }
    
    # Paliers de couleurs en fonction du chiffre significatif choisi
    color_thresholds <- if (input$n_digits == "1") {
      c(2, 25, 100)
    } else {
      c(2, 10, 50)
    }
    
    
    # Appliquer les couleurs en fonction de la colonne SSD
    joined_data$fillColor <- case_when(
      joined_data$SSD < color_thresholds[1] ~ "green",
      joined_data$SSD >= color_thresholds[1] & joined_data$SSD < color_thresholds[2] ~ "lightgreen",
      joined_data$SSD >= color_thresholds[2] & joined_data$SSD < color_thresholds[3] ~ "yellow",
      joined_data$SSD >= color_thresholds[3] ~ "red",
      TRUE ~ "blue"
    )

    return(joined_data)
  })
  
  
  
  output$benford_map <- renderLeaflet({
    req(france_geojson_with_color())
    
    benford_map <- leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lng = 2.5, lat = 47, zoom = 5) %>%
      addPolygons(data = france_geojson_with_color(),
                  fillColor = ~fillColor,
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  fillOpacity = 0.7) %>%
      addLegend(
        position = "topright",
        title = "Paliers SSD",
        colors = c("green", "lightgreen", "yellow", "red"),
        labels = c("Parfaitement conforme", "Acceptable", "Legerement Benford", "Non conforme"),
        opacity = 1
      )
    
    benford_map
  })
  
  observe({
    req(input$analysis_option, france_geojson_with_color())
    
    if (input$analysis_option == "Nationale") {
      leafletProxy("benford_map") %>%
        removeShape(layerId = paste0("region_", 1:nrow(france_geojson_with_color()))) %>%
        addPolygons(data = france_geojson_with_color(),
                    fillColor = ~fillColor,
                    weight = 0,
                    opacity = 0,
                    fillOpacity = 0.7,
                    layerId = paste0("region_", 1:nrow(france_geojson_with_color())))
    } else {
      leafletProxy("benford_map") %>%
        removeShape(layerId = paste0("region_", 1:nrow(france_geojson_with_color()))) %>%
        addPolygons(data = france_geojson_with_color(),
                    fillColor = ~fillColor,
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.7,
                    layerId = paste0("region_", 1:nrow(france_geojson_with_color())))
    }
  })

 


  
  
}

shinyApp(ui = ui, server = server)
