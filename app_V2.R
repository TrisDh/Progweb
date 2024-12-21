library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(shinydashboard)
library(readr)
library(caret)
library(randomForest)
library(xgboost)
library(e1071)
library(ROCR)
library(DT)
library(tidyr)
library(corrplot)

# Fonction pour identifier les types de variables
detect_var_type <- function(data, var) {
  if (is.numeric(data[[var]])) {
    return("quantitative")
  } else {
    return("qualitative")
  }
}

# Fonction pour discrétiser une variable quantitative si nécessaire
discretize_variable <- function(data, var, n_classes = 6) {
  if (length(unique(data[[var]])) > n_classes) {
    data[[var]] <- cut(data[[var]], breaks = n_classes, include.lowest = TRUE, labels = FALSE)
    data[[var]] <- factor(data[[var]], labels = paste("Classe", 1:n_classes))
  }
  return(data)
}

# Interface utilisateur
ui <- navbarPage(theme = shinytheme("darkly"),
  title = "Visualisation des Données et Prédictions",
  
  # Onglet principal : "Analyse et exploration"
  tabPanel(
    "Analyse et exploration",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Importer un fichier CSV", accept = ".csv"),
        uiOutput("var_select"),
      ),
      mainPanel(
        h4("Extrait des 10 premières lignes de la base de données"),
        tableOutput("data_sample"),
        textOutput("test_info"),  
        plotOutput("graph_plot"),
        fluidRow(
          column(6, textOutput("name_var1")),
          column(6, textOutput("name_var2"))
        ),
        fluidRow(
          column(6, tableOutput("metrics_table_1")),
          column(6, tableOutput("metrics_table_2"))
        ),
        textOutput("correlation_info")
      )
    )
  ),
  
  # Nouvel onglet "Prédictions"
  tabPanel(
    "Prédictions",
    sidebarLayout(
      sidebarPanel(
        # Sélection de la variable à prédire
        uiOutput("var_pred"),
        
        # Sélection des features
        selectizeInput("features", "Features à utiliser:", choices = NULL, multiple = TRUE),
        
        # Choix du modèle
        selectInput("model", "Modèle de ML:",
                    choices = c("Random Forest" = "rf",
                                "SVM" = "svm",
                                "XGBoost" = "xgb")),
        
        # Proportion train/test
        sliderInput("split", "Proportion d'entrainement:",
                    min = 0.5, max = 0.9, value = 0.7),
        
        # Bouton pour lancer l'entrainement
        actionButton("train", "Entraîner le modèle", class = "btn-primary")
      ),
      mainPanel(
        tabsetPanel(
          # Onglets existants pour les résultats du modèle
          tabPanel("Résultats du modèle",
            conditionalPanel(
              condition = "output.modelTrained",
                     
              tabsetPanel(
                tabPanel("Métriques",
                    h3("Résultats de l'évaluation"),
                        verbatimTextOutput("metrics")),
                       
                tabPanel("Importance des features",
                    plotOutput("featImportance")),
                    
                tabPanel("Courbe ROC",
                    plotOutput("rocCurve"))
              )
            )
          )
        )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  # Charger les données avec gestion de l'encodage
  dataset <- reactive({
    req(input$file)
    tryCatch({
      raw_data <- read.csv(input$file$datapath, fileEncoding = "UTF-8")
      if (nrow(raw_data) == 0 || ncol(raw_data) == 0) {
        stop("Le fichier CSV semble vide ou mal formaté.")
      }
      raw_data
    }, error = function(e) {
      stop("Erreur : le fichier n'est pas en UTF-8. Veuillez convertir le fichier au codage UTF-8 avant de le réessayer.")
    })
  })
  
  # Variables pour la prédiction
  model <- reactiveVal()
  predictions <- reactiveVal()
  
  # Générer les sélections de variables
  output$var_select <- renderUI({
    req(dataset())
    vars <- names(dataset())
    tagList(
      selectInput("var1", "Variable 1 :", choices = vars),
      selectInput("var2", "Variable 2 :", choices = c("Aucune", vars))
    )
  })
  
  # Même opération pour la prédiction
  output$var_pred <- renderUI({
    req(dataset())
    vars <- names(dataset())
    selectInput("target", "Sélectionner une variable cible :", choices = vars)
  })
  
  # Chargement des données
  observeEvent(input$file, {
    df <- read_csv(input$file$datapath)
    data <- df
    
    # Mise à jour des choix de variables
    updateSelectInput(session, "target",
                      choices = names(df))
    updateSelectizeInput(session, "features",
                         choices = names(df))
    updateSelectInput(session, "var_to_plot",
                      choices = names(df))
  })
  
  # Entraînement du modèle
  observeEvent(input$train, {
    req(data(), input$target, input$features)
    
    # Préparation des données
    df <- data()
    features <- input$features
    target <- input$target
    
    # Création du jeu de données
    model_data <- df %>%
      select(all_of(c(target, features))) %>%
      na.omit()
    
    # Split train/test
    set.seed(123)
    trainIndex <- createDataPartition(model_data[[target]], p = input$split, list = FALSE)
    training <- model_data[trainIndex, ]
    testing <- model_data[-trainIndex, ]
    
    # Entraînement selon le modèle choisi
    fitted_model <- switch(input$model,
                           "rf" = {
                             rf_model <- randomForest(as.formula(paste(target, "~ .")),
                                                      data = training)
                             list(model = rf_model,
                                  pred = predict(rf_model, testing),
                                  importance = importance(rf_model))
                           },
                           "svm" = {
                             svm_model <- svm(as.formula(paste(target, "~ .")),
                                              data = training,
                                              probability = TRUE)
                             list(model = svm_model,
                                  pred = predict(svm_model, testing),
                                  importance = NULL)
                           },
                           "xgb" = {
                             # Préparation pour XGBoost
                             train_matrix <- model.matrix(as.formula(paste("~", paste(features, collapse = "+"))), data = training)
                             test_matrix <- model.matrix(as.formula(paste("~", paste(features, collapse = "+"))), data = testing)
                             
                             xgb_model <- xgboost(data = train_matrix,
                                                  label = training[[target]],
                                                  nrounds = 100,
                                                  objective = "binary:logistic")
                             
                             list(model = xgb_model,
                                  pred = predict(xgb_model, test_matrix),
                                  importance = xgb.importance(feature_names = colnames(train_matrix), model = xgb_model))
                           }
    )
    
    model(fitted_model)
    predictions(list(pred = fitted_model$pred,
                     actual = testing[[target]]))
  })
  
  # Indicateur de modèle entraîné
  output$modelTrained <- reactive({
    !is.null(model())
  })
  outputOptions(output, "modelTrained", suspendWhenHidden = FALSE)
  
  
  # Affichage des métriques
  output$metrics <- renderPrint({
    req(predictions())
    
    # Calcul des métriques
    pred_obj <- prediction(predictions()$pred, predictions()$actual)
    perf_auc <- performance(pred_obj, "auc")
    auc <- perf_auc@y.values[[1]]
    
    # Conversion en classes binaires pour les autres métriques
    pred_class <- ifelse(predictions()$pred > 0.5, 1, 0)
    conf_matrix <- table(Actual = predictions()$actual, Predicted = pred_class)
    
    # Calcul precision, recall, F1
    precision <- conf_matrix[2,2] / sum(conf_matrix[,2])
    recall <- conf_matrix[2,2] / sum(conf_matrix[2,])
    f1 <- 2 * (precision * recall) / (precision + recall)
    
    # Affichage
    cat("Métriques d'évaluation:\n\n")
    cat(sprintf("Precision: %.3f\n", precision))
    cat(sprintf("Recall: %.3f\n", recall))
    cat(sprintf("F1-Score: %.3f\n", f1))
    cat(sprintf("AUC: %.3f\n", auc))
  })
  
  # Graphique d'importance des features
  output$featImportance <- renderPlot({
    req(model())
    
    if (!is.null(model()$importance)) {
      importance_df <- as.data.frame(model()$importance)
      
      if (input$model == "xgb") {
        importance_df <- importance_df %>%
          arrange(desc(Gain)) %>%
          head(10)
        
        barplot(importance_df$Gain,
                names.arg = importance_df$Feature,
                main = "Importance des features (XGBoost)",
                las = 2)
      } else {
        importance_df$Feature <- rownames(importance_df)
        importance_df <- importance_df %>%
          arrange(desc(IncNodePurity)) %>%
          head(10)
        
        barplot(importance_df$IncNodePurity,
                names.arg = importance_df$Feature,
                main = "Importance des features (Random Forest)",
                las = 2)
      }
    }
  })
  
  # Courbe ROC
  output$rocCurve <- renderPlot({
    req(predictions())
    
    pred_obj <- prediction(predictions()$pred, predictions()$actual)
    perf <- performance(pred_obj, "tpr", "fpr")
    
    plot(perf,
         main = "Courbe ROC",
         colorize = TRUE)
    abline(a = 0, b = 1, lty = 2)
  })
  
  # Afficher l'extrait de la base de données (les 10 premières lignes)
  output$data_sample <- renderTable({
    req(dataset())
    head(dataset(), 10)  # Affiche les 10 premières lignes de la base de données
  })
  
  # Décrire le test ou méthode utilisée
  output$test_info <- renderText({
    req(input$var1, dataset())
    if (input$var2 == "Aucune") {
      return("Aucun test de corrélation effectué. Analyse univariée.")
    } else {
      data <- dataset()
      var1_type <- detect_var_type(data, input$var1)
      var2_type <- detect_var_type(data, input$var2)
      
      if (var1_type == "quantitative" && var2_type == "quantitative") {
        return("Test employé : Coefficient de corrélation de Pearson (mesure de la relation linéaire entre deux variables quantitatives).")
      } else if (var1_type == "qualitative" && var2_type == "qualitative") {
        return("Test employé : Test du Chi² pour évaluer l'association entre deux variables qualitatives.")
      } else {
        return("Méthode employée : Visualisation avec boxplot (analyse des différences entre les groupes d'une variable qualitative par rapport à une variable quantitative).")
      }
    }
  })
  
  # Créer le graphique
  output$graph_plot <- renderPlot({
    req(input$var1, dataset())
    data <- dataset()
    
    var1_type <- detect_var_type(data, input$var1)
    var2_type <- if (input$var2 != "Aucune") detect_var_type(data, input$var2) else "none"
    
    # Discrétiser les variables quantitatives si nécessaire
    if (var1_type == "quantitative") {
      data <- discretize_variable(data, input$var1)
    }
    if (var2_type == "quantitative") {
      data <- discretize_variable(data, input$var2)
    }
    
    # Cas univarié
    if (var2_type == "none") {
      if (var1_type == "qualitative") {
        ggplot(data, aes(x = .data[[input$var1]])) +
          geom_bar(fill = "skyblue") +
          labs(title = paste("Distribution de", input$var1), x = input$var1, y = "Fréquence")
      } else {
        ggplot(data, aes(x = .data[[input$var1]])) +
          geom_histogram(fill = "skyblue", bins = 30) +
          labs(title = paste("Distribution de", input$var1), x = input$var1, y = "Fréquence")
      }
    } else {
      # Cas bivarié
      if (var1_type == "qualitative" && var2_type == "qualitative") {
        chi_test <- chisq.test(table(data[[input$var1]], data[[input$var2]]))
        ggplot(data, aes(x = .data[[input$var1]], fill = .data[[input$var2]])) +
          geom_bar(position = "fill") +
          labs(
            title = paste("Diagramme empilé :", input$var1, "et", input$var2),
            x = input$var1, y = "Proportion", fill = input$var2
          ) +
          annotate("text", x = 1, y = 1, label = paste("p-value Chi² :", round(chi_test$p.value, 4)), color = "red")
      } else if (var1_type == "quantitative" && var2_type == "quantitative") {
        corr_value <- cor(data[[input$var1]], data[[input$var2]], use = "complete.obs")
        ggplot(data, aes(x = .data[[input$var1]], fill = factor(.data[[input$var2]]))) +
          geom_bar(position = "stack") +
          labs(
            title = paste("Diagramme en barres empilées :", input$var1, "et", input$var2),
            x = input$var1, y = "Fréquence", fill = input$var2
          ) +
          annotate("text", x = 1, y = 1, label = paste("Coefficient r :", round(corr_value, 2)), color = "red")
      } else {
        quanti_var <- if (var1_type == "quantitative") input$var1 else input$var2
        quali_var <- if (var1_type == "qualitative") input$var1 else input$var2
        
        ggplot(data, aes(x = .data[[quali_var]], y = .data[[quanti_var]])) +
          geom_boxplot(fill = "skyblue") +
          labs(
            title = paste("Boxplot :", quali_var, "et", quanti_var),
            x = quali_var, y = quanti_var
          )
      }
    }
  })
  
  # Informations sur la corrélation
  output$correlation_info <- renderText({
    req(input$var1, input$var2, dataset())
    if (input$var2 == "Aucune") {
      return("Analyse univariée uniquement.")
    } else {
      var1_type <- detect_var_type(dataset(), input$var1)
      var2_type <- detect_var_type(dataset(), input$var2)
      if (var1_type == "quantitative" && var2_type == "quantitative") {
        corr_value <- cor(dataset()[[input$var1]], dataset()[[input$var2]], use = "complete.obs")
        corr_interpretation <- if (abs(corr_value) > 0.7) {
          if (corr_value > 0) {
            "Il y a une forte corrélation positive entre les deux variables."
          } else {
            "Il y a une forte corrélation négative entre les deux variables."
          }
        } else if (abs(corr_value) > 0.3) {
          "Il y a une corrélation modérée entre les deux variables."
        } else {
          "Il n'y a pas de corrélation significative entre les deux variables."
        }
        
        return(paste("Coefficient de corrélation de Pearson :", round(corr_value, 2), "-", corr_interpretation))
      } else if (var1_type == "qualitative" && var2_type == "qualitative") {
        chi_test <- chisq.test(table(dataset()[[input$var1]], dataset()[[input$var2]]))
        p_value <- chi_test$p.value
        chi_interpretation <- if (p_value < 0.05) {
          "Il existe une association significative entre les deux variables."
        } else {
          "Aucune association significative n'a été trouvée entre les deux variables."
        }
        
        return(paste("Test Chi² : p-value =", round(p_value, 4), "-", chi_interpretation))
      } else if (var1_type == "quantitative" && var2_type == "qualitative") {
        # ANOVA pour les différences entre groupes
        aov_result <- aov(dataset()[[input$var1]] ~ dataset()[[input$var2]])
        p_value <- summary(aov_result)[[1]]["Pr(>F)"][1]
        aov_interpretation <- if (p_value < 0.05) {
          "Les moyennes des groupes sont significativement différentes."
        } else {
          "Aucune différence significative n'a été trouvée entre les moyennes des groupes."
        }
        
        return(paste("Test ANOVA : p-value =", round(p_value, 4), "-", aov_interpretation))
      }
    }
  })
  
  # Affichage du nom de la première variable
  output$name_var1 <- renderText({
    return(input$var1)
  })
  
  # Affichage du nom de la deuxième variable
  output$name_var2 <- renderText({
    return(input$var2)
  })
  
  # Affichage des métriques de base pour la première variable
  output$metrics_table_1 <- renderTable({
    req(input$var1, dataset())
    data <- dataset()
    var1 <- input$var1
    
    # Calcul des métriques de base
    if (detect_var_type(data, var1) == "quantitative") {
      summary_stats <- data.frame(
        Métrique = c("Moyenne", "Écart-type", "Variance", "Min", "1er Quartile", "Médiane", "3e Quartile", "Max"),
        Valeur = c(
          round(mean(data[[var1]], na.rm = TRUE), 2),
          round(sd(data[[var1]], na.rm = TRUE), 2),
          round(var(data[[var1]], na.rm = TRUE), 2),
          min(data[[var1]], na.rm = TRUE),
          quantile(data[[var1]], 0.25, na.rm = TRUE),
          median(data[[var1]], na.rm = TRUE),
          quantile(data[[var1]], 0.75, na.rm = TRUE),
          max(data[[var1]], na.rm = TRUE)
        )
      )
    } else {
      summary_stats <- data.frame(
        Métrique = c("Effectif", "Fréquence relative"),
        Valeur = c(
          length(data[[var1]]),
          prop.table(table(data[[var1]]))
        )
      )
    }
    
    return(summary_stats)
  })
  
  # Affichage des métriques de base pour la deuxième variable
  output$metrics_table_2 <- renderTable({
    req(input$var2, dataset())
    data <- dataset()
    var2 <- input$var2
    
    # Calcul des métriques de base
    if (detect_var_type(data, var2) == "quantitative") {
      summary_stats <- data.frame(
        Métrique = c("Moyenne", "Écart-type", "Variance", "Min", "1er Quartile", "Médiane", "3e Quartile", "Max"),
        Valeur = c(
          round(mean(data[[var2]], na.rm = TRUE), 2),
          round(sd(data[[var2]], na.rm = TRUE), 2),
          round(var(data[[var2]], na.rm = TRUE), 2),
          min(data[[var2]], na.rm = TRUE),
          quantile(data[[var2]], 0.25, na.rm = TRUE),
          median(data[[var2]], na.rm = TRUE),
          quantile(data[[var2]], 0.75, na.rm = TRUE),
          max(data[[var2]], na.rm = TRUE)
        )
      )
    } else {
      summary_stats <- data.frame(
        Métrique = c("Effectif", "Fréquence relative"),
        Valeur = c(
          length(data[[var2]]),
          prop.table(table(data[[var2]]))
        )
      )
    }
    
    return(summary_stats)
  })
}

# Lancer l'application
shinyApp(ui, server)
