#' Function to run the web application
#'
#' @details This function takes one argument, a dataframe, and
#' returns a shiny web app that allows various unsupervised learning
#' techniques to be applied to a dataframe
#'
#' @param input_data A dataframe
#'
#' @return a shiny web application
#' @author Josué AFOUDA
#'
#' @export
ShinyUL <- function(input_data, seed = 123) {

  set.seed(seed)

  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("flatly"),

    shiny::h1("UNSUPERVISED LEARNING METHODS FOR DATA MINING"),

    shiny::h2("Author : Josué AFOUDA"),

    shiny::tags$a("Follow me on Linkedin",href='https://www.linkedin.com/in/josu%C3%A9-afouda/'),

    shiny::sidebarLayout(
      shiny::sidebarPanel(

        shiny::selectInput('scaled_met', 'Normalization or Standardization of data:',
                           c('None',
                             'Normalization',
                             'Standardization')),

        shiny::selectInput('dim_reduction', 'Choose a Dimensionnality Reduction method:',
                           c('PCA',
                             'T-SNE')),

        shiny::conditionalPanel(
          condition = "input.dim_reduction == 'T-SNE'",
          shiny::checkboxInput("pca_tsne", "T-SNE with PCA or not:", FALSE),
          shiny::numericInput('n_iter', 'Number of iterations:', 1000, min = 1000, step = 500)
        ),

        shiny::helpText("Choose the 2 variables to represent in a scatter plot"),

        shiny::selectInput('varx', 'X-axis variable:', names(input_data)),

        shiny::selectInput('vary', 'Y-axis variable:', names(input_data)),

        shiny::helpText("Click on 'Submit' after each change of parameter(s)"),

        shiny::actionButton("go", "Submit")

      ),

      # Tableaux et Graphiques dans le panneau principal
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel('Cleaned Data', DT::DTOutput('dataframe'),
                          shiny::downloadButton('download_clean_data', label = 'Telechargez les donnees nettoyees :')),

          shiny::tabPanel('Norm/Stand Data', DT::DTOutput('scaled'),
                          shiny::downloadButton('download_stand_data', label = 'Telechargez les donnees standardisees ou nourmalisees :')),

          shiny::tabPanel('Dim. Reduction',
                          shiny::conditionalPanel(condition = "input.dim_reduction == 'PCA'",
                                                  shiny::plotOutput("pca_screeplot"),
                                                  shiny::plotOutput("pca_biplot"),
                                                  shiny::numericInput('first_cp', 'How many first components do you want to choose ?', 2, min = 1, step = 1),
                                                  DT::DTOutput('table_pca'),
                                                  shiny::downloadButton('save_pca_data', label = 'Save PCA Data :'),
                          ),

                          shiny::conditionalPanel(condition = "input.dim_reduction == 'T-SNE'",
                                                  shiny::plotOutput("tsne_costs_plot"),
                                                  DT::DTOutput("tsne_data"),
                                                  shiny::downloadButton('save_tsne_data', label = 'Save TSNE Data :'),

                                                  shiny::plotOutput('tsne_biplot'))

          ),

          shiny::tabPanel('Elbow Method', plotly::plotlyOutput('elbow', height = "550px")),

          shiny::tabPanel('Average Silhouette', plotly::plotlyOutput('silhouette', height = "550px")),

          shiny::tabPanel('Clusters Visualization',
                          shiny::helpText("The number of clusters is a strictly positive natural integer"),
                          shiny::numericInput('n_clusters', 'How many clusters?', value = 3, min = 1),
                          shiny::plotOutput("scatter_plot", 850, 600))

        )

      )
    )
  )

  server <- function(input, output, session) {

    # Nettoyage de la dataframe en entrée de l'application
    df <- cleaning_df(input_data)

    # Essai
    output$dataframe <- DT::renderDT({
      input$go
      isolate({
        #df
        DT::datatable(df, rownames = FALSE,
                      extensions = 'Buttons', options = list(
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      ),
                      filter = 'top', style = 'bootstrap')
      })
    })

    # L'utilisateur peut télécharger les données nettoyées au format csv
    output$download_clean_data <- downloadHandler(
      filename <- function() {
        paste("clean_data_", Sys.Date(), ".csv", sep=",")
      },
      content <- function(file) {
        write.csv(df, file)
      }
    )

    # Mise à l'échelle selon le choix (Normalisation ou Standardisation) de l'utilisateur
    data <- reactive({
      if (input$scaled_met == 'Normalization') {
        normalize(df)
      } else if (input$scaled_met == 'Standardization') {
        standardize(df)
      } else if (input$scaled_met == 'None') {
        df
      }
    })

    # PCA Model
    model_pca <- reactive({
      prcomp(data(), scale = FALSE, center = FALSE)
    })

    # PCA Scree Plot
    output$pca_screeplot <- shiny::renderPlot({
      input$go
      isolate({
        factoextra::fviz_screeplot(model_pca(), ncp = ncol(data()))
      })
    })

    # PCA Biplot
    output$pca_biplot <- shiny::renderPlot({
      input$go
      isolate({
        plot(model_pca()$x[, 1:2])
      })
    })

    # PCA Data
    output$table_pca <- DT::renderDT({
      input$go
      isolate({
        #as.data.frame(model_pca()$x[, 1:input$first_cp])
        DT::datatable(as.data.frame(model_pca()$x[, 1:input$first_cp]),
                      rownames = FALSE,
                      extensions = 'Buttons', options = list(
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      ),
                      filter = 'top', style = 'bootstrap')
      })
    })

    # Save PCA Data to csv format
    output$save_pca_data <- downloadHandler(
      filename <- function() {
        paste("pca_data_", Sys.Date(), ".csv", sep=",")
      },
      content <- function(file) {
        write.csv(as.data.frame(model_pca()$x[, 1:input$first_cp]), file)
      }
    )

    # TSNE model
    model_tsne <- reactive({
      Rtsne(data(), pca = input$pca_tsne, dims = 2,
            check_duplicates = FALSE, max_iter = input$n_iter)
    })

    # Costs Plot for TSNE
    output$tsne_costs_plot <- shiny::renderPlot({
      input$go
      isolate({
        plot(model_tsne()$costs, type = 'l', ylab = 'Costs')
      })
    })

    # TSNE 2 dim data
    output$tsne_data <- DT::renderDT({
      input$go
      isolate({
        ####
        DT::datatable(data.frame(tsne_x = model_tsne()$Y[, 1],
                                 tsne_y =model_tsne()$Y[, 2]),
                      rownames = FALSE,
                      extensions = 'Buttons', options = list(
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      ),
                      filter = 'top', style = 'bootstrap')
      })
    })

    # TSNE scatter plot
    output$tsne_biplot <- shiny::renderPlot({
      input$go
      isolate({
        plot(data.frame(tsne_x = model_tsne()$Y[, 1],
                        tsne_y =model_tsne()$Y[, 2]))
      })
    })

    # Save TSNE Data to csv format
    output$save_tsne_data <- downloadHandler(
      filename <- function() {
        paste("tsne_data_", Sys.Date(), ".csv", sep=",")
      },
      content <- function(file) {
        write.csv(data.frame(tsne_x = model_tsne()$Y[, 1],
                             tsne_y =model_tsne()$Y[, 2]), file)
      }
    )

    output$scaled <- DT::renderDT({

      input$go
      isolate({
        #####
        DT::datatable(data(),
                      rownames = FALSE,
                      extensions = 'Buttons', options = list(
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      ),
                      filter = 'top', style = 'bootstrap')
      })

    })

    output$download_stand_data <- downloadHandler(
      filename <- function() {
        paste("scaled_or_norm_data_", Sys.Date(), ".csv", sep=",")
      },
      content <- function(file) {
        write.csv(data(), file)
      }
    )

    output$elbow <- plotly::renderPlotly({
      input$go
      isolate({
        factoextra::fviz_nbclust(data(), FUNcluster = kmeans, method = "wss")
      })
    })

    output$silhouette <- plotly::renderPlotly({
      input$go
      isolate({
        factoextra::fviz_nbclust(data(), FUNcluster = kmeans, method = "silhouette")
      })
    })

    # K-Means Model
    model <- reactive({
      kmeans(data(), centers = input$n_clusters, nstart = 20)
    })

    # Selected columns for visualization of clustering
    data_selected <- reactive({
      cbind(data()[input$varx], data()[input$vary])
    })

    output$scatter_plot <- shiny::renderPlot({
      input$go
      isolate({
        plot(data_selected(), col = as.factor(model()$cluster), pch = 20, cex = 3)
      })
    })

  }
  shiny::shinyApp(ui = ui, server = server)
}
