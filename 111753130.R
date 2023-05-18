library(shiny)
library(ggbiplot)
library(ggplot2)
library(FactoMineR)
library(DT)
library(ggcorrplot)

a <- c('PC1', 'PC2', 'PC3', 'PC4')


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  #titlePanel("PCA"),
  navbarPage(" ", 
    tabPanel(icon("home"),
             fluidRow(p(strong(h3("HW4 Interactive web service of PCA and CA analysis by Shinyapp")))), 
             fluidRow(p(h4("Name: 高語謙"))), 
             fluidRow(p(h4("Student ID: 111753130"))), 
    ), 
    tabPanel("PCA analysis", icon = icon("chart-line"), 
             tabsetPanel(
               tabPanel('PCA Plot', 
                        sidebarLayout(
                          sidebarPanel(
                            selectizeInput('xcol', 'X axis', c('PC1', 'PC2', 'PC3', 'PC4'), selected = 'PC1'),
                            selectizeInput('ycol', 'Y axis', c('PC1', 'PC2', 'PC3', 'PC4'), selected = 'PC2'),
                          ), 
                          mainPanel(
                            plotOutput("pca")
                          ),  
                        ),
               ), 
               tabPanel('Result', 
                        mainPanel(
                          verbatimTextOutput('pca_res')
                        )
               )
             )
    ), 
    tabPanel("CA analysis", icon = icon("chart-line"),
             mainPanel(
               plotOutput("ca")

             )
             # sidebarLayout(
             #   sidebarPanel(
             # 
             # 
             #   ),
             # 
             #   mainPanel(
             #     plotOutput("ca")
             #     
             #   )
             # )
    ), 
    tabPanel("Data", icon = icon("database"), 
             tabsetPanel(
               tabPanel('raw data', 
                        mainPanel(
                         p(strong(h3(icon("spa"), " iris Data"))), 
                         br(), 
                         DT::dataTableOutput("data")
                       )
               ), 
               tabPanel('Summary', 
                        mainPanel(
                          verbatimTextOutput('summary'), 
                          plotOutput("cor_plot")
                        )
               )
             )
             
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  observeEvent(input$xcol, {
    updateSelectizeInput(session, "ycol", choices = a[a!=input$xcol], selected = input$ycol)
  })
  observeEvent(input$ycol, {
    updateSelectizeInput(session, "xcol", choices = a[a!=input$ycol], selected = input$xcol)
  })
 
  output$pca <- renderPlot({

    data(iris)
    # log transform 
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species, ellipse = TRUE, 
                  choices = c(as.integer(substr(input$xcol, 3, 3)), as.integer(substr(input$ycol, 3, 3)))) + 
                  scale_color_discrete(name = '') + 
                  theme(legend.direction = 'horizontal', legend.position = 'top')
    print(g)
  })
  
  
  output$ca <- renderPlot({
    data(iris)
    # log transform
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]

    res.ca <- CA(iris[,1:4])


    gr <- plot(res.ca) + 
          theme(panel.grid.major = element_blank(),
                plot.title=element_text(size=14, color="blue"),
                axis.title = element_text(size=12, color="red"))
    print(gr)

  })
  output$data <- DT::renderDataTable(datatable(iris))
  # output$pca_res <- DT::renderDataTable({
  #   data(iris)
  #   # log transform 
  #   log.ir <- log(iris[, 1:4])
  #   ir.species <- iris[, 5]
  #   
  #   # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
  #   ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
  #   print(ir.pca)
  #   #datatable(ir.pca)
  # })
  output$pca_res <- renderPrint({
    data(iris)
    # log transform 
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    print(ir.pca)
    #datatable(ir.pca)
  })
  output$summary <- renderPrint(summary(iris))
  output$cor_plot <- renderPlot({
    g <- ggcorrplot(cor(iris[, 1:4]), colors = c("darkred", "white", "steelblue"))
    print(g)
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
