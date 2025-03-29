library(shiny)
library(ggplot2)
library(datasauRus)
library(DT)

ui <- fluidPage(
  titlePanel("Datasaurus 資料集視覺化"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "選擇資料集:",
                  choices = unique(datasaurus_dozen$dataset)),
      checkboxInput("show_ground_truth_table", "顯示真實資料 (dino) 表格", value = FALSE),
      verbatimTextOutput("stats"),
      p("說明：相同的平均值和標準差可以產生非常不同的散佈圖。") # 加入說明文字
    ),
    mainPanel(
      plotOutput("datasaurusPlot"),
      conditionalPanel(
        condition = "input.show_ground_truth_table == true",
        h3("真實資料 (dino)"),
        DTOutput("ground_truth_table")
      )
    )
  )
)

server <- function(input, output) {
  selected_data <- reactive({
    datasaurus_dozen[datasaurus_dozen$dataset == input$dataset, ]
  })

  output$datasaurusPlot <- renderPlot({
    ggplot(selected_data(), aes(x, y)) +
      geom_point() +
      labs(title = input$dataset)
  })

  output$ground_truth_table <- renderDT({
    if (input$show_ground_truth_table) {
      datasaurus_dozen[datasaurus_dozen$dataset == "dino", ]
    }
  })

  output$stats <- renderPrint({
    data <- selected_data()
    x_mean <- mean(data$x)
    y_mean <- mean(data$y)
    x_sd <- sd(data$x)
    y_sd <- sd(data$y)

    cat("平均值 (x):", x_mean, "\n")
    cat("平均值 (y):", y_mean, "\n")
    cat("標準差 (x):", x_sd, "\n")
    cat("標準差 (y):", y_sd, "\n")
  })
}

shinyApp(ui = ui, server = server)
