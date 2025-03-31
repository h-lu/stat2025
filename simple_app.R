# simple_app.R - 最简单的Shiny应用

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "简单Shiny示例"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("主页", tabName = "home", icon = icon("home"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(title = "简单输出",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    textOutput("simple_text")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  output$simple_text <- renderText({
    "这是一个简单的Shiny应用！"
  })
}

shinyApp(ui = ui, server = server) 