source("global.R")

ui <- dashboardPage(
  dashboardHeader(title = "Cronbach's Alpha Reliability Testing"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Import", tabName = "import", icon = icon("upload")),
      menuItem("Column Selection", tabName = "selection", icon = icon("check-square")),
      menuItem("Objective Grouping", tabName = "grouping", icon = icon("layer-group")),
      menuItem("Likert Scale Setup", tabName = "likert", icon = icon("star")),
      menuItem("Reliability Analysis", tabName = "analysis", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      importUI("import"),
      selectionUI("selection"),
      groupingUI("grouping"),
      likertUI("likert"),
      analysisUI("analysis")
    )
  )
)

server <- function(input, output, session) {
  importServer("import", values)
  selectionServer("selection", values)
  groupingServer("grouping", values)
  likertServer("likert", values)
  analysisServer("analysis", values)
}

shinyApp(ui = ui, server = server)
