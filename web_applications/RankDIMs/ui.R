library(shiny)
## Define a UI app that ranks the DIMs at a specific time stamp for specific stakeholder group(s)
ui <- fluidPage(
  titlePanel(strong(em("RankDIMs"))),
  # sidebarLayout(
    sidebarPanel(
      h5('Ranks the disaster impacts associated with different spatial scales and a specific temporal instance for specific stakeholder group(s)'),
      actionButton("button", "Generate new results"),
      selectInput('t', 'Which temporal instance is of interest?',c("2 weeks", "6 months", "over 6 months")),
      selectInput('which_group',
                  'Which stakeholder group(s) to investigate?',
                  c("Resident","Other stakeholders","Both")),
      numericInput(inputId = "wts1",
                   label = "Weight assigned to residents",
                   min = 0,
                   max = 1,
                   value = 0.5),
      numericInput(inputId = "wts2",
                   label = "Weight assigned to other stakeholders",
                   min = 0,
                   max = 1,
                   value = 0.5),
      numericInput(inputId = "num",
                   label = "How many disaster impacts to show in the top lists?",
                   min = 10,
                   max = 60,
                   value = 20),
      selectInput(inputId = 'A', 'Disaster impact to show ranking and score for: ',labels_DIMs_region, selected="Access to drinking water is lost"),
      width = 12
      ),
    mainPanel(
      # Outputs
      textOutput(outputId = "result.A"),
      plotOutput(outputId = "plot.1",height = "1000px"),
      plotOutput(outputId = "plot.2",height = "1000px"),
      plotOutput(outputId = "plot.3",height = "1000px"),
      tableOutput(outputId = "ratio_importance"),
      tableOutput(outputId = "df.hh"),
      tableOutput(outputId = "df.neighbour"),
      tableOutput(outputId = "df.region"),
      tableOutput(outputId = "tbl_hh"),
      tableOutput(outputId = "tbl_neighbour"),
      tableOutput(outputId = "tbl_region"),
      width = 12
    ),
    # position = "left"
  # )
)