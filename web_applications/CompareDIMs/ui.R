library(shiny)
# Define a UI app that performs Welch ANOVA, ANOVA, Welch t-test across spatial (Household, Neighbourhood, and Region) questions 
# and for each resolution between residents and beneficiaries
# and shows the bar plots for each spatial resolutionload("clean_Data.RData")
ui <- fluidPage(
  #figure out how to add an submit button
  titlePanel(strong(em("CompareDIMs"))),
  h5('Performs hypothesis tests on the average importance score given by different stakeholder groups on three spatial resolution (Household, Neighbourhood, and Region) and compare the importance score associated with any two entities (stakeholder group, space, and time).'),
  sidebarPanel(
    actionButton("button", "Click me to generate new results")
  ),
  sidebarPanel(
    p('Select a disaster impact to compare the importance scores associated with different spatial scale:'),
    selectInput('DI', 'Which disaster impact to investigate?',c(labels_DIMs_spatial,labels_DIMs_all)),
    selectInput('which_group', 'Which stakeholder group(s) to investigate?',c("Resident","Beneficiary","Both")),
    selectInput('t', 'Which time instance is of interest?',c("2 weeks", "6 months", "over 6 months"))
    ),
  sidebarPanel(
    p('Select two disaster impacts and compare the importance scores associated with different temporal instances and a specified spatial scale:'),
    selectInput('A', 'Disaster impact #1',labels_DIMs_region, selected = "Access to clean air is lost"
    ),
    selectInput('space_A', 'spatial scale of analysis',
                c("Household","Neighbourhood","Region")
    ),
    selectInput('B', 'Disaster impact #2',labels_DIMs_region, selected = "Access to water is lost"
    ),
    selectInput('space_B', 'spatial scale of analysis',
                c("Household","Neighbourhood","Region")
    ),
    sliderInput(inputId = "how_jitter",
                label = "How jittered do you want the dots in scatter plots to be",
                min = 0,
                max = 1,
                value = 0.2)
  ),
  mainPanel(
    # results for spatial comparisons
    plotOutput(outputId = "plot.DI"),
    tableOutput(outputId = "anova.result.DI"),
    tableOutput(outputId = "welch.anova.result.DI"),
    tableOutput(outputId = "tb.t.test.DI"),
    tableOutput(outputId = "pwr.DI"),
    tableOutput(outputId = "pwr.inv.DI"),
    tableOutput(outputId = "df.plot.DI"),
    # results for two specific DIMs
    plotOutput(outputId = "plot1"),
    plotOutput(outputId = "plot2"),
    plotOutput(outputId = "plot3"),
    plotOutput(outputId = "plot4"),
    tableOutput(outputId = "tb.t.test"),
    tableOutput(outputId = "tb.pwr"),
    tableOutput(outputId = "tb.t.test.temporal"),
    tableOutput(outputId = "tb.pwr.temporal"),
    tableOutput(outputId = "df.plot")
  ),
  width = 3
)