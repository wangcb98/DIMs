library(shiny)
# Define a UI app that performs Welch ANOVA, ANOVA, Welch t-test across spatial (Household, Neighbourhood, and Region) questions 
# and for each resolution between residents and beneficiaries
# and shows the bar plots for each spatial resolutionload("clean_Data.RData")
ui <- fluidPage(
  #figure out how to add an submit button
  titlePanel(strong(em("CompareDIMs"))),
  tags$head(
    tags$link(rel="stylesheet", 
              href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css", 
              integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
              crossorigin="anonymous"),
    HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
    HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
    HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
  ),
  h4(helpText('Performs Welch’s Analysis of Variance, Welch’s two-sample t-test, and power analyses on the $S_{DIM_{i}}$ values across different spatial scales, temporal instances, and stakeholder categories')),
  sidebarPanel(
    h4(helpText(strong(em('Spatial comparison')))),
    p(helpText('Compare $S_{DIM_{i}}$ values across different spatial scales, considering a specific disaster impact, a prescribed temporal instance, and a set of stakeholder categories')),
    selectInput('DI_s', 'Disaster impact of interest',c(labels_DIMs_spatial,labels_DIMs_all), selected ="Access to community assets is lost"),
    selectInput('t_s', 'Prescribed temporal instance',c("two weeks", "six months", "over six months","not applicable")),
    selectInput('which_group_s', 'Stakeholder category',c("Resident","Other stakeholders","Both")),
    ),
  sidebarPanel(
    h4(helpText(strong(em('Temporal comparison')))),
    p(helpText('Compare $S_{DIM_{i}}$ values across different temporal instances, considering a specific disaster impact, a prescribed spatial scale, and a set of stakeholder categories')),
    selectInput('DI_t', 'Disaster impact of interest',labels_DIMs_temporal, selected = "Access to drinking water is lost"
    ),
    selectInput('space_t', 'Prescribed spatial scale',
                c("Household","Neighbourhood","Region")),
    selectInput('which_group_t', 'Stakeholder category',c("Resident","Other stakeholders","Both")),
  ),
  sidebarPanel(
    h4(helpText(strong(em('Stakeholder category comparison')))),
    p(helpText('Compare $S_{DIM_{i}}$ values across different stakeholder categories for a specific DIM')),
    selectInput('DI_g', 'Disaster impact of interest',c(labels_DIMs_spatial,labels_DIMs_all), selected = "Access to required healthcare is lost"
    ),
    selectInput('space_g', 'Prescribed spatial scale',
                c("Household","Neighbourhood","Region")),
    selectInput('t_g', 'Prescribed temporal instance',c("two weeks", "six months", "over six months","not applicable")),
  ),
  sidebarPanel(
    actionButton("button", "Generate new results")
  ),
  mainPanel(
    # results for spatial comparisons
    h4("Spatial comparison (Welch's Analysis of Variance and two-sample t-test and power analyses)"),
    tableOutput(outputId = "Result.s.welch.anova"),
    tableOutput(outputId = "Result.s"),
    h4("Temporal comparison (Welch's two-sample t-test and power analyses)"),
    tableOutput(outputId = "Result.t"),
    h4("Stakeholder category comparison (Welch's two-sample t-test and power analyses)"),
    tableOutput(outputId = "Result.g")
  ),
  width = 3
)