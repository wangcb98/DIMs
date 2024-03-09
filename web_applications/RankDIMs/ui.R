library(shiny)
## Define a UI app that ranks the DIMs at a specific time stamp for specific stakeholder group(s)
ui <- fluidPage(
  titlePanel(strong(em("RankDIMs"))),
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
  h4(helpText('Provides rankings, $S_{DIM_{i}}$ values and importance ratios $I_{pos}$ and $I_{tot}$ for a customisable number ($n_{cus}=n_{sub}$) of DIMs at all spatial scales and a prescribed temporal resolution')),
  # sidebarLayout(
    sidebarPanel(
      selectInput('t', helpText('Prescribed temporal instance'), c("two weeks", "six months", "over six months")),
      numericInput(inputId = "wts1",
                   label = helpText("Weight assigned to residents ($w_1$)"),
                   min = 0,
                   max = 1,
                   value = 0.5),
      numericInput(inputId = "wts2",
                   label = helpText("Weight assigned to other stakeholders ($w_2$)"),
                   min = 0,
                   max = 1,
                   value = 0.5),
      numericInput(inputId = "num",
                   label = helpText('The number of DIMs to consider ($n_{cus}=n_{sub}$)'),
                   min = 10,
                   max = 60,
                   value = 25),
      selectInput(inputId = 'A', helpText('Specify a disaster impact to provide ranking information and $S_{DIM_{i}}$ values for, across all spatial scales'),labels_DIMs_region, selected="Access to drinking water is lost"),
      actionButton("button", "Generate new results"),
      width = 12
      ),
    mainPanel(
      # Outputs
      textOutput(outputId = "result.A"),
      h4("A plot showing the rankings of household-level DIMs:"),
      plotOutput(outputId = "plot.1",height = "1000px"),
      h4("A plot showing the rankings of neighbourhood-level DIMs:"),
      plotOutput(outputId = "plot.2",height = "1000px"),
      h4("A plot showing the rankings of region-level DIMs:"),
      plotOutput(outputId = "plot.3",height = "1000px"),
      helpText(h4("The positive importance ratio ($I_{pos}$) and total importance ratio ($I_{tot}$) captured by the list of top $n_{cus}=n_{sub}$ DIMs at different spatial scales:")),
      tableOutput(outputId = "ratio_importance"),
      h4('Average weighted importance scores associated with top-ranked household-level DIMs:'),
      tableOutput(outputId = "df.hh"),
      h4('Average weighted importance scores associated with top-ranked neighbourhood-level DIMs:'),
      tableOutput(outputId = "df.neighbour"),
      h4('Average weighted importance scores associated with top-ranked region-level DIMs:'),
      tableOutput(outputId = "df.region"),
      h4('Importance scores given by each participant on household-level DIMs:'),
      tableOutput(outputId = "tbl_hh"),
      h4('Importance scores given by each participant on neighbourhood-level DIMs:'),
      tableOutput(outputId = "tbl_neighbour"),
      h4('Importance scores given by each participant on region-level DIMs:'),
      tableOutput(outputId = "tbl_region"),
      width = 12
    ),
    # position = "left"
  # )
)