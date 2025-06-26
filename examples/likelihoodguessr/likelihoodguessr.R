# Interactive Graphical Demonstrations for Survival Analysis

# Init ------------------------------------------------------------

library(shiny)
library(ggplot2)
library(patchwork)
library(flexsurv)
library(survival)
library(plotly)

# Data ------------------------------------------------------------

bc <-
  data.frame(
    positive = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    weeks = c(19, 25, 30, 
              34, 37, 46, 47, 51, 56, 57, 61, 66, 67, 74, 78, 86, 122, 123, 
              130, 130, 133, 134, 136, 141, 143, 148, 151, 152, 153, 154, 156, 
              162, 164, 165, 182, 189, 22, 23, 38, 42, 73, 77, 89, 115, 144),
    censored = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                 0, 0, 0, 0, 0, 0, 0, 0, 1)
  )

# Functions -------------------------------------------------------

GompertzHazard <- function (x, a, b) {
  a*exp(b*x)
}
GompertzSurvival <- function (x, a, b) {
  if (b != 0) {
    exp(-a/b*(exp(b*x)-1))  
  } else {
    exp(-a*x)
  }
}
GompertzQuantile <- function (p, a, b) {
  flexsurv::qgompertz(p, shape = b, rate = a)
}
GompertzDensity <- function (x, a, b) {
  flexsurv::dgompertz(x, shape = b, rate = a)
}
GompertzLogDensity <- function (x, a, b) {
  flexsurv::dgompertz(x, shape = b, rate = a, log = TRUE)
}
GompertzSample <- function (n, a, b) {
  flexsurv::rgompertz(n, shape = b, rate = a)
}

# UI --------------------------------------------------------------

# define user interface for plotting survival quantities
ui <- fluidPage(
  
      # Application title
    titlePanel("Likelihood maximization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          checkboxInput("showtrue", label = 'Show truth', value = TRUE),
          checkboxInput("showkm", label = 'Show Kaplan-Meier', value = FALSE),
          conditionalPanel(
            "input.showtrue == true",
            sliderInput("true_rate",
                        "Rate (x 100,000)",
                        min = 0,
                        max = 3000,
                        value = 1500),
            sliderInput("true_slope",
                        "Relative derivatve",
                        min = -0.05,
                        max = 0.05,
                        value = 0)
          ),
          actionButton('randomize', 'Randomize Parameters!', width = '100%'),
          actionButton('sample', 'Draw Sample!', width = '100%'),
          sliderInput("sample_size",
                      "Sample size",
                      min = 1,
                      max = 100,
                      value = 10),
          sliderInput("guess_rate",
                      "Guess rate",
                      min = 0,
                      max = 3000,
                      value = 1500),
          sliderInput("guess_b",
                      "Guess derivative",
                      min = -0.05,
                      max = 0.05,
                      value = 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            id = "tabset",
            tabPanel("Distribution", plotOutput("distPlot")),
            tabPanel("Likelihood contributions", tableOutput('table')),
            tabPanel("Likelihood surface", plotlyOutput('llsurface', width = "auto", height = "300px"))
          ),
           h5('Log-likelihood'),
           verbatimTextOutput('likelihood')
        )
    )
)

# Server ----------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  cnst <- list(
    rate_scale = 1e5,
    min_rate = 0,
    max_rate = 3000,
    min_b = -0.05,
    max_b = 0.05
  )
  
  v <- reactiveValues()

  observeEvent(input$randomize, {
    updateSliderInput(session, 'true_rate',
                      value = runif(1, cnst$min_rate, cnst$max_rate))
    updateSliderInput(session, 'true_slope',
                      value = runif(1, cnst$min_b, cnst$max_b))
  })
  
  observeEvent(input$sample, {
    v$samples <-
      GompertzSample(input$sample_size,
                     input$true_rate/cnst$rate_scale, input$true_slope)
    km <- survfit(Surv(v$samples)~1)
    v$km <- c(1, km[['surv']])
    v$x <- c(0, km[['time']])
  })
  
  params <- reactive({
    l <- list(
      true_a = input$true_rate/cnst$rate_scale,
      true_b = input$true_slope,
      guess_a = input$guess_rate/cnst$rate_scale,
      guess_b = input$guess_b
    )
    l$samples <- v$samples
    if (is.null(v$samples)) {
      l$samples <- as.numeric(NA)
    }
    l$loglikelihood <-
      GompertzLogDensity(
        l$samples, l$guess_a, l$guess_b
      )
    l$rate = seq(cnst$min_rate, cnst$max_rate, length.out = 100)/cnst$rate_scale
    l$b = seq(cnst$min_b, cnst$max_b, length.out = 100)
    l$loglikelihoodsurface <-
      matrix(NA, nrow = 100, ncol = 100)
    for (i in 1:100) {
      for (j in 1:100) {
        l$loglikelihoodsurface[i,j] <-
          sum(GompertzLogDensity(l$samples, l$rate[i], l$b[j]))
        l$loglikelihoodsurface[is.infinite(l$loglikelihoodsurface)] <- NA
      }
    }
    l
  })
  
  output$distPlot <- renderPlot({
    
    plot_fx <-
      ggplot() +
      list(
        if (input$showtrue) {
        geom_function(
          fun = GompertzDensity,
          args = list(a = params()$true_a, b = params()$true_b)
        )} else { NULL },
        geom_function(
          fun = GompertzDensity,
          args = list(a = params()$guess_a, b = params()$guess_b),
          color = 'blue', lty = 2
        ),
        annotate('point', x = params()$samples, y = 0.001),
        scale_x_continuous(limits = c(0, 100), expand = c(0,0)),
        scale_y_continuous(limits = c(0, 0.03), expand = c(0,0)),
        labs(x = 'Time x', subtitle = 'Probability density f(x)',
             y = ''),
        theme_classic(),
        coord_cartesian(clip = 'off')
      )
    
    plot_hx <-
      ggplot() +
      list(
        if (input$showtrue) {
          geom_function(
            fun = GompertzHazard,
            args = list(a = params()$true_a, b = params()$true_b)
          )} else { NULL },
        geom_function(
          fun = GompertzHazard,
          args = list(a = params()$guess_a, b = params()$guess_b),
          color = 'blue', lty = 2
        ),
        scale_x_continuous(limits = c(0, 100),
                           expand = c(0,0)),
        scale_y_continuous(limits = c(0, 0.1), expand = c(0,0),
                           labels = scales::label_number(scale = 1e5)),
        labs(
          x = 'Time x', y = '',
          subtitle = 'Hazard rate h(x) (x 100,000)'
        ),
        theme_classic(),
        coord_cartesian(clip = 'off')
      )
    
    plot_sx <-
      ggplot() +
      list(
        if (input$showtrue) {
          geom_function(
            fun = GompertzSurvival,
            args = list(a = params()$true_a, b = params()$true_b)
          )} else { NULL },
        if (input$showkm) {
          geom_step(
            aes(x = v$x, y = v$km)
          )} else { NULL },
        geom_function(
          fun = GompertzSurvival,
          args = list(a = params()$guess_a, b = params()$guess_b),
          color = 'blue', lty = 2
        ),
        scale_x_continuous(limits = c(0, 100),
                           expand = c(0,0)),
        scale_y_continuous(limits = c(0, 1), expand = c(0,0)),
        labs(x = 'Time x',
             subtitle = 'Probability of Survival S(x)',
             y = ''),
        theme_classic(),
        coord_cartesian(clip = 'off')
      )
    
    plot_hx + plot_fx + plot_sx +
    patchwork::plot_layout(nrow = 3)
    
  })
  
  output$likelihood <- renderPrint({
    
    loglikelihood <- sum(params()$loglikelihood)
    loglikelihood
    
  })
  
  output$table <- renderTable({
    
    data.frame(
      `Observation x` = params()$samples,
      `Log-likelihood contribution` = params()$loglikelihood
    )
    
  })
  
  # output$llsurface <- renderRglwidget({
  #   
  #   try(close3d())
  #   persp3d(
  #     x = params()$loglikelihoodsurface,
  #     zlim = c(-1000, range(params()$loglikelihoodsurface, na.rm = TRUE)[2]),
  #     col = 'lightblue', box = FALSE
  #   )
  # 
  #   rglwidget()
  #   
  # })
  
  output$llsurface <- renderPlotly({
    rate <- params()$rate*1e5
    b <- params()$b
    ll <- params()$loglikelihoodsurface
    plot_ly(
      x = ~rate,
      y = ~b,
      z = ~ll,
      type = "surface",
      showscale = FALSE
    ) %>%
      layout(
        scene = list(
          zaxis = list(
            title = "LogLike",
            range = range(ll, na.rm = TRUE)),
          xaxis = list(title = "rate"),
          yaxis = list(title = "b")
        )
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
