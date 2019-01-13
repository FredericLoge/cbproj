# File Content:
#
# This is a Shiny web application, allowing you to run simulations and plot 
# observed regret and expected regret

# constant vector: enumerating scenario generators
CONTEXT_GEN_VALUE <- c('K Arm Bandit', 'Independent Truncated Gaussians')

# load functions
source(file = 'foo.R')

# library required
library(shiny)
library(shinydashboard)
library(ggplot2)
library(gridExtra)

# constants for some UI widget dimensions
WD <- '100px'
WD_BETA <- '200px'

# define UI for application that draws a histogram
ui <- dashboardPage(
  
  title = "Greedy against UCB",
  
  # Application title
  header = dashboardHeader(title = "ExploitASAP", disable = FALSE, titleWidth = '30%'),
  
  ## withMathJax(),
  
  # Sidebar with a slider input for number of bins 
  sidebar = dashboardSidebar(width = '30%',
                             withMathJax(),
                             tags$head(
                               tags$style(type="text/css", 
                                          "#inline label{ display: table-cell; text-align: right; vertical-align: middle; padding-left:10px ; padding-right:10px;}
                                                     #inline .form-group { display: table-row; padding-bottom:30px }")
                             ),
                             br(),
                             box(title = "Problem parameters",
                                 tags$div(id = "inline", numericInput(inputId = "par_nb_arms", label = '\\(K\\)', min = 2, max = 50, value = 3, width = WD)),
                                 br(),
                                 tags$div(id = "inline", numericInput(inputId = "par_nb_rounds", label = '\\(T\\)', min = 100, max = 1000, value = 200, step = 100, width = WD)),
                                 br(),
                                 tags$div(id = "inline", numericInput(inputId = "par_sigma", label = '\\(\\sigma\\)', min = 0, max = 100, value = 10, step = 1, width = WD)),
                                 br(),
                                 tags$div(id = "inline", numericInput(inputId = "par_dim_beta", label = '\\(d\\)', min = 1, max = 10, value = 3, width = WD)),
                                 br(),
                                 tags$div(id = "inline", textInput(inputId = "par_beta", label = '\\(\\beta\\)', width = WD_BETA)),
                                 # br(),
                                 actionButton(inputId = 'rand_beta', label = 'Randomize \\(\\beta\\)'),
                                 # br(),
                                 radioButtons(inputId = "par_context_gen", label = "Context scenario:", choices = CONTEXT_GEN_VALUE),
                                 background = 'blue'
                             ),
                             box(title = "UCB parameter",
                                 tags$div(id = "inline", numericInput(inputId = "par_alpha", label = '\\(\\alpha\\)', min = 0, max = 50, value = 7, width = WD)),
                                 background = 'blue'
                             ),
                             box(title = "Simulation parameters",
                                 tags$div(id = "inline", numericInput(inputId = "par_seed", label = "\\(Seed\\)", min = 1, max = 50, value = 30, width = WD)),
                                 br(),
                                 tags$div(id = "inline", numericInput(inputId = "nb_exp", label = '\\(N\\)', min = 1, max = 50, value = 10)),
                                 background = 'blue'
                             ),
                             helpText('Reminder on the parameters above: K, number of arms; T, the number of rounds; \\(\\sigma^2\\) the reward variance; \\(\\beta\\) 
                                      the true vector to learn. Note that you can randomize it or directly change it within the text input. The Greedy strategy is played 
                                      following ExploitASAP and the UCB strategy following LinUCB, taking \\(\\lambda\\) = 1 for the ridge estimator.'),
                             fluidRow(),
                             actionButton(inputId = 'launch_exp', label = 'GO'),
                             column(width = 12, verbatimTextOutput(outputId = 'text_result')),
                             br()
  ),
  
  body = dashboardBody(
    background = 'white',
    tags$head(tags$style(HTML('
    /* body */
                              .content-wrapper, .right-side {
                              background-color: #ffffff;
                              }
                              '))),
    h1('Simulation results.', align = 'center'),
    br(), br(),
    plotOutput(outputId = "sim_run_plot"), # , height = '1000px', width = '700px'),
    verbatimTextOutput(outputId = "sim_run_text")
  ), 
  
  skin = 'blue'
  
)

# define server logic 
server <- function(input, output, session) {
  
  ### randomize beta
  observeEvent(input$rand_beta, {
    # simulate beta at random
    b <- runif(n = input$par_dim_beta, min = -5, max = +5)
    b <- round(b, digits = 1)
    bt <- paste0('c(', paste0(b, collapse = ','), ')')
    
    # update beta parameter
    updateTextInput(session = session, inputId = 'par_beta', value = bt)
  })
  
  ### plot ob ubar{lambda}
  output$null_plot <- renderPlot({
    ggplot(data = NULL) + geom_point(x = 1, y = 1)
  })
  
  ### generating beta at random in text input (allowing the user to change it afterwards)
  observe({
    
    # check on input for d
    d <- input$par_dim_beta
    d_is_numeric <- is.numeric(d)
    
    # if check is passed, simulate new vector beta and update 'par_beta'
    if( d_is_numeric ){
      
      # simulate beta at random
      b <- runif(n = d, min = -5, max = +5)
      b <- round(b, digits = 1)
      bt <- paste0('c(', paste0(b, collapse = ','), ')')
      
      # update beta parameter
      updateTextInput(session = session, inputId = 'par_beta', value = bt)
      
    }
    
  })
  
  ### set of values to store in memory
  rea <- reactiveValues(game_config = list(), sim_result = -1, startedSim = FALSE)
  
  ### launch simulations
  observeEvent(input$launch_exp, {
    
    # update boolean
    rea$startedSim <- TRUE
    
    # extract game configuration
    gc <- list(
      generic_seed = input$par_seed, 
      game_duration = input$par_nb_rounds, 
      K = input$par_nb_arms, 
      d = input$par_dim_beta, 
      beta = eval(parse(text = input$par_beta)),
      set_s2 = rep(input$par_sigma, input$par_nb_arms),
      CONTEXT_GEN_VALUE = input$par_context_gen,
      alpha = input$par_alpha
    )
    
    # checkpoints on game config
    checks <- (gc$d == length(gc$beta)) &
      (length(gc$set_s2) == gc$K) &
      ifelse(gc$CONTEXT_GEN_VALUE == CONTEXT_GEN_VALUE[1], gc$d == gc$K, TRUE) &
      (input$nb_exp >= 1)
    
    # simulate if checks are passed, otherwise print message in 'text_result' element
    if( checks == TRUE ){
      rea$finished <- FALSE
      rea$checks <- TRUE
      rea$sim_result <- simulate_games(n_games = input$nb_exp, game_config = gc)
      rea$finished <- TRUE
    }else{
      rea$checks <- FALSE
    }
    
  })
  
  ### indicate if checks were not passed and when simulations are finished
  output$text_result <- renderText({ 
    input$launch_exp
    if( rea$startedSim ){
      if ( rea$checks ){
        m <- 'Checks passed. Running ...'
        if( rea$finished ){
          m <- 'Simulations done.'
        }
      }else{
        m <- 'Checks not passed! Check input.'
      }
    }else{
      m <- '-'
    }
    return(m)
  })
  
  ### plot observed regret and estimated expected regret
  output$sim_run_plot <- renderPlot({
    
    if( rea$startedSim == FALSE ){
      
      plot(x = 1, col = 'white', axes = FALSE, xlab = '', ylab = '')
      legend(title = 'Pick input values \n and click on GO button.', 'center', legend = '', box.col = 'white', cex = 3)      
      
    }else{
      
      # extract simulation results
      tt <- rea$sim_result
      
      # ggplot 1: regret
      gr <- factor(paste0(tt$id, tt$strategy))
      gg1 <- ggplot(data = tt) +
        geom_line(mapping = aes(x = time, y = regret, colour = factor(strategy), group = gr)) +
        labs(colour = 'strategy', title = 'Observed cumulative regret') + ylab(label = '')
      
      # ggplot 2: expected regret
      tt_agg <- aggregate(x = tt$regret, by = tt[,c('time', 'strategy')], mean, na.rm = FALSE)
      colnames(tt_agg)[3] <- 'regret'
      gg2 <- ggplot(data = tt_agg) +
        geom_line(mapping = aes(x = time, y = regret, colour = factor(strategy))) +
        labs(colour = 'strategy', title = 'Empirical expected cumulative regret') + ylab(label = '')
      
      # plot both ggplots
      p <- grid.arrange(gg1, gg2, ncol = 2)
      p
      
    }
    
  })
  
}

# set seed for initial draws
set.seed(0)

# run the application 
shinyApp(ui = ui, server = server)

