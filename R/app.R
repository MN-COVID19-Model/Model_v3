library(shiny)
library(shinycssloaders)

# Set up the loading gif
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

## Load necessary packages, source files, and load data ##
source("setup_model.R")

# Define UI ----
ui <- fluidPage(
  titlePanel("COVID-19 Model Sandbox"),
  
  sidebarLayout(
    sidebarPanel(
      dateInput("shelter_in_place_end_date", 
                h3("End Date for Shelter-in-Place (Stay at Home Order"), 
                format = "MM d, yyyy",
                value = "2020-05-19"),
      dateInput("social_distancing_end_date", 
                h3("End Date for Social Distancing"), 
                value = "2020-08-01"),
      actionButton("simulationButton", "Run Simulation")
      ),
    mainPanel(      
      tabsetPanel(
        tabPanel("ICU Bed Demand", withSpinner(plotOutput("icu_bed_demand_plot"), type = 8)), 
        tabPanel("Cumulative Deaths", plotOutput("cumulative_deaths_plot")), 
        tabPanel("Prevalent Infections", plotOutput("prevalent_infections_plot")),
        tabPanel("Cumulative Infections", plotOutput("cumulative_infections_plot")),
        tabPanel("Daily Deaths", plotOutput("daily_deaths_plot")),
        tabPanel("Prevalent Hospitalizations", plotOutput("prevalent_hospitalizations_plot"))
    ),
      tableOutput("simulations_table"))
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  baseline_date_as_integer = 18343 # set to march 22 2020, per model
  
  session$userData$params = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("simulation_number", "sip_end_date", "social_distancing_end_date"))
  session$userData$results = matrix(nrow = length(scn_vec), ncol = 8)
  colnames(session$userData$results) <- c("simulation_number", 
                             "n_deaths", 
                             "n_deaths_may30",
                             "day_icu_cap_reached",
                             "max_icu_demand",
                             "Rt_est",
                             "day_peak_infections",
                             "additional_vulnerable_sd_days")
  session$userData$lst_out_raw <- list()
  session$userData$lst_out <- list()

  runModel <- reactive({
    input$simulationButton
    
    # Intializing lists to store the raw model output and processed model output
    
    # Initializing a matrix to store printed summary output from model
    summary_out <- matrix(nrow = length(scn_vec), ncol = 8)
    colnames(summary_out) <- c("strategy", 
                               "n_deaths", 
                               "n_deaths_may30",
                               "day_icu_cap_reached",
                               "max_icu_demand",
                               "Rt_est",
                               "day_peak_infections",
                               "additional_vulnerable_sd_days")
    
    # Set parameters to their default base case values 
    parms <- parameters(n_icu_beds = )
    
    # Set parameters
    parms$start_time_social_distancing <- 1 # March 23nd
    parms$end_time_social_distancing <- as.integer(isolate(input$social_distancing_end_date) - baseline_date_as_integer)
    parms$start_time_sip <- 6 # March 27th
    parms$end_time_sip <- as.integer(isolate(input$shelter_in_place_end_date) - baseline_date_as_integer)
    
    # Save parameters to simulations table
    session$userData$params[input$simulationButton,] <- list(as.character(input$simulationButton), 
                                                          isolate(format(as.Date(input$shelter_in_place_end_date),format="%m/%d/%Y")),
                                                          isolate(format(as.Date(input$social_distancing_end_date), format="%m/%d/%Y")))
    
    # Run model
    m_out_raw <- solve_model(parms$init_vec, 
                             times = times,
                             func = covid_19_model_function,
                             parms = parms)
    
    # Store the raw output from each strategy in a list
    session$userData$lst_out_raw <- c(session$userData$lst_out_raw, list(m_out_raw))
    
    # Process output matrix "out" to extract more data
    out <- process_output(m_out_raw,parms)
    session$userData$lst_out <- c(session$userData$lst_out,list(out))
    
    
    ## Store select summary outputs ##
    
    # deaths
    n_deaths <- round(out[nrow(out), "cumulative_deaths"], 0)
    pct_deaths <- round(100 * n_deaths / parms$N, 2)
    n_deaths_may30 <- round(out[70,"cumulative_deaths"],0) # day 70 is may 30
    
    # healthcare demand
    day_icu_cap_reached <- which(out[, "ICU_bed_demand"] >= parms$n_icu_beds)[1]
    max_icu_demand <- round(max(out[, "ICU_bed_demand"]), 0)
    
    # infections
    day_peak_infections <- which.max(out[, "prevalent_infections"])
    
    # Rt estimation (first 20 days)
    lm_Rt <- lm(log(out[1:20,"cumulative_infections"])~out[1:20,"Time"])
    avg_exp_dur <- parms$n_exposed_states/(parms$exposed_transition_rate/parms$timestep)
    avg_inf_dur <- parms$n_infected_states/(parms$infected_transition_rate/parms$timestep)
    Rt_est <- round((1+lm_Rt$coefficients[[2]]*avg_inf_dur)*(1+lm_Rt$coefficients[[2]]*avg_exp_dur),2)
    
    # Scenario features
    # number of additional days distancing for vulnerable (aged 60+)
    n_add_vulnerable_sd_days <- ifelse(parms$sixty_plus_days_past_peak >= 0,
                                       round(max(which(m_out_raw[, "sd60p"] == 1)) - 
                                               max(which(m_out_raw[, "sd"] == 1 | m_out_raw[, "sip"] == 1)),0),
                                       NA)
    
    # Store summary results in matrix
    session$userData$results[input$simulationButton,] <- c(i_scenario, n_deaths, n_deaths_may30, 
                         day_icu_cap_reached, max_icu_demand, 
                         Rt_est, day_peak_infections, 
                         n_add_vulnerable_sd_days)
    
    ## Create data.frame for plotting
    df_ls <- lapply(1:length(session$userData$lst_out), FUN = function(x) {
      df <- as.data.frame(session$userData$lst_out[[x]][, 1:6])
      df$t <- 1:nrow(session$userData$lst_out[[x]])
      df$strategy <- scn_vec[x]
      return(df)
    })
    df_out <- do.call(rbind, df_ls)
  })

  output$icu_bed_demand_plot <- renderPlot({
    ## Function for basic plots of model outputs over time
    plot_func <- function(var_name) {
      var <- sym(var_name)
      plot <- ggplot(runModel(), aes(x = t / 7, y = !! var, color = strategy)) + geom_path() +
        ylab(var_name) + xlab("Time (weeks)") +
        ggtitle(paste0(var_name)) +
        theme(plot.title = element_text(hjust = 0.5))
      return(plot)
    }
    
    ## Generate plots
    plot_func("ICU_bed_demand")
  })
  
  output$cumulative_deaths_plot <- renderPlot({
    ## Function for basic plots of model outputs over time
    plot_func <- function(var_name) {
      var <- sym(var_name)
      plot <- ggplot(runModel(), aes(x = t / 7, y = !! var, color = strategy)) + geom_path() +
        ylab(var_name) + xlab("Time (weeks)") +
        ggtitle(paste0(var_name)) +
        theme(plot.title = element_text(hjust = 0.5))
      return(plot)
    }
    
    ## Generate plots
    plot_func("cumulative_deaths")
  })
  
  output$prevalent_infections_plot <- renderPlot({
     ## Function for basic plots of model outputs over time
    plot_func <- function(var_name) {
      var <- sym(var_name)
      plot <- ggplot(runModel(), aes(x = t / 7, y = !! var, color = strategy)) + geom_path() +
        ylab(var_name) + xlab("Time (weeks)") +
        ggtitle(paste0(var_name)) +
        theme(plot.title = element_text(hjust = 0.5))
      return(plot)
    }
    
    ## Generate plots
    plot_func("prevalent_infections")
  })
  
  output$cumulative_infections_plot <- renderPlot({
    ## Function for basic plots of model outputs over time
    plot_func <- function(var_name) {
      var <- sym(var_name)
      plot <- ggplot(runModel(), aes(x = t / 7, y = !! var, color = strategy)) + geom_path() +
        ylab(var_name) + xlab("Time (weeks)") +
        ggtitle(paste0(var_name)) +
        theme(plot.title = element_text(hjust = 0.5))
      return(plot)
    }
    
    ## Generate plots
    plot_func("cumulative_infections")
  })
  
  
  output$daily_deaths_plot <- renderPlot({
    ## Function for basic plots of model outputs over time
    plot_func <- function(var_name) {
      var <- sym(var_name)
      plot <- ggplot(runModel(), aes(x = t / 7, y = !! var, color = strategy)) + geom_path() +
        ylab(var_name) + xlab("Time (weeks)") +
        ggtitle(paste0(var_name)) +
        theme(plot.title = element_text(hjust = 0.5))
      return(plot)
    }
    
    ## Generate plots
    plot_func("daily_deaths")
  })
  
  
  output$prevalent_hospitalizations_plot <- renderPlot({
    ## Function for basic plots of model outputs over time
    plot_func <- function(var_name) {
      var <- sym(var_name)
      plot <- ggplot(runModel(), aes(x = t / 7, y = !! var, color = strategy)) + geom_path() +
        ylab(var_name) + xlab("Time (weeks)") +
        ggtitle(paste0(var_name)) +
        theme(plot.title = element_text(hjust = 0.5))
      return(plot)
    }
    
    ## Generate plots
    plot_func("prevalent_hospitalizations")
  })
  
  output$simulations_table <- renderTable({
    input$simulationButton
    session$userData$params
    },
    display = c('s','d','s','s'))

}

# Run the app ----
shinyApp(ui = ui, server = server)