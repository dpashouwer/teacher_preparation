library(shiny)
library(tidyverse)

# Load data -------------------
by_state_program_completers <- readRDS(here::here("data/clean/by_state_program_completers.rds"))

# Functions -------------------
plot_total_completers_by_year <- function(dat){
    p <- dat %>% 
        dplyr::filter(ihe_name == "TOTAL") %>% 
        dplyr::group_by(year) %>% 
        dplyr::mutate(n_completers = sum(as.numeric(n_completers))) %>% 
        ggplot2::ggplot(aes(year, n_completers, group = 1)) + 
        ggplot2::geom_path() + 
        ggplot2::geom_text(aes(label = n_completers)) + 
        expand_limits(y = 0) +
        labs(x = "School Year", 
             y = "Number of Completers", 
             title = paste0("Number of Completers by Year")) + 
        ggplot2::theme_minimal()
    p
}

ui <- fluidPage(
    selectInput(inputId = "states", label = "States", choices = unique(by_state_program_completers$state)),
    plotOutput(outputId = "line")
)

server <- function(input, output) {
    
    output$line <- renderPlot({
        by_state_program_completers %>% 
            filter(state == input$states) %>% 
            pull(data) %>% 
            as.data.frame() %>% 
            plot_total_completers_by_year()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
