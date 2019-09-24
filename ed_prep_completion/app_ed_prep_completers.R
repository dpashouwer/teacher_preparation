library(shiny)
library(tidyverse)
library(janitor)
library(tntpr)

# Load data -------------------
by_state_program_completers <- readRDS(here::here("data/clean/by_state_program_completers.rds"))

# Functions ------------------------
plot_total_completers <- function(dat, state){
    dat %>% 
        filter(state == !!state) %>% 
        pull(data) %>% 
        as.data.frame() %>% 
        filter(ihe_name == "TOTAL") %>% 
        group_by(year) %>% 
        summarise(n_completers = sum(as.numeric(n_completers))) %>% 
        ggplot(aes(year, n_completers, group = 1)) + 
            geom_path() + 
            geom_text(aes(label = n_completers))
}

plot_total_completers_by_type <- function(dat, state){
    dat %>% 
        filter(state == !!state) %>% 
        pull(data) %>% 
        as.data.frame() %>% 
        filter(program_type != "TOTAL") %>% 
        group_by(year, program_type) %>% 
        summarise(n_completers = sum(as.numeric(n_completers))) %>% 
        ggplot(aes(year, n_completers, color = program_type)) + 
            geom_path(aes(group = program_type)) + 
            geom_text(aes(label = n_completers))
}

plot_formatting <- function(plot){
    plot + 
        theme_tntp_2018() + 
        scale_y_continuous(limits=c(0,NA)) + 
        labs(x = "Academic Year", y = "Total Completers", 
             color = NULL) +
        scale_color_tntp()
}

plot_completers <- function(dat, state, type = "Total"){
    if(type == "Total"){
        plot_total_completers(dat, state = state) %>% 
            plot_formatting()
    } else if(type == "By Program Type"){
        plot_total_completers_by_type(dat, state = state) %>% 
            plot_formatting()
    } 
}

# App ------------------------

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "state", label = "States", choices = unique(by_state_program_completers$state)), 
            selectInput(inputId = "program_type", label = "Program Type", choices = c("Total", "By Program Type"))
            ), 
        mainPanel(
            plotOutput(outputId = "line")
            )
    )
)

server <- function(input, output) {
    output$line <- renderPlot({
        by_state_program_completers %>% plot_completers(state = input$state, type = input$program_type)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
