# Global -------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(janitor)
library(tntpr)
library(here)
library(scales)
library(knitr)
library(kableExtra)

# Load data 
by_state_program_completers <- readRDS("by_state_program_completers.rds")
by_state_program_enrollment <- readRDS("by_state_enrollment.rds")

# Functions 

### Completers
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
            geom_text(aes(label = scales::comma(n_completers)))
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
            geom_text(aes(label = scales::comma(n_completers)))
}

plot_completers <- function(dat, state, type = "Total"){
    
    plot_formatting <- function(plot){
        plot + 
            theme_tntp_2018() + 
            scale_y_continuous(limits=c(0,NA), labels = scales::comma) + 
            labs(x = "Academic Year", y = "Total Completers", 
                 color = NULL) +
            scale_color_tntp()
    }
    
    if(type == "Total"){
        plot_total_completers(dat, state = state) %>% 
            plot_formatting()
        
    } else if(type == "By Program Type"){
        plot_total_completers_by_type(dat, state = state) %>% 
            plot_formatting()
    } 
}


### Enrollment
table_enrollment <- function(dat, state){
  dat %>% 
    filter(state == !!state) %>% 
    pull(data) %>% 
    as.data.frame() %>% 
    arrange(desc(total_enrollment)) %>% 
    mutate(perc_poc = scales::percent((total_enrollment - white) / total_enrollment, accuracy = 1),
           total_enrollment = round(total_enrollment, digits = 0)) %>% 
    distinct(`Program` = ihe_name, `Program Type` = program_type, `Total Enrollment` = total_enrollment, `Percent POC` = perc_poc)
}

# UI ------------------------
ui <- dashboardPage(
    skin = 'blue',
    dashboardHeader(
        title = "Teacher Preparation", 
        titleWidth = 300
    ), 
    dashboardSidebar(
        width = 300, 
        sidebarMenu(
            menuItem('Overview', tabName = "overview",icon = icon('school')), 
            menuItem('Completers', tabName = "completers",icon = icon('bookmark')), 
            menuItem('Prep Programs', tabName = "programs",icon = icon('bookmark'))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "overview",
                        fluidPage(
                            sidebarLayout(
                                sidebarPanel(
                                    h1('Exploring Teacher Preperation'), 
                                    h3(strong('Motivation')), 
                                    h4('Decreasing supply of new teachers.'),
                                    h3(strong('Key Data Questions')), 
                                    h3(strong('Data Sources')),
                                    h4(tags$ul(
                                        tags$li(completers <- a('Title II: Higher Education Act', href="https://title2.ed.gov/Public/DataTools/Tables.aspx"))
                                    )), 
                                    width = 12), 
                                mainPanel()
                            )
                        )
            ),
            tabItem(
                tabName = "completers",
                fluidPage(
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
            ),
            tabItem(
              tabName = "programs",
              fluidPage(
                verticalLayout(
                  sidebarPanel(
                    selectInput(inputId = "state_program_table", label = "States", choices = unique(by_state_program_enrollment$state))
                  ), 
                  mainPanel(
                    htmlOutput(outputId = "programs_table")
                  )
                )
              )
            )
        )
    )
)

# Server ------------------------
server <- function(input, output) {
    output$line <- renderPlot({
        by_state_program_completers %>% plot_completers(state = input$state, type = input$program_type)
    })
    output$programs_table <- renderText({
        by_state_program_enrollment %>% 
            table_enrollment(state = input$state_program_table) %>% 
            kable(format = "html") %>% 
            kable_styling(bootstrap_options = "striped", full_width = TRUE)
    })
}


# Run the application  ------------------------
shinyApp(ui = ui, server = server)
