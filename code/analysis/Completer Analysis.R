# TITLE: Completer Analysis
# AUTHOR(S): Dustin Pashouwer

# DESCRIPTION: [enter a few comments about what this script does]

# Load packages - first pacman, installing if necessary, then others
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(here, readxl, tidyverse, janitor) # add more here as needed
if (!suppressPackageStartupMessages(require("tntpr"))) {pacman::p_load(devtools); devtools::install_github("tntp/tntpr")}; pacman::p_load(tntpr)

# Load data
by_state_program_completers <- readRDS(here::here("data/clean/by_state_program_completers.rds"))

### Total Completers, by state, by year -------------------------------

##### functions

plot_total_completers_by_year <- function(dat, state){
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
           title = paste0("Number of Completers by Year: ", state)) + 
      theme_tntp_2018()
  p
}

##### implement functions
total_completers_by_year <- by_state_program_completers %>% 
  mutate(plot_total_completers_by_year = map2(data, state, plot_total_completers_by_year))

#### ex. how to access plots
total_completers_by_year %>% 
  filter(state == "Wisconsin") %>% 
  pull(plot_total_completers_by_year)


### Total Completers, by state, by program type, by year -------------------------------

##### functions
plot_total_completers_by_program_type_by_year <- function(dat, state){
  p <- dat %>% 
    filter(ihe_name == "TOTAL") %>% 
    group_by(year, program_type) %>% 
    mutate(n_completers = sum(as.numeric(n_completers))) %>% 
    ggplot(aes(year, n_completers, color = program_type)) + 
      geom_line(aes(group = program_type)) + 
      geom_text(aes(label = n_completers)) + 
      expand_limits(y = 0) +
      labs(x = "School Year", 
           y = "Number of Completers", 
           title = paste0("Number of Completers by Year: ", state)) + 
      theme_tntp_2018() + 
      scale_color_tntp()
  p
}

##### implement functions
total_completers_by_program_type_by_year <- by_state_program_completers %>% 
  mutate(plot_total_completers_by_program_type_by_year = map2(data, state, plot_total_completers_by_program_type_by_year))

#### ex. how to access plots
total_completers_by_program_type_by_year %>% 
  filter(state == "Wisconsin") %>% 
  pull(plot_total_completers_by_program_type_by_year)


### Specific program(s), by year -------------------------------

##### functions
plot_total_completers_by_program_by_year <- function(dat, state){
  p <- dat %>% 
    filter(ihe_name != "TOTAL") %>% 
    group_by(ihe_name, year) %>% 
    summarise(n_completers = sum(as.numeric(n_completers))) %>% 
    ggplot(aes(year, n_completers, color = ihe_name)) + 
      geom_line(aes(group = ihe_name)) + 
      geom_text(aes(label = n_completers)) + 
      expand_limits(y = 0) + 
      labs(x = "School Year", 
           y = "Number of Completers", 
           title = paste0("Number of Completers by Year: ", state)) + 
      theme_tntp_2018() + 
      scale_fill_discrete()
    p
}

##### implement functions
total_completers_by_program_by_year <- by_state_program_completers %>% 
  mutate(plot_total_completers_by_program_by_year = map2(data, state, plot_total_completers_by_program_by_year))

#### ex. how to access plots

plot_total_completers_by_program_by_year_tn <- total_completers_by_program_by_year %>% 
  filter(state == "Wisconsin") %>% 
  pull(plot_total_completers_by_program_by_year)

plot_total_completers_by_program_by_year_tn[[1]]

