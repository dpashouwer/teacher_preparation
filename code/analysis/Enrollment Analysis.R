# TITLE: Enrollment Analysis
# AUTHOR(S): Dustin Pashouwer

# DESCRIPTION: [enter a few comments about what this script does]

# Load packages - first pacman, installing if necessary, then others
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(here, readxl, tidyverse, janitor) # add more here as needed
if (!suppressPackageStartupMessages(require("tntpr"))) {pacman::p_load(devtools); devtools::install_github("tntp/tntpr")}; pacman::p_load(tntpr)

# Load data
by_state_enrollment <- readRDS(here::here("data/clean/by_state_enrollment.rds"))

### Enrollment, percent teachers of color -------------------------------

##### functions

calc_perc_poc <- function(dat, level = c("state", "program_type", "program")) {
  if(level == "state") {
     dat %>% 
      summarise(perc_poc = round((sum(total_enrollment) - sum(white)) / sum(total_enrollment), 2),
                n_enrolled = sum(total_enrollment)) %>% 
      arrange(desc(perc_poc))
  } else if (level == "program_type") {
     dat %>% 
      group_by(program_type) %>% 
      summarise(perc_poc = round((sum(total_enrollment) - sum(white)) / sum(total_enrollment), 2), 
                n_senrolled = sum(total_enrollment)) %>% 
      arrange(desc(perc_poc))
  } else {
    dat %>% group_by(ihe_name, program_type) %>% 
      summarise(perc_poc = round((sum(total_enrollment) - sum(white)) / sum(total_enrollment), 2), 
                n_enrolled = sum(total_enrollment)) %>% 
      arrange(desc(perc_poc))
  }
}


##### implement functions
enrollment_perc_poc <- by_state_enrollment %>% 
  mutate(calc_perc_poc_state = map(data, calc_perc_poc, "state"), 
         calc_perc_poc_program_type = map(data, calc_perc_poc, "program_type"), 
         calc_perc_poc_program = map(data, calc_perc_poc, "program"))

#### ex. how to access plots
enrollment_perc_poc %>% 
  filter(state == "Tennessee") %>% 
  pull(calc_perc_poc_program)
