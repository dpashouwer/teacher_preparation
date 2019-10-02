# TITLE: Clean data
# AUTHOR(S): Dustin Pashouwer

# DESCRIPTION: [enter a few comments about what this script does]

# Load packages - first pacman, installing if necessary, then others
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(here, readxl, tidyverse, janitor) # add more here as needed
if (!suppressPackageStartupMessages(require("tntpr"))) {pacman::p_load(devtools); devtools::install_github("tntp/tntpr")}; pacman::p_load(tntpr)

# Load data
all_files <- readRDS(here::here("data/raw/all_files.rds"))

# Completer data ---------------------------

completer_files <- all_files %>% 
  dplyr::filter(category == "Completers") %>% 
  dplyr::pull(data)

program_completers_list <- completer_files[[1]] %>% 
  dplyr::filter(file_title == "Completers, by state, by IHE level.xls", sheets == "Program Completers") %>% 
  dplyr::pull(data)

program_completers <- program_completers_list[[1]] %>% 
  purrr::set_names(program_completers_list[[1]][6,]) %>% 
  janitor::clean_names() %>% 
  dplyr::slice(7:n()) %>% 
  dplyr::select(-na)

# Analysis
by_state_program_completers <- program_completers %>% 
  tidyr::gather(year, n_completers, -c(state, ihe_name, program_type)) %>% 
  mutate(year = year %>% str_remove("ay_") %>% 
           str_replace("_", "-"), 
         state = ifelse(state == "TOTAL", "01_All States Combined", state)) %>% 
  dplyr::group_nest(state) %>% 
  dplyr::filter(! state %in% c("Source: U.S. Department of Education, Higher Education Act Title II State Report Card System", NA_character_)) %>% 
  dplyr::arrange(state) %>% 
  dplyr::mutate(state = str_remove(state, "01_"))

saveRDS(by_state_program_completers, here::here("data/clean/by_state_program_completers.rds"))
saveRDS(by_state_program_completers, here::here("output/Teacher Preparation App/by_state_program_completers.rds"))

# Enrollment data ---------------------------

enrollment_files <- all_files %>% 
  filter(category == "Enrollment") %>% 
  pull(data)

enrollment_by_state_by_gender_by_race_files_list <- enrollment_files[[1]] %>% 
  filter(file_title == "Enrollment, by state, by gender and race-ethnicity.xls") %>% 
  pull(data)

enrollment_by_state_by_gender_by_race <- enrollment_by_state_by_gender_by_race_files_list[[1]] %>% 
  purrr::set_names(enrollment_by_state_by_gender_by_race_files_list[[1]][6,]) %>% 
  janitor::clean_names() %>% 
  slice(7:n()) %>% 
  select(-na) %>% 
  mutate_at(vars(-state, -ihe_name, -program_type), 
            as.numeric)

# Analysis
by_state_enrollment <- enrollment_by_state_by_gender_by_race %>% 
  group_nest(state) %>% 
  filter(str_detect(state, "Source:") == FALSE,
         str_detect(state, "An enrolled student is an individual admitted") == FALSE)

saveRDS(by_state_enrollment, here::here("data/clean/by_state_enrollment.rds"))
saveRDS(by_state_enrollment, here::here("output/Teacher Preparation App/by_state_enrollment.rds"))

