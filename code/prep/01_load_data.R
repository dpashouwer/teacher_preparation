# TITLE: Pull all data
# AUTHOR(S): Dustin Pashouwer

# DESCRIPTION: [enter a few comments about what this script does]

# Load packages - first pacman, installing if necessary, then others
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(here, readxl, tidyverse, janitor) # add more here as needed
if (!suppressPackageStartupMessages(require("tntpr"))) {pacman::p_load(devtools); devtools::install_github("tntp/tntpr")}; pacman::p_load(tntpr)

# Load data
all_files <- tibble(
  file_names_full = list.files(here::here("data/raw"), full.names = TRUE, pattern = ".xls"), 
  file_names_short = list.files(here::here("data/raw"), pattern = ".xls")) %>% 
  separate(file_names_short, into = c("pull_date", "category", "file_title"), sep = "_") %>% 
  mutate(sheets = map(file_names_full, excel_sheets)) %>% 
  unnest() %>% 
  mutate(data = map2(file_names_full, sheets, ~ read_excel(path = .x, sheet = .y))) %>% 
  group_nest(category)

# Save data
saveRDS(all_files, here::here("data/raw/all_files.rds"))
