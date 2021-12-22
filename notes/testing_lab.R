
rm(list = ls())
library(tidyverse)

data(skills_data) 

skills_data <- skills_data %>% 
  rename(skill_name = skill) %>% 
  rowid_to_column("rowid")

load_all()
testing_df <- skills_data %>% 
  detect_skills(rowid, skill_name, new_skill_name)

