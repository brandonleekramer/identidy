
# skills dataset --------------------------------------------------

library(dplyr)
skills_data <- readr::read_csv("data-raw/skills_data.csv") 
readr::write_rds(skills_data, "R/skills_data.rds")
usethis::use_data(skills_data, overwrite = TRUE)


# skills dictionary --------------------------------------------------

library(dplyr)
skills <- readr::read_csv("data-raw/identidy - skills.csv") 
readr::write_rds(skills, "R/skills.rds")
usethis::use_data(skills, overwrite = TRUE)
usethis::use_data(skills, internal = TRUE, overwrite = TRUE)







