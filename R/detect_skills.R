#' Detect and standardize occupational skills in unstructured text data.
#'
#' Explanation here.
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character vector of text data that includes the names of skills you want to detect.
#' @param output Output column can be named whatever name you want. 
#'
#' @examples
#'
#' library(tidyverse)
#' library(identidy)
#' data(skills_data)
#' 
#' skills_data <- skills_data %>% 
#'   rename(skill_name = skill) %>% 
#'   rowid_to_column("rowid")
#'
#' classified_skills <- skills_data %>%
#'   detect_skills(rowid, skill_name, recoded_skill)
#'
#' @export
detect_skills <- function(data, id, input, output){
  
  pb <- progress::progress_bar$new(total = 100)
  pb$tick(0)
  
  # 1. convert all vars with enquos
  id <- enquo(id)
  input <- enquo(input)
  output <- enquo(output)
  `%notin%` <- Negate(`%in%`)
  
  # 2. error messages 
  if (missing(id)) { 
    return(print("Error: 'id' column requires numeric or character vector."))
  } else if (missing(input)) { 
    return(print("Error: 'input' column requires character vector."))
    # need to add in output error messages 
  }
  
  # 3. prep the dictionary 
  dictionary <- skills #readr::read_rds(file = "R/skills.rds")
  
  # 4. run the funnel process 
  # 4a. pull the max_n (longest certification sequence)
  max_n <- dictionary %>%
    tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
    dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) 
  max_n <- max(max_n$word_count)  
  # 4b. set up the dataframe to write to 
  funnelized <- data.frame()
  data <- data %>% dplyr::mutate("{{input}}" := tolower(!!input))
  
  pb$tick(10)
  
  # 4c. funnel through the word sequences to match terms to dictionary 
  funnelized <- data.frame()
  for (n_word in max_n:2) {
    subdictionary <- dictionary %>%
      tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
      dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) %>%
      dplyr::filter(word_count == n_word)
    subdictionary <- stats::na.omit(subdictionary$catch_terms)
    funnelized <- data %>%
      tidytext::unnest_tokens(words, !!input, token="ngrams", n=n_word, to_lower = TRUE) %>%
      dplyr::filter(words %in% subdictionary) %>%
      dplyr::select(!!id, words) %>%
      dplyr::bind_rows(funnelized)
  }
  # 5. funnel match on all of the single tokens 
  subdictionary <- dictionary %>%
    tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
    dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) %>%
    dplyr::filter(word_count == 1)
  subdictionary <- stats::na.omit(subdictionary$catch_terms)
  funnelized <- data %>%
    #dplyr::filter(!!id %notin% ids_to_filter) %>%
    tidytext::unnest_tokens(words, !!input) %>%
    dplyr::filter(words %in% subdictionary) %>%
    dplyr::select(!!id, words) %>%
    dplyr::bind_rows(funnelized) %>% 
    dplyr::select(!!id, words) 
  dictionary <- dictionary %>%
    dplyr::mutate(original_string = paste0("\\b(?i)(",recode_column,")\\b")) %>%
    dplyr::select(original_string, skill_name) %>% tibble::deframe()
  
  pb$tick(20)
  
  all_matched_data <- funnelized %>%
    dplyr::mutate("{{ output }}" := stringr::str_replace_all(words, dictionary)) %>% 
    dplyr::select(!!id, !!output)
  
  pb$tick(60)
  
  suppressMessages(
    data <- data %>% 
      dplyr::left_join(all_matched_data) %>%
      dplyr::rename(skill_temp := !!output) %>% 
      dplyr::distinct(across(everything())) %>%
      dplyr::group_by(!!id, !!input) %>%
      dplyr::mutate(skill_temp =  paste0(skill_temp, collapse = "|")) %>% 
      dplyr::distinct(across(everything())) %>%
      dplyr::mutate("{{output}}" := dplyr::na_if(skill_temp, "NA")) %>% 
      dplyr::rename_all(~stringr::str_replace_all(.,"\"","")) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-skill_temp) 
  ) 
  pb$tick(60)
  data
  
}