library(rvest)
library(readr)
library(dplyr)
library(janitor)

#web scraping

webpage <- read_html("https://www.pwrc.usgs.gov/bbl/manual/speclist.cfm")

tbls <- html_nodes(webpage, "table") %>% 
  html_table(fill = TRUE)

species <- tbls[[1]] %>% 
  clean_names() %>% 
  select(alpha_code, common_name) %>% 
  mutate(alpha_code = tolower(alpha_code) %>% rename(species=alpha_code)
  
# load data
nest_data <- read.csv ("https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A982bd2fc-4edf-4da7-96ef-0d11b853102d")
head(nest_data)

predation_data <- read.csv("https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A9ffec04c-7e2d-41dd-9e88-b6c2e8c4375e")
head(predator_data)

predator_species_join <- left_join(predator_data,species,by = c("species"="alpha_code")) %>% select(year, species, count)
 
nest_species_join <- left_join(nest_data, species, by = c("species"="alpha_code")) %>% select(common_name, )

#' Function to rename spp code to common names
#'
#' @param df 
#' @param species 
#'
#' @return common names
#' @export
#'
#' @examples
#' 
assign_species_name <- function(df, species){
  if (!("alpha_code" %in% names(species)) |
      !("species" %in% names(df)) |
      !("common_name" %in% names(species))){
    stop("Tables appear to be formatted incorrectly.")
  }  
  
  
  return_df <- left_join(df, species, by = c("species" = "alpha_code"))
  
  if (nrow(return_df) > nrow(df)){
    warning("Joined table has more rows than original table. Check species table for duplicated code values.")
  }
  
  
  
  
  
  
  
  if (length(which(is.na(return_df$common_name))) > 0){
    x <- length(which(is.na(return_df$common_name)))
    warning(paste("Common name has", x, "rows containing NA"))
  }
  
  return(return_df)
  
}
  
  
  