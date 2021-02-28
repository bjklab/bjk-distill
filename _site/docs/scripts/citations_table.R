#' #################################
#' load libraries and set seed
#' #################################
library(tidyverse)
#library(bibtex)
library(gt)

#' add gt formatting functions
source("./scripts/gt_themes.R")


#' #################################
#'  read and format citation data
#' #################################

read_lines(file = "./bib/bjk_bib.txt") %>%
  enframe(name = "citation_index", value = "citation") %>%
  filter(citation != "") %>%
  select(-citation_index) %>%
  separate(col = citation, into = c("authors","title","journal","date_vol_page","link"), sep = "\\. ") %>%
  mutate(link = gsub("Available from\\: ","",link)) %>%
  separate(link, into = "link", sep = " ", remove = TRUE, extra = "drop") %>%
  filter(grepl("reply",title) == FALSE) %>%
  mutate(year = stringr::str_extract(string = date_vol_page, pattern = paste0(seq(2000,2040,1), collapse = "|"))) %>%
  select(-date_vol_page) %>%
  mutate(
    link = glue::glue("<a href='{link}'>{link}</a>"),
    link = map(link, gt::html)) %>%
  gt() %>%
  gt::cols_label(authors = "Authors", title = "Title", journal = "Journal", year = "Year", link = "Link") %>%
  gt_black_grey() %>%
  gt::tab_options(column_labels.font.size = 16)




