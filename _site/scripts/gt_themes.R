#' load libraries
library(tidyverse)
library(gt)
#library(tidybayes)
#library(brms)
set.seed(16)



#' create dummy data

tibble(subject_id = paste0("subject",seq(100)), group = paste0("group ",rep(c(1,2), each = 50))) %>%
  expand(nesting(subject_id, group), test = c("liver", "kidney", "heart", "blood_one", "blood_two")) %>%
  mutate(test_value = runif(n = n(), min = 0, max = 1)) %>%
  pivot_wider(names_from = test, values_from = test_value, id_cols = c(subject_id, group)) %>%
  gtsummary::tbl_summary(by = "group", include = -subject_id) %>%
  gtsummary::as_gt() %>%
  gt::tab_row_group(group = "Blood Tests", rows = 1:2) %>%
  gt::tab_row_group(group = "Other Tests", rows = 3:5) %>%
  identity() -> dummy_gt

dummy_gt


#' ###########################
#' gt themes
#' ###########################

#' black lines
gt_black <- function(data, ...) {
  data %>% 
    tab_options(
      # fonts
      table.font.size = 11,
      column_labels.font.size = 11,
      source_notes.font.size = 11,
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold",
      # lines
      table_body.hlines.color = "black",
      table_body.vlines.color = "black",
      column_labels.border.bottom.color = "black",
      column_labels.border.top.color = "black",
      table_body.border.bottom.color = "black",
      table_body.border.top.color = "black",
      #table_body.border.right.color = "black",
      #table_body.border.left.color = "black",
      table.border.bottom.color = "white",
      table.border.top.color = "black",
      table.border.right.color = "black",
      table.border.left.color = "black",
      row_group.border.bottom.color = "black",
      row_group.border.top.color = "black",
      summary_row.border.color = "black",
      grand_summary_row.border.style = "solid",
      grand_summary_row.border.width = 2,
      grand_summary_row.border.color = "black",
      heading.border.bottom.color = "black",
      heading.border.lr.color = "black",
      stub.border.color = "black",
      # strips
      ...
    )
}


dummy_gt %>%
  gt_black()


#' black lines, grey group headings
gt_black_grey <- function(data, ...) {
  data %>% 
    tab_options(
      # fonts
      table.font.size = 11,
      column_labels.font.size = 11,
      source_notes.font.size = 11,
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold",
      # lines
      table_body.hlines.color = "black",
      table_body.vlines.color = "black",
      column_labels.border.bottom.color = "black",
      column_labels.border.top.color = "black",
      table_body.border.bottom.color = "black",
      table_body.border.top.color = "black",
      #table_body.border.right.color = "black",
      #table_body.border.left.color = "black",
      table.border.bottom.color = "white",
      table.border.top.color = "black",
      table.border.right.color = "black",
      table.border.left.color = "black",
      row_group.border.bottom.color = "black",
      row_group.border.top.color = "black",
      summary_row.border.color = "black",
      grand_summary_row.border.style = "solid",
      grand_summary_row.border.width = 2,
      grand_summary_row.border.color = "black",
      heading.border.bottom.color = "black",
      heading.border.lr.color = "black",
      stub.border.color = "black",
      # strips
      row_group.background.color = "lightgrey",
      ...
    )
}


dummy_gt %>%
  gt_black_grey()



#' column stripes
gt_col_stripe <- function(data, ...) {
  data %>% 
    data_color(
      columns = gt::everything()[gt::everything() %% 2 == 1],
      colors = "lightgrey",
      ...
    )
}


dummy_gt %>%
  gt_black() %>%
  gt_col_stripe()


gt_col_stripe_specific <- function(data, which_cols = c("label", "stat_2")) {
  data %>% 
    data_color(
      columns = which_cols,
      colors = "lightgrey",
    )
}

dummy_gt %>%
  gt_black() %>%
  gt_col_stripe_specific()





#' highlight column data with Thomas Mock color scheme
#' palette suggestion from Thomas Mock: https://themockup.blog/posts/2020-05-16-gt-a-grammer-of-tables/

gt_col_viridivar <- function(data, col_num) {
  data %>% 
    data_color(
      columns = col_num,
      colors = scales::col_numeric(
        # custom defined values - notice that order matters!
        # palette suggestion from Thomas Mock: https://themockup.blog/posts/2020-05-16-gt-a-grammer-of-tables/
        palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
        domain = NULL
    )
    )
}


tibble(subject = letters,
       var1 = runif(length(letters), min = -1, max = 1),
       var2 = runif(length(letters), min = -1, max = 1),
       var3 = runif(length(letters), min = -1, max = 1),
       var4 = runif(length(letters), min = -1, max = 1),) %>%
  gt() %>%
  gt_black() %>%
  gt_col_viridivar(c(2,4))



#' highlight rows according to key column values

gt_row_highlight <- function(data, rule_var, less_than_value) {
  data %>% 
    tab_style(
      style = list(
        cell_fill(color = "lightcyan")#,
        #cell_text(weight = "bold")
        ),
      locations = cells_body(
        columns = gt::everything(),
        rows = .data[[rule_var]] < less_than_value)
    )
}


tibble(subject = letters,
       var1 = runif(length(letters), min = -1, max = 1),
       var2 = runif(length(letters), min = -1, max = 1),
       var3 = runif(length(letters), min = -1, max = 1),
       var4 = runif(length(letters), min = -1, max = 1),) %>%
  gt() %>%
  gt_black() %>%
  gt_row_highlight("var4", -0.5)


tibble(subject = letters,
       var1 = runif(length(letters), min = -1, max = 1),
       var2 = runif(length(letters), min = -1, max = 1),
       var3 = runif(length(letters), min = -1, max = 1),
       var4 = runif(length(letters), min = -1, max = 1),) %>%
  gt() %>%
  gt_black() %>%
  gt_row_highlight("var4", -0.5) %>%
  gt_col_viridivar("var3")
  
  


