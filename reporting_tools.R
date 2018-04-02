## Plotting and table functions

library(tidyverse)
library(knitr)

## tables ----

perf_table <- function(perf_array, type1, table_format = "html", font_size, title) {
  # perf_array - array of performance metric, embedding x split x iter
  # type1 - flag for type 1 embedding
  tbl <- apply(perf_array, MARGIN = c(1, 2), mean) %>%
    as_tibble(rownames = "method") %>%
    mutate(type1 = mod(1:nrow(.), 2) == type1) %>%
    filter(type1) %>% select(-type1) %>%
    mutate(method = cell_spec(method, format = table_format, escape = TRUE)) %>%
    mutate_if(is.numeric, percent) %>%
    mutate_if(is.numeric, .funs = function(x) {
      idx <- which.max(x)
      x <- cell_spec(x, format = table_format, bold = FALSE)
      x[idx] <- cell_spec(x[idx], format = table_format, bold = TRUE, escape = FALSE)
      x
    }) %>%
    kable(caption = title, format = table_format, booktabs = TRUE, escape = FALSE) %>% 
    kable_styling(font_size = font_size, latex_options = c("scale_down"))
}

## plots ----

perf_as_df <- function(perf_mat, measure_name) {
  # perf_mat - matrix of performance metric, embedding x split
  # measure_name - name of performance metric, e.g. "Micro-F1"
  
  perf_mat %>%
    as_data_frame() %>%
    mutate(method = rownames(perf_mat), measure = measure_name) %>%
    separate(method, c("Method", "Type"), sep = "\\.")   
}

