source("reporting_tools.R")
# library(clipr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(scales)


# Blog catalog ----
load(file = "perf_data/blog.Rda")

bg_compare_dat <- 
  rbind(
    perf_as_df(apply(bg_perf$micro_f1, c(1, 2), function(x) sd(x) / sqrt(5)), "Micro-F1"),
    perf_as_df(apply(bg_perf$macro_f1, c(1, 2), function(x) sd(x) / sqrt(5)), "Macro-F1")
  ) %>% 
  gather(key = "Split", value = "Std Error", -Method, -Type, -measure) %>%
  mutate(Labeled = as.numeric(str_replace(Split, "split_", "")),
         Data = "Blog Catalog",
         Penalty = str_extract(Method, "\\d{2,4}") %>% 
           paste0(".", .) %>% 
           as.numeric(),
         Method = str_replace(Method, "_.*", ""))

# Wikipedia ----
load(file = "perf_data/wiki.Rda")

wiki_compare_dat <- 
  rbind(
    perf_as_df(apply(wiki_perf$micro_f1, c(1, 2), function(x) sd(x) / sqrt(5)), "Micro-F1"),
    perf_as_df(apply(wiki_perf$macro_f1, c(1, 2), function(x) sd(x) / sqrt(5)), "Macro-F1")
  ) %>% 
  gather(key = "Split", value = "Std Error", -Method, -Type, -measure) %>%
  mutate(Labeled = as.numeric(str_replace(Split, "split_", "")),
         Data = "Wikipedia",
         Penalty = str_extract(Method, "\\d{2,4}") %>% 
           paste0(".", .) %>% 
           as.numeric(),
         Method = str_replace(Method, "_.*", ""))

# Flickr ----
load(file = "perf_data/flickr.Rda")

flickr_compare_dat <- 
  rbind(
    perf_as_df(apply(flickr_perf$micro_f1, c(1, 2), function(x) sd(x) / sqrt(5)), "Micro-F1"),
    perf_as_df(apply(flickr_perf$macro_f1, c(1, 2), function(x) sd(x) / sqrt(5)), "Macro-F1")
  ) %>% 
  gather(key = "Split", value = "Std Error", -Method, -Type, -measure) %>%
  mutate(Labeled = as.numeric(str_replace(Split, "split_", "")),
         Data = "Flickr",
         Penalty = str_extract(Method, "\\d{2,4}") %>%
           paste0(".", .) %>% 
           as.numeric(),
         Method = str_replace_all(Method, "_flickr|_\\d{2,4}|true_", "") %>%
           str_replace("line", "LINE"))

# all std errors together ----
compare_dat <- rbind(flickr_compare_dat, bg_compare_dat, #ppi_compare_dat, 
                     wiki_compare_dat) %>%
  mutate(Method = str_replace_all(Method, 
                                  c("kkk" = "NN-NS", 
                                    "LINE" = "NS",
                                    "rns" = "RNS",
                                    "nopenalty" = "RNS No Penalty")))

compare_table <- compare_dat %>% 
  group_by(Method, Type, measure, Labeled, Data) %>%
  mutate(is_best = ifelse(Method == "RNS",
                          (Type == "t1_embed" & Penalty %in% 
                             c(0.0154, 0.0144, 0.0178)) |
                            (Type == "t2_embed" & Penalty %in% 
                               c(0.0197, 0.0186, 0.0224)), 
                          `Std Error` == max(`Std Error`))) %>%
  ungroup() %>%
  filter(Labeled == 0.5 & is_best)

# Type I embedding, mIcro f1
compare_table %>% 
  filter(Type == "t1_embed", measure == "Micro-F1") %>%
  select(-Type, -Split, -Labeled, -is_best) %>%
  unite(m_data, measure, Data) %>%
  spread(key = m_data, value = `Std Error`)

# Type I embedding, mAcro f1
compare_table %>% 
  filter(Type == "t1_embed", measure == "Macro-F1") %>%
  select(-Type, -Split, -Labeled, -is_best) %>%
  unite(m_data, measure, Data) %>%
  spread(key = m_data, value = `Std Error`)

# Type II embedding, mIcro f1
compare_table %>% 
  filter(Type == "t2_embed", measure == "Micro-F1") %>%
  select(-Type, -Split, -Labeled, -is_best) %>%
  spread(key = Data, value = `Std Error`)

# Type II embedding, mAcro f1
compare_table %>% 
  filter(Type == "t2_embed", measure == "Macro-F1") %>%
  select(-Type, -Split, -Labeled, -is_best) %>%
  spread(key = Data, value = `Std Error`)
