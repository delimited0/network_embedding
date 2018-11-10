source("reporting_tools.R")
source("experiment_tools.R")

library(ggplot2)
library(ggrepel)
library(cowplot)
library(scales)
library(grid)
library(gridExtra)

# Load data ----

load(file = "perf_data/blog.Rda")

bg_label <- readMat("BlogCatalog/LabelBlogCatalog.mat")
bg_embeds <- lapply(dir(path = "BlogCatalog", pattern = "*_BlogCatalog",
                        full.names = TRUE), readMat, USE.NAMES = TRUE)
names(bg_embeds) <- dir(path = "BlogCatalog", pattern = "*_BlogCatalog") %>% 
  gsub(x = ., pattern = ".mat", replacement = "")

bg_label_mat <- bg_label$labelmatrix == 1
colnames(bg_label_mat) <- paste0("class_", 1:ncol(bg_label_mat))

bg_embed_dats <- 
  lapply(bg_embeds, 
         function(x) list(t1_embed = as_embed_df(get_type_k(x, 1),
                                                 bg_label_mat), 
                          t2_embed = as_embed_df(get_type_k(x, 2),
                                                 bg_label_mat))
  )

# learner settings ----

n_iter <- 5  # number of mccv iterations
roc_split <- 0.5
n_labels <- 6
big_label_idx <- order(colSums(bg_label_mat), decreasing = TRUE)[1:n_labels]
t1_models <- c("LINE_BlogCatalog", "kkk_BlogCatalog", "nopenalty_BlogCatalog",
               "rns_015_BlogCatalog")
t2_models <- c("LINE_BlogCatalog", "kkk_BlogCatalog", "nopenalty_BlogCatalog",
               "rns_0197_BlogCatalog")

l2logreg <- makeLearner("classif.LiblineaRL2LogReg", predict.type = "prob")
desc <- makeResampleDesc(method = "Holdout", stratify = FALSE, split = roc_split)

# plot theme ----
roc_theme <- 
  list(theme_bw(), 
       theme(legend.position = "bottom", legend.text = element_text(size = 12),
             axis.title.x = element_blank(), axis.title.y = element_blank(),
             axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)),
       scale_x_continuous(breaks = seq(0, 1, 0.2)),
       scale_size_manual(values = c(1, 1, 3, 1)))

# t1 embeds ----
t1_plot_df <- Reduce(rbind, lapply(big_label_idx, function(label_idx) {
  tasks <- lapply(t1_models, function(model_name) {
    makeClassifTask(id = paste0(model_name, "_t1"), 
                    data = bg_embed_dats[[model_name]]$t1_embed[, c(1:100, 100+label_idx)], 
                    target = paste0("class_", label_idx))
  })
  results <- lapply(tasks, function(t) resample(l2logreg, t, desc, measures = list(fpr, tpr)))
  names(results) <- t1_models
  df <- generateThreshVsPerfData(results, measures = list(fpr, tpr), aggregate = FALSE)
  df$data$Label <- label_idx
  df$data
})) 

t1_plot_df %<>% 
  mutate(Method = str_replace_all(learner, 
                                  c("kkk" = "NN-NS", 
                                    "LINE" = "NS",
                                    "rns" = "RNS",
                                    "nopenalty" = "RNS No Penalty",
                                    "BlogCatalog" = "",
                                    "_" = "",
                                    "\\d\\d\\d" = "")))


t1_plot <- ggplot(t1_plot_df, aes(x = fpr, y = tpr, color = Method)) +
  facet_wrap(vars(Label)) +
  geom_path() +
  roc_theme

# t2 embeds ----
t2_plot_df <- Reduce(rbind, lapply(big_label_idx, function(label_idx) {
  tasks <- lapply(t2_models, function(model_name) {
    makeClassifTask(id = paste0(model_name, "_t2"), 
                    data = bg_embed_dats[[model_name]]$t2_embed[, c(1:100, 100+label_idx)], 
                    target = paste0("class_", label_idx))
  })
  results <- lapply(tasks, function(t) resample(l2logreg, t, desc, measures = list(fpr, tpr)))
  names(results) <- t2_models
  df <- generateThreshVsPerfData(results, measures = list(fpr, tpr), aggregate = FALSE)
  df$data$Label <- label_idx
  df$data
}))

t2_plot_df %<>% 
  mutate(Method = str_replace_all(learner, 
                                  c("kkk" = "NN-NS", 
                                    "LINE" = "NS",
                                    "rns" = "RNS",
                                    "nopenalty" = "RNS No Penalty",
                                    "BlogCatalog" = "",
                                    "_" = "",
                                    "\\d\\d\\d\\d" = "")))

t2_plot <- ggplot(t2_plot_df, aes(x = fpr, y = tpr, color = Method)) +
  facet_wrap(vars(Label)) +
  geom_path() +
  roc_theme

# roc plot ----
roc_plot <- plot_grid(
  ggdraw() + draw_label("Type I Objective", x = 0, hjust = 0),
  ggdraw() + draw_label("Type II Objective", x = 0, hjust = 0),
  t1_plot + theme(legend.position = "none"), 
  t2_plot + theme(legend.position = "none"),
  nrow = 2, ncol = 2, rel_heights = c(.1, 1)
) #%>% plot_grid(get_legend(t1_plot), ncol = 1, rel_heights = c(1, .1))

y_grob <- textGrob("True Positive Rate", 
                   gp=gpar(fontsize=12), rot=90)
x_grob <- textGrob("False Positive Rate", 
                   gp=gpar(fontsize=12))
roc_plot <- grid.arrange(arrangeGrob(roc_plot, left = y_grob, bottom = x_grob))

roc_plot %<>% plot_grid(get_legend(t1_plot), ncol = 1, rel_heights = c(1, .1))

ggsave("roc_plot.pdf", plot = roc_plot, device = "pdf", path = "plots", 
       height = 5, width = 11)



