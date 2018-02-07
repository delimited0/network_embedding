library(R.matlab)
library(tidyverse)
library(magrittr)
library(mlr)
library(reticulate)
library(knitr)

# reticulate::py_discover_config()
use_python("/Users/patrickding/anaconda3/bin/python")
sk <- reticulate::import("sklearn")

mm.f1.fun <- function(task, model, pred, feats, extra.args) {
  # scikit-learn f1 score
  sk$metrics$f1_score(y_true = getPredictionTruth(pred), 
                      y_pred = getPredictionResponse(pred), 
                      average = extra.args$average)
}
micro.f1 <- makeMeasure(id = "micro.f1", name = "micro f1 score", 
                        minimize = FALSE, 
                        properties = c("classif", "classif.multi", "multilabel"),
                        best = 1, worst = 0, fun = mm.f1.fun,
                        extra.args = list(average = "micro"))
macro.f1 <- makeMeasure(id = "macro.f1", name = "macro f1 score", 
                        minimize = FALSE, 
                        properties = c("classif", "classif.multi", "multilabel"),
                        best = 1, worst = 0, fun = mm.f1.fun,
                        extra.args = list(average = "macro"))

get_type_k <- function(two_embeds, k) {
  # two_embeds - list of embedding matrices from Matlab
  # k - objective type
  # return type k embedding from list of two properly named embeddings
  # This is what happens when you don't have the right data structures!!!!
  
  two_idx <- grep("typeII", names(two_embeds))
  if (length(two_idx) == 0) stop("invalid matrix names")
  if (!(k %in% c(1, 2))) stop("k must be 1 or 2")
  if (k == 2) return(two_embeds[[two_idx]])
  else return(two_embeds[[which(c(1, 2) != two_idx)]])
}

as_embed_df <- function(embed_mat, label) {
  # convert matrix to embedding tibble with multiclass label
  as_data_frame(embed_mat) %>% 
    cbind(label) %>%
    select(-V1)
}

as_embed_ml_df <- function(embed_df) {
  # convert multiclass labeled embedding tibble with multilabel indicators
  nlabel <- nlevels(embed_df$label)
  embed_ml_df <- embed_df %>% 
    mutate(i = TRUE) %>% 
    spread(label, i, fill = FALSE)
  return(embed_ml_df)
}

repeat_resampler <- function(tasks, descs, learner, attempts, verbose = TRUE) {
  # resampling evaluation on all tasks and descs, trying again when one class 
  #   errors arise
  # tasks - list of tasks
  # descs - list of resampler descs
  # learner - 
  # attempts - max number of tries
  # verbose - if true say what you're doing
  
  printer <- function(task, desc)
    if (verbose) { print(task); print(desc) }
  
  mat_names <- list(names(tasks), sapply(descs, function(x) x$split))
  micro_f1_tbl <- matrix(nrow = length(tasks), ncol = length(descs), 
                         dimnames = mat_names)
  macro_f1_tbl <- matrix(nrow = length(tasks), ncol = length(descs),
                         dimnames = mat_names)
  for (d in 1:length(descs)) {
    for (k in 1:length(tasks)) {
      printer(names(tasks)[k], descs[[d]])
      a <- 0
      res <- NULL
      while (is.null(res) && a < attempts ) {
        a <- a + 1
        try(
          res <- resample(learner = learner, task = tasks[[k]], resampling = descs[[d]], 
                          show.info = FALSE, measures = list(micro.f1, macro.f1)) 
        )
      }
      
      micro_f1_tbl[k, d] <- res$aggr["micro.f1.test.mean"]
      macro_f1_tbl[k, d] <- res$aggr["macro.f1.test.mean"]
      # resamps <- c(resamps, res)
    }
  }
  return(list(micro_f1 = micro_f1_tbl, macro_f1 = macro_f1_tbl))
}