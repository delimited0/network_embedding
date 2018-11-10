library(R.matlab)
library(tidyverse)
library(magrittr)
library(mlr)
library(reticulate)
library(knitr)
library(kableExtra)
library(formattable)

# reticulate::py_discover_config()
use_python("/Users/patrickding/anaconda3/bin/python")
sk <- reticulate::import("sklearn")

euc_norm <- function(v) {
  # euclidean norm
  sqrt(sum(v^2))
}

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
roc.auc.fun <- function(task, model, pred, feats, extra.args) {
  # scikit-learn f1 score
  sk$metrics$roc_auc_score(y_true = getPredictionTruth(pred),
                           y_score = getPredictionProbabilities(pred),
                           average = extra.args$average)
}
micro.f1.sd <- setAggregation(micro.f1, test.sd)
macro.f1.sd <- setAggregation(macro.f1, test.sd)
multilabel.acc.sd <- setAggregation(multilabel.acc, test.sd)
multilabel.tpr.sd <- setAggregation(multilabel.tpr, test.sd)
multilabel.ppv.sd <- setAggregation(multilabel.ppv, test.sd)


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

as_embed_df <- function(embed_mat, label, normalize = TRUE) {
  # convert matrix to embedding tibble with multiclass label
  # embed_mat - numeric matrix, embedding
  # label - logical matrix, labels
  # normalize - flag, normalize embedding vectors 
  embed_mat %<>% as_data_frame() %>% select(-V1)
  if (normalize)
    embed_mat <- embed_mat / apply(embed_mat, 1, function(x) euc_norm(x))
  cbind(embed_mat, label)
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
  micro_f1_sd_tbl <- matrix(nrow = length(tasks), ncol = length(descs), 
                         dimnames = mat_names)
  macro_f1_sd_tbl <- matrix(nrow = length(tasks), ncol = length(descs), 
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
                          show.info = FALSE, 
                          measures = list(micro.f1, macro.f1, micro.f1.sd, macro.f1.sd)) 
        )
      }
      
      micro_f1_tbl[k, d] <- res$aggr["micro.f1.test.mean"]
      macro_f1_tbl[k, d] <- res$aggr["macro.f1.test.mean"]
      micro_f1_sd_tbl[k, d] <- res$aggr["micro.f1.test.sd"]
      macro_f1_sd_tbl[k, d] <- res$aggr["macro.f1.test.sd"]
      # resamps <- c(resamps, res)
    }
  }
  return(list(micro_f1 = micro_f1_tbl, macro_f1 = macro_f1_tbl,
              micro_f1_sd = micro_f1_sd_tbl, macro_f1_sd = macro_f1_sd_tbl))
}

small_class_mccv <- function(label_mat, split = .5, min_case = 1) {
  # monte carlo cv for multilabel when some classes have few instances
  # sample at least 1 instance of each class before sampling rest of data
  # label_mat - logical matrix, indicating whether label present
  # split - proportion of data to use for training
  # min_case - minimum number of cases of each label needed for train split
  
  train_inds <- rep(NA, floor(length(label_mat) * split))
  sample_list <- 1:nrow(label_mat)
  
  starting_sample <- c()  
  for (k in 1:ncol(label_mat)) {
    candidates <- which(label_mat[, k])
    # if (sum(candidates) < 2) stop("Must have at least 2 instances in each class")
    starting_sample <- c(starting_sample, sample(candidates, size = min_case))
  }
  starting_sample <- unique(starting_sample)
  
  sample_list <- sample_list[-starting_sample]
  split_size <- floor(split * nrow(label_mat)) - length(starting_sample)
  if (split_size < 1) stop("Can't have than many guaranteed samples per class")
  train_ind <- c(sample(sample_list, size = split_size, replace = FALSE), 
                 starting_sample)
  test_ind <- which(!(1:nrow(label_mat) %in% train_ind))
  
  return(list(train = train_ind, tests = test_ind))
}

multilabel_resample <- function(tasks, splits, labels, learner, iters, 
                                min_case, verbose = TRUE) {
  # tasks  - list of tasks
  # splits - vector of split proportions p, 0 < p < 1
  # labels - logical matrix of multilabels
  # learner - learner object
  # iters - number of times to resample for each task, split combo
  # min_case - minimum number of cases of each label needed for training split
  
  printer <- function(task, desc)
    if (verbose) { print(task); print(desc) }
  
  micro_f1_tbl <- array(dim = c(length(tasks), length(splits), iters),
                        dimnames = list(names(tasks), 
                                        paste0("split_", splits), 
                                        paste0("iter_", 1:iters)))
  macro_f1_tbl <- array(dim = c(length(tasks), length(splits), iters),
                        dimnames = list(names(tasks), 
                                        paste0("split_", splits), 
                                        paste0("iter_", 1:iters)))
  results <- list()
  for (s in 1:length(splits)) {
    for (t in 1:length(tasks)) {
      printer(names(tasks)[t], splits[s])
      
      for (i in 1:iters) {
        mccv <- small_class_mccv(labels, splits[s], min_case)
        mod <- train(learner, tasks[[t]], subset = mccv$train)
        pred <- predict(mod, tasks[[t]], subset = mccv$test)
        res <- performance(pred, measures = list(micro.f1, macro.f1,
                                                 micro.f1.sd, macro.f1.sd))
        micro_f1_tbl[t, s, i] <- res["micro.f1"]
        macro_f1_tbl[t, s, i] <- res["macro.f1"]
        results <- append(results, res)
      }
    }
  }
  
  return(list(micro_f1 = micro_f1_tbl, macro_f1 = macro_f1_tbl))
}