library(ProNet)

data(human)

genesets <- list.files("genesets", full.names = TRUE)
geneset_data <- genesets %>%
  map(read_csv) %>%
  map(~ mutate(., label = colnames(.)[1])) %>%
  Map(function(x) {
    colnames(x)[1] <- "gene"
    return(x[-1, ])
  }, .) %>%
  bind_rows()

ppi_data <- left_join(human, geneset_data, 
                      by = c("Official.Symbol.Interactor.A" = "gene")) %>%
  rename(label_A = label) %>%
  left_join(geneset_data,
            by = c("Official.Symbol.Interactor.B" = "gene")) %>%
  rename(label_B = label) 
  