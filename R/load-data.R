load_statements <- function(data_lang) {
  if (data_lang == "en") {
    path <- "data/statements_en.rda"
    load(path)
    name <- load(path)
    out <- get(name)
  } else {
    path <- "data/statements_fr.rda"
    load(path)
    name <- load(path)
    out <- get(name)
  }
  return(out)
}

load_statement_tags <- function(data_lang) {
  if (data_lang == "en") {
    path <- "data/statement_tags_en.rda"
    load(path)
    name <- load(path)
    out <- get(name)
  } else {
    path <- "data/statement_tags_fr.rda"
    load(path)
    name <- load(path)
    out <- get(name)
  }
  return(out)
}
