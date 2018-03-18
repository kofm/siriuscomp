getObservations <- function(file, type = NULL) {
  if (type == "sqmat") {
    obs <-
    read_delim(file,
               delim = "\t",
               skip = 1) %>%
    slice(-1) %>%
    mutate(daysto_anth = as.integer(as.Date(ZC65_Anthesis) - as.Date(`Sowing date`)))
  }
  return(obs)
}
