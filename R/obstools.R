#' @importFrom magrittr "%>%"
#' @export
getObservations <- function(file, type = NULL) {
  # Lines to skip before the one starting with the word "Management"
  skip.lines <- which(startsWith(x = readLines(file, n = 30), prefix = "Management")) - 1

  if (type == "sqmat") {
    obs <-
    read.table(file,
               sep = "\t",
               header = T,
               skip = skip.lines,
               stringsAsFactors = F) %>%
      dplyr::slice(-1) %>%
      dplyr::select(-dplyr::ends_with(".1")) %>%
      sqtranslate_colnames() %>%
      dplyr::mutate(daysto_anth = as.integer(as.Date(anthesis) - as.Date(sow)))
  }
  return(obs)
}
