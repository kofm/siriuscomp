#' @export
getSimulation <- function(file, type = NULL) {
  read.table(file,
             sep = "\t",
             header = T,
             skip = 9,
             fill = T,
             blank.lines.skip = T,
             stringsAsFactors = F,
             fileEncoding = "UCS-2LE") %>%
    sqtranslate_colnames()
}
