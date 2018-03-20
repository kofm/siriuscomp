#' @export
getSimulation <- function(path, type) {
  if (type == "sum")
    {
    sum <-
      read.table(path,
               sep = "\t",
               header = T,
               skip = 9,
               fill = T,
               blank.lines.skip = T,
               stringsAsFactors = F,
               fileEncoding = "UCS-2LE") %>%
      sqtranslate_colnames()

    return(sum)

  } else if (type == "daily") {
    files <- list.files(path = path, pattern = "\\.sqsro$", recursive = F)

    first_file <- TRUE

    for (file in files) {

      filename <- gsub(".sqsro","",file)

      dat <-
        read.table(paste0(path,file),
                   sep = "\t",
                   fileEncoding = "utf-16le",
                   skip = 6,
                   header = T,
                   stringsAsFactors = F) %>%
        dplyr::mutate(Management = rep(substring(file,1,5),nrow(.)),
                      Variety = rep(substring(filename,nchar(filename) - 3 + 1),nrow(.))) %>%
        sqtranslate_colnames()

      if (first_file) {
        dailyout <- dat
        first_file <- FALSE
      } else {
        dailyout <- dplyr::bind_rows(dailyout,dat)
      }


    }
    return(dailyout)
  } else {
      stop("Invalid output file type")
    }
}
