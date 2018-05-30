#' @export
getSimulation <- function(path, type) {
  if (type == "seasonal")
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
      sqtranslate_colnames() %>%
      dplyr::mutate(daysto_anth = as.numeric(as.Date(anthesis) - as.Date(sow)))

    return(sum)

  } else if (type == "daily") {
    files <- list.files(path = dirname(path), pattern = "\\.sqsro$", recursive = F)

    first_file <- TRUE

    for (file in files) {
      # print(file)
      filename <- gsub(".sqsro","",file)

      dat <-
          read.table(paste0(dirname(path),"/",file),
                     sep = "\t",
                     fileEncoding = "utf-16",
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

#' @export
locatePredictionData <- function(variable) {
  param_location <- list(daysto_anth = "seasonal",
                         fln = "seasonal",
                         eln = "daily")

  return(param_location[[variable]])
}
