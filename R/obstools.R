#' @importFrom magrittr "%>%"
#' @export
getObservations <- function(file, type = NULL) {
  # Lines to skip before the one starting with the word "Management"
  skip.lines <- which(startsWith(x = readLines(file, n = 30), prefix = "Management")) - 1

  if (type == "sqmat") {
    obs <-
    sqobs_importer(file) %>%
      dplyr::mutate(daysto_anth = as.integer(as.Date(anthesis) - as.Date(sow)))

  } else if (type == "sqoln") {

    obs <-
      sqobs_importer(file)

  } else {

    stop("Invalid observation type")

  }

  return(obs)
}

#' @export
sqobs_importer <- function(file) {

  skip.lines <- which(startsWith(x = readLines(file, n = 30), prefix = "Management")) - 1

  data <-
    read.table(file,
               sep = "\t",
               header = T,
               skip = skip.lines,
               stringsAsFactors = F, na.strings = "-999", as.is = T,strip.white = T) %>%
    dplyr::slice(-1) %>%
    dplyr::select(-dplyr::ends_with(".1")) %>%
    sqtranslate_colnames() %>%
    dplyr::na_if(-999)

  has_data <- function(x) { sum(!is.na(x)) > 0 }

  data %>% dplyr::select_if(has_data) %>% return
}

#' @export
sqobsdaily_importer <- function(path) {

  # Import daily outputs

  # Invoke dedicated shell script to convert .sqbrs output files
  #(Remove comments and change character encoding)

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
}

#' @export
locateObservationData <- function(observation) {
  param_location <- list(daysto_anth = "sqmat",
                         fln = "sqmat")

  return(paste0("obs.",param_location[[observation]]))
}
