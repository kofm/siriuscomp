#' Edit .sqvar or .sqpar XML files
#'
#' This function returns a XML .sqvar or .sqpar file
#' with the specified parameter changed for a variety/specie
#' with the provided value
#'
#' @param node Object of class "XMLInternalDocument" or "XMLAbstractDocument" obtained from sqpar or sqvar file
#' @param varspecie The name of the target specie/variety
#' @param parameter The name of the parameter to change
#' @param value Value to set the parameter
#' @return Changed XML tree
#' @import XML
#' @export
sqeditParam <- function(node, varspecie = NULL, parameter, value) {

  if (is.null(varspecie)) {
  path <- paste0("//CropParameterItem/*/*/*/string[text()='",
                 parameter,
                 "']/../../Value/double")
  } else {
  path <- paste0("//CropParameterItem[@name='",
                 varspecie,
                 "']/ParamValue/Item/Key/string[text()='",
                 parameter,
                 "']/../../Value/double")
  }
  if (!is.null(getNodeSet(node,path))) {
    xpathSApply(node,
                path,
                `xmlValue<-`,
                value = value)
    return(node)
  }
  else {
    stop("Variety or specie or parameter not found.")
  }
}

#' @export
sqgetRunItems <- function(sqrun) {
  xmlInternalTreeParse(sqrun) %>%
    xpathApply(.,"//RunItem[@name]", xmlAttrs) %>%
    unlist %>%
    unname %>%
    return
}

#' @export
sqgetObsItems <- function(sqpro) {
  xmlInternalTreeParse(sqpro) %>%
    xpathApply(., "//ObservationFileName[text()]", xmlValue) %>%
    .[[1]] %>%
    gsub('\\\\', '/', .) %>%
    tools::file_path_as_absolute() %>%
    xmlInternalTreeParse() %>%
    xpathApply(., paste0("//ObservationItem[@name]"), xmlAttrs) %>%
    unlist %>%
    unique %>%
    return
}

#' @export
sqgetRunManag <- function(sqrun, runitem) {
  xmlInternalTreeParse(sqrun) %>%
    xpathApply(.,paste0("//RunItem[@name='", runitem,"']/Multi/MultiRunsArray/MultiRunItem/ManagementItem/text()"), xmlValue) %>%
    unlist %>%
    unique %>%
    return
}

#' @export
sqchangeParam <- function(file, varspecie, parameter, value) {
  node <- xmlInternalTreeParse(file)
  sqeditParam(node, varspecie = varspecie, parameter = parameter, value = value)
  saveXML(node, file)
}

#' @export
sqgetObsFile <- function(sqpro, runitem, type) {

  if (type == "sqmat") xml.tag <- "PhenologyObservationFile"
  else if (type == "sqoln") xml.tag <- "HaunIndexObservationFile"

  obs.file <-
    xmlInternalTreeParse(sqpro) %>%
    xpathApply(., "//ObservationFileName[text()]", xmlValue) %>%
    .[[1]] %>%
    gsub('\\\\', '/', .) %>%
    tools::file_path_as_absolute() %>%
    xmlInternalTreeParse() %>%
    xpathApply(., paste0("//ObservationItem[@name='", runitem,"']/",xml.tag), xmlValue) %>%
    .[[1]] %>%
    gsub('\\\\', '/', .) %>%
    tools::file_path_as_absolute()

  return(obs.file)
}


