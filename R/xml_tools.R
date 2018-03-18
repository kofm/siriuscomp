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
sqgetRunManag <- function(sqrun, runitem) {
  xmlInternalTreeParse(sqrun) %>%
    xpathApply(.,paste0("//RunItem[@name='", runitem,"']/Multi/MultiRunsArray/MultiRunItem/ManagementItem/text()"), xmlValue) %>%
    unlist %>%
    unique %>%
    return
}


