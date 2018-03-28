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

#' Edit .sqvar or .sqpar XML files
#'
#' This function change the specified parameter in a XML .sqvar or .sqpar file
#' for a specific variety/specie
#'
#' @param file A valid .sqvar or .sqpar Sirius Quality file.
#' @param varspecie The name of the target specie/variety.
#' @param parameter The name of the parameter to change.
#' @param value Value to set the parameter.
#' @return The name of the edited file.
#' @import XML
#' @export
sqchangeParam <- function(file, varspecie, parameter, value) {
  node <- xmlInternalTreeParse(file)
  sqeditParam(node, varspecie = varspecie, parameter = parameter, value = value)
  saveXML(node, file)
}

#' @export
sqgetVarSpecie <- function(file) {
	xmlInternalTreeParse(file) %>%
	xpathApply("//CropParameterItem", xmlAttrs) %>%
	unlist %>%
	unname %>%
	return
}

#' Duplicate an existing variety/specie in XML files
#'
#' This function duplicates an existing variety/specie with a new name in the supplied .sqvar/.sqpar file
#'
#' @param file Target .sqpar or .sqvar file
#' @param varspecie Variety/Specie to duplicate
#' @param new Name of the new Variety/Specie
#' @export
#' @import XML
sqduplVariety <- function(file, varspecie, new) {

  doc <- xmlParse(file)
  node <- xpathApply(doc, paste0("//CropParameterItem[@name = '", varspecie,"']"), xmlClone)
  xmlAttrs(node[[1]]) <- c(name = new)
  xpathApply(doc, "//ItemsArray", addChildren, node)
  saveXML(doc, file)

}

####################################### RUN FILES #######################################
#' Get all the run items in Run File
#'
#' Funciton to retrieve all the Run Items present in the specified Sirius Quality Run file (.sqrun)
#'
#' @param sqrun Target .sqrun file
#' @return A character vector containing all the present Run Items
#' @import XML
#' @export
sqgetRunItems <- function(sqrun) {
  xmlInternalTreeParse(sqrun) %>%
    xpathApply(.,"//RunItem[@name]", xmlAttrs) %>%
    unlist %>%
    unname %>%
    return
}

#' @export
sqgetRunManag <- function(sqrun, runitem, variety = NULL) {
  if (is.null(variety)) {
  xmlInternalTreeParse(sqrun) %>%
    xpathApply(.,paste0("//RunItem[@name='", runitem,"']/Multi/MultiRunsArray/MultiRunItem/ManagementItem/text()"), xmlValue) %>%
    unlist %>%
    unique %>%
    return
  } else {
    xmlInternalTreeParse(sqrun) %>%
      xpathApply(.,paste0("//RunItem[@name='",
                          runitem,
                          "']/Multi/MultiRunsArray/MultiRunItem/VarietyItem[text()='",
                          variety,
                          "']/../ManagementItem/text()"), xmlValue) %>%
      unlist %>%
      unique %>%
      return
  }
}

#' Get all the Observations Items in the observation file
#'
#' Function to retrieve all the Observations Items present in the Observation File
#'
#' @param sqpro Target Project file (.sqpro)
#' @return A character vector containing all the present Observations Items
#' @import XML
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

#' @export
sqaddMultiRunItem <- function(sqrun, runitem, management, parameter, option, site, soil, variety, experiment) {

  doc <- XML::xmlParse(sqrun)

    for (i in 1:length(management)) {
      new <- XML::addChildren(XML::newXMLNode("MultiRunItem"),
                              XML::newXMLNode("ManagementItem", management[i]),
                              XML::newXMLNode("ParameterItem", parameter),
                              XML::newXMLNode("RunOptionItem", option),
                              XML::newXMLNode("SiteItem", site),
                              XML::newXMLNode("SoilItem", soil),
                              XML::newXMLNode("VarietyItem", variety),
                              XML::newXMLNode("ExperimentItem", experiment))
      XML::xpathApply(doc,
                      paste0("//RunItem[@name='", runitem,"']/Multi/MultiRunsArray"),
                      XML::addChildren,
                      new)
    }

  XML::saveXML(doc, file = sqrun)
}

