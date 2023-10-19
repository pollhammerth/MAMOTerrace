#' Create points along a line with attributes for metering
#'
#' Requires sf and terra packages.
#' Creates points along a line with given spacing, starting at line distance = 0. The points are attributed by distance along the line. Output, spatVector, can be used for adding metering along a line in map plots.
#' Output can be used e.g. for terra::plot or plet
#' @param profile_line spatVector, line, along which points are placed.
#' @param spacing Distance between points in meters.
#' @param label logical. Standard = T. Should attributes for labelling = distance along the line, be set?
#' @param labelUnit Either "km" or "m". Standard = "km". Should label attributes be in kilometers or meters?
#' @return spatVector
#' @examples 
#' profile = data("profile.gpkg");
#' metering = mm_metering(profile_line=profile,spacing=10000,label=T,labelUnit="km");
#' @export
mm_metering = function(profile_line, spacing = 1000, label = T, labelUnit = "km") {
  
  # convert to simple feature for package sf
  sf = sf::st_as_sf(profile_line)
  
  # for correct labels, crs units must be m when doing sf::st_line_sample()
  # check and transform if necessary
  original_proj = terra::crs(profile_line, proj=T)
  unitOK = grepl("+units=m", original_proj)
  if(unitOK == FALSE) { sf = sf::st_transform(sf, 3857) }
  
  # create metering
  l = as.numeric( sf::st_length(sf) ) # get profile length
  metr = sf::st_line_sample( sf, sample = seq(0, 1, by = 1/l*spacing) )
  metr = sf::st_cast(metr,"POINT") # convert to POINT instead of MULTIPOINT
  
  # transform metering result back to original crs
  if(unitOK == FALSE) {
    original_EPSG = sf::st_crs(profile_line)$epsg # get original epsg
    metr = sf::st_transform(metr, original_EPSG)
  }
  
  # convert to spatVector
  profile_metering = terra::vect(metr)
  
  #### add field for labels
  if (label) {
    n = seq(0, 1, by = 1/l*spacing)
    label = n * l / if (labelUnit == "km") {1000} else if (labelUnit == "m") {1}
    terra::values(profile_metering) = as.data.frame(label)
  }
  
  return(profile_metering)
}
#terra::plot(profile_metering, cex = 3, col = "green")
#terra::text(profile_metering, labels = profile_metering$label, halo = T, cex = 1)
