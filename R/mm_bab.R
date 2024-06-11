#' Create split buffer around a line (one polygon for right and left side respectively)
#'
#' Requires terra package
#' Takes SpatVector (profile line). Output is a SpatVector. This is used within mm_f(), to choose (profile-)orographic right of left side respectively.
#' Output is a SpatVector with two polygons, one on the left, and one on the right side of the given line.
#' @param profile SpatVector. A Line, around which the buffer should be created.
#' @param width Numeric. Width of the buffer.
#' @param s Numeric. Either 1 (buffer for side A), 2 (side B), or 3 (both sides).
#' @return SpatVector
#' @examples 
#' # create double sided buffer;
#' mm_bab(x = "notInPackage/testdata/profile.gpkg", width = 3000, side = 3);
#' 
#' @export
mm_bab = function(profile = pro$line, width = NA, s = stds$orographicSide){
  if (!is.na(width)) {} else if (names(pro$line)[1] %in% "id_chr") { width = para$p360$radius } else { width = para$sr } # if a width is given, use it, if a 360 is provided (identified via "id_chr" field), use 360 radius, else use longProfile radius from params.
  oroA = terra::buffer(x = profile, capstyle = "flat", singlesided = T, width = width, joinstyle = "round")
  fullbuffer = buffer(x = profile, width = width, capstyle = "flat", joinstyle = "round")
  oroB = terra::erase(fullbuffer, oroA)
  if (s == 3) { return(fullbuffer) } else
    if (s == 1) { return(oroA) } else
      if (s == 2) { return(oroB) }
}
