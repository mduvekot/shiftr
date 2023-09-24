#' Interpolate between two hex colours
#'
#' @param colour1 colour to interpolate from
#' @param colour2 colour to interpolate to
#' @param amt amount 0, 1 0 is all colour 1, 1 is all colour 2
#' @param cs colourspace to use
#'
#' @return hex colour
#' @export
#'
#' @examples
interpolate_cs <- function(
  colour1 = "#ff7f23",
  colour2 = "#237f00",
  amt = 0.5,
  cs = "oklch") {
  c1 = farver::decode_colour(colour1, to = cs)
  c2 = farver::decode_colour(colour2, to = cs)
  bld_ok = (amt * c2 + (1 - amt) * c1)
  bld = farver::encode_colour(bld_ok, from = cs)
  return (bld)
}

