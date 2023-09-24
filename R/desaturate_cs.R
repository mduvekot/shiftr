#' Destaturate a colour using any colour space
#'
#' @param col hex colour
#' @param cs coulur space
#'
#' @return hex colour
#' @export
#'
#' @examples
#' desaturate_cs()

desaturate_cs <- function(col = "#fedcba", cs = "oklch") {
  print(paste("using colour space:", cs))
  decoded_col = farver::decode_colour(col, to = cs)
  print(decoded_col)
  str_len = stringr::str_length(cs)
  if (cs == "oklch") {M = matrix(c(l = decoded_col[, 'l'],c = 0, h = 0), ncol = 3)}
  else if (cs == "oklab") {M = matrix(c(l = decoded_col[, 'l'],a = 0, b = 0), ncol = 3)}
  else if (cs == "lab")   {M = matrix(c(l = decoded_col[, 'l'],a = 0, b = 0), ncol = 3)}
  else if (cs == "lch")   {M = matrix(c(l = decoded_col[, 'l'],c = 0, h = 0), ncol = 3)}
  else if (cs == "hsl")   {M =matrix(c(h = 0, s = 0, l = decoded_col[, 'l']), ncol = 3)}
  else if (cs == "hsb")   {M =matrix(c(h = 0, s = 0, b = decoded_col[, 'b']), ncol = 3)}
  else if (cs == "lch")   {M = matrix(c(l = decoded_col[, 'l'], c = 0, h = 0), ncol = 3)}
  else if (cs == "hcl")   {M = matrix(c(h = 0, c = 0, l = decoded_col[, 'l']), ncol = 3)}
  else  {M = matrix(c(l = 0.5, c = 0, h= 0), ncol = 3)}

  new_col = M
  result <- farver::encode_colour(new_col, from = cs, white = "D65")
  return(result)
  }

desaturate_cs_v = Vectorize(desaturate_cs)


