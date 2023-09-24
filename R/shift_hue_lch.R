# usethis::use_package("farver")
# usethis::use_package("prismatic")
# usethis::use_package("stringr")
# usethis::use_package("forcats")
# usethis::use_package("tibble")

#' Shift hue in lch colourspace
#'
#' @param col hex colour
#' @param shift amount to shift between 0 and 360
#' @param cs which colour space to use
#'
#' @return hex colour
#' @export
#'
#' @examples
#' shift_hue_cs()


shift_hue_cs <- function(col = "#fdb975", shift = 30, cs = "oklch"){
  decoded_col = farver::decode_colour(col, to = cs)
  if (cs %in% c("hsl", "hsb", "hsv", "hcl")) {
    M = matrix(
      c(
        (decoded_col[, substring(cs, 1,1)] + shift) %% 360,
        decoded_col[, substring(cs, 2,2)],
        decoded_col[, substring(cs, 3,3)]
      ), ncol = 3
    )
  }
  else if (cs %in% c("lch", "oklch")){
    M = matrix(
      c(
        decoded_col[, substring(cs, stringr::str_length(cs)-2,stringr::str_length(cs)-2)],
        decoded_col[, substring(cs, stringr::str_length(cs)-1,stringr::str_length(cs)-1)],
        (decoded_col[, substring(cs, stringr::str_length(cs),stringr::str_length(cs))]+ shift) %% 360
      ), ncol = 3
    )
  }
  new_col = M
  result <- farver::encode_colour(
    new_col,
    from = cs,
    white = "D65"
  )
  return (result)
}
shift_hue_cs_v = Vectorize(shift_hue_cs)
