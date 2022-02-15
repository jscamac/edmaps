#' Expand sequences
#'
#' Expand sequence specified with colon or hyphen.
#' notation.
#'
#' @param x A character vector of shorthand sequences described by colons (e.g.
#'   `c('1:3', '5:10')` or hyphens (e.g. `c('1-3', '5-10'`).
#' @return Expanded integer sequences.
#' @keywords internal
expand_range <- function(x) {
  lapply(x, function(y) {
    sort(unlist(lapply(gsub('(\\d+)[-:](\\d+)', 'seq.int(\\1, \\2)', y),
                       function(z) eval(parse(text=z)))))
  })
}
