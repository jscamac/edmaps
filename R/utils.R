# Expand sequences specified with colon (e.g. 5:10) or hyphen (e.g. 5-10) 
# notation. 
expand_range <- function(x) {
  lapply(x, function(y) {
    sort(unlist(lapply(gsub('(\\d+)[-:](\\d+)', 'seq.int(\\1, \\2)', y), 
                       function(z) eval(parse(text=z)))))
    
  })
}
