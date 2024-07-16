source(here("data-cleaning", "r_scripts", "libraries.R"))

# profvis_wrapper <- function(to_profvis, fnc, fname) {
#   if (to_profvis) {
#     p <- profvis({
#       fnc()
#     })
#     htmlwidgets::saveWidget(
#       p,
#       file = here("git-ignored-files", "profvis", paste0(fname)),
#       selfcontained = TRUE
#     )
#   } else {
#     fnc()
#   }
# }
