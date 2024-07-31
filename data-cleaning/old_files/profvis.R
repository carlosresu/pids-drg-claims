source(here("data-cleaning", "r_scripts", "libraries.R"))

# profvis_wrapper <- function(to_profvis, func_name, func_args, fname) {
#   if (to_profvis) {
#     p <- profvis({
#       do.call(func_name, func_args)
#     })
#     htmlwidgets::saveWidget(
#       p,
#       file = here("git-ignored-files", "profvis", paste0(fname)),
#       selfcontained = TRUE
#     )
#   } else {
#     func_name(func_args)
#   }
# }
