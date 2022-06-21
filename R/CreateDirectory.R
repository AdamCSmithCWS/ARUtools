#' Create directory structure for ARU folders
#'
#' @param hexagons Character vector of hexagons or clusters
#' @param units    Character vector of ARU unit ids. Should include the hexagon in the name
#' @param base_dir Base directory to build directory structure from.
#'
#' @return Does not return anything if run. If cancelled returns string notification.
#' @export
#'
create_directory_structure <- function(hexagons, units, base_dir){
  if(!is_interactive()) abort("This function must be run in interactive session.")
  yn <- menu(c("Yes", "No"),
             title = glue::glue("Current function will create {length(hexagons)} directories in {base_dir}\n
                                Do you want to continue?"))
  if(yn!=1) return("Function cancelled")
  for(i in hexagons){
    dir.create(glue::glue("{base_dir}/{i}"))
    sites <- units[grep(i, units)]
    for(j in sites)  dir.create(glue::glue("{base_dir}/{i}/{j}"))
  }

  if (rlang::is_installed("sessioninfo")) {
  readr::write_lines(sessioninfo::session_info(), glue::glue("{base_dir}/session_info.md"))
  } else{ readr::write_lines(sessionInfo(), glue::glue("{base_dir}/session_info.md"))}
  # print(sessioninfo::session_info())
  # sink()

}
