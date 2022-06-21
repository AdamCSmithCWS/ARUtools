#' Play random track
#'
#' Will play a random track from a wave file in a given folder.
#'
#' @param base_folder Base folder path from which to search from.
#'
#' @return Will not return anything. It does open your media player
#' @export
#'
play_random_track <- function(base_folder, random_seed = NULL){
  if(!interactive()) abort("This program does not work outside of an interactive seesion")
  if (!requireNamespace("pkg", quietly = TRUE)) {
    stop(
      "Package \"tuneR\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if(is_null(random_seed)) random_seed <- Sys.time()
  list_waves <- list.files(base_folder, pattern = ".wav", recursive = T, full.names = T)
  if(length(list_waves)==0) abort("No wav files found. Check path")
  withr::with_seed(random_seed,
                   {wav_ <- sample(list_waves, 1)})
  t <- Sys.time()
    v <- job::job({tuneR::play(wav_)}, import = c(wav_), packages = c("tuneR"))
    Sys.sleep(32)
    message(glue::glue("Playing {wav_}. Enjoy!"))
    if(Sys.time()-t > 30) rstudioapi::jobRemove(v)
}
