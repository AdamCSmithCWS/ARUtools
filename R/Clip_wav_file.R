#' Clip wave to length from given start time in recording
#'
#' @param seg
#' @param length_clip
#' @param out_dir
#' @param sub_dir_col
#' @param filename_col
#' @param warn
#'
#' @return
#' @export
#'
#' @examples
format_clip_wave <- function(seg, length_clip, out_dir, sub_dir_col, filename_col, warn =T){
  if(nrow(seg)>1){rlang::abort(c("seg is of length greater than 1",
                               "x" = "Currently can only process one file at a time",
                               "i" = "Use purrr::map or lapply to iterate along rows for multiple files.
                               Future developments will fix this."))}
  outfile <- glue::glue("{out_dir}/{seg[[sub_dir_col]]}/{seg[[filename_col]]}.wav")
  if(isTRUE(warn) & isTRUE(interactive())) {
    yn <- menu(c("Yes", "No"),
               title = glue::glue("Current function will copy {seg$filename} to {outfile}.
               This will overwrite any existing file there.\n
                                Do you want to continue?"))
    if(yn!=1) return("Function cancelled")
  }

  l <- get_wav_length(seg$filename, return_numeric = T)

  if(length_clip<= l){

    file.copy(from = seg$filename,
              to =  outfile)
  } else{
    in_wav <-
      tuneR::readWave(
        seg$filename, from = seg$StartTime,
        to = seg$StartTime + length_clip,
        units = 'seconds')
    tuneR::writeWave(in_wav,
                     outfile)
    return(TRUE)
  }
}


#' Get the length of a recording in seconds
#'
#' @param file Path to wave file
#'
#' @return length of recording in seconds
#' @export
#'
#' @examples
#' f <- tempfile()
#' wav <- download.file("https://www2.cs.uic.edu/~i101/SoundFiles/StarWars3.wav", destfile = f)
#' get_wav_length(f)
get_wav_length <- function(file, return_numeric= F){
  audio <- tuneR::readWave(file, header = T)
  if(return_numeric) return( round(audio$samples / audio$sample.rate, 2))
  else return(glue::glue("{round(audio$samples / audio$sample.rate, 2)} seconds"))
}
