
#' Create spectrogram from wave file
#'
#'Functions from Sam Hache to create spectrograms
#'
#' Create spectrogram and save it wherever you want
#' Input must be file with its directory (file.path)
#' Defult will save in Spectrogram folder in the current working directory otherwise specify location (out.path)
#' Size specifies the x and y size of the output in pixels must be list
#' Duration sets the duration of spectrogram default is 3 min and is listed in seconds
#'
#' @param file.path Input must be file with its directory (file.path)
#' @param out.path Defult will save in Spectrogram folder in the current working directory otherwise specify location (out.path)
#' @param out.app
#' @param size
#' @param duration
#' @param sox.file.path
#'
#' @return
#' @export
#'
soxSpectrogram <- function(file.path, out.path = "Spectrograms",
                            out.app = "test",
                            size = list(x = 2000, y = 1000), duration = list(start = 0, end = 180), sox.file.path){
  if ((basename(sox.file.path)!="sox.exe")) stop("You need to specify the location of Sox.exe")
  if(!file.exists(out.path)) dir.create(out.path)
  fn <- paste0(out.app, "_",unlist(strsplit(basename(file.path), split = "[.]wav")) )
  system(paste0("\"",sox.file.path,"\" \"",file.path,"\" -n ","trim ",duration$start," ",duration$end, " spectrogram", " -x ", size$x, " -Y ", size$y, " -o \"", file.path(out.path,fn),".png\""))
}
