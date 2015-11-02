#' imagine is the only function in the package. It's a wrapper the convert/gm convert commands respectively of ImageMagick and GraphicsMagick
#'
#' This function allows you to:
#' - convert the format of an image
#' - rotate an image
#' - crop an image from any angle
#' - resize an image
#' - create a GIF animation
#'
#' @param filename either a character vector of file names, or a single string
#'   containing wildcards (e.g. \file{img_frame*.png}) to create an animation
#' @param output the file name of the output with the desired extension when converting a file or
#'   \code{gif} for an animation. If the output filename only specifies the extension like \code{.bmp}  the output file name will be the current timestamp like \code{20151011892319.bmp}
#'
#'   If \code{ani.options('autobrowse') == TRUE}, this function will also try to
#'   open the output automatically.
#' @note If \code{files} is a character vector, please make sure the order of
#'   filenames is correct! The first animation frame will be \code{files[1]},
#'   the second frame will be \code{files[2]}, ...
#'
#'   For animations, both ImageMagick and GraphicsMagick may have a limit on the number of
#'   images to be converted. It is a known issue that this function can fail
#'   with more than (approximately) 9000 images.
#'
#'   Most Windows users do not have read the boring notes below after they have
#'   installed ImageMagick or GraphicsMagick. For the rest of Windows users:
#'
#'   \describe{
#'
#'   \item{\strong{ImageMagick users}}{Please install ImageMagick from
#'   \url{http://www.imagemagick.org}, and make sure the the path to
#'   \command{convert.exe} is in your \code{'PATH'} variable, in which case the
#'   command \command{convert} can be called without the full path.  Windows
#'   users are often very confused about the ImageMagick and \code{'PATH'}
#'   setting, so I'll try to search for ImageMagick in the Registry Hive by
#'   \code{readRegistry('SOFTWARE\ImageMagick\Current')$BinPath}, thus you might
#'   not really need to modify your \code{'PATH'} variable.
#'
#'   For Windows users who have installed LyX, I will also try to find the
#'   \command{convert} utility in the LyX installation directory, so they do not
#'   really have to install ImageMagick if LyX exists in their system (of
#'   course, the LyX should be installed with ImageMagick).
#'
#'   Once the \command{convert} utility is found, the animation option
#'   \code{'convert'} will be set (\code{ani.options(convert =
#'   'path/to/convert.exe')}); this can save time for searching for
#'   \command{convert} in the operating system next time.  }
#'
#'   \item{\strong{GraphicsMagick users}}{During the installation of
#'   GraphicsMagick, you will be asked if you allow it to change the PATH
#'   variable; please do check the option.  }
#'
#'   }
#'
#'   A reported problem is \code{cmd.fun = shell} might not work under Windows
#'   but \code{cmd.fun = system} works fine. Try this option in case of
#'   failures.
#' @author Olalekan Houdan ABOU BAKAR
#' @family utilities
#' @references ImageMagick: \url{http://www.imagemagick.org/script/convert.php}
#'   GraphicsMagick: \url{http://www.graphicsmagick.org}
#' @export
#' @example
#' #convert a PNG to a BMP
#' imagine("test.png", "test.bmp")
#'
#' #convert a JPG to something, because you feel uninspired about the filename
#' imagine("test.jpg", ".png")
"imagine"
imagine <- function(filename, output="", list.options=NULL){

  convert_path_img <- as.vector(Sys.which("convert"))

  convert_path_gm <- as.vector(Sys.which("gm"))

  if(convert_path_img == "" && convert_path_gm == ""){
    stop("You need to install ImageMagick or GraphicsMagick / put their binaries in your system PATH variable for [imagine] to work")
  } else if(convert_path_img != ""){
  convert_path <- convert_path_img
  } else if(convert_path_gm != ""){
    convert_path <- convert_path_gm
  }

  #file_formats <- c("tif","tiff","pgm","ppm","png","pnm","gif","jpg","jpeg","bmp", "svg")

  file_formats <- c("tif","tiff","png", "gif","jpg","jpeg","bmp")

  if(is.null(list.options$width))
    width <- "100%"
  else
    width <- list.options$width

  if(is.null(list.options$height))
    height <- ""
  else
    height <- list.options$height

  output_dim <- c(width, height)

  if((grep("*", filename) > 0) || (length(filename) >1)){

    if(list.options$operation != "animation"){
      stop("The filename arguments says that you want to create an animation, so set list.options$operation to animation")
    }

  } else if(!file.exists(filename)){

  tmp <- paste0(getwd(), filename)
  if(!file.exists(tmp)){

    stop("Input file", filename, "not found\n")

  } else {
    filename <- tmp
  }

}

  fileparts <- strsplit(filename,"\\.")[[1]]
  ext <- tolower(fileparts[length(fileparts)])

  output_ext <- ""
  if( var.exists(list.options$extension) )
    output_ext <- list.options$extension
  else if(output != "") {
    out_fileparts <- strsplit(output, "\\.")[[1]]
    output_ext <- tolower(out_fileparts[length(out_fileparts)])
  } else if((output == "") && (!var.exists(list.options$extension))){
    stop("You must at least specify the extension(format) in which you want to convert this file")
  }

 file_output <- gsub(paste0(".", output_ext), "", output)

  if ( (!(ext %in% file_formats)) || (!(output_ext %in% file_formats))  ) {

    stop("Only the following file formats are supported: ", paste(file_formats, collapse =", "))

  }

  if(( file_output =="" ) && ( !var.exists(list.options$output) )){
      output <- format(Sys.time(), "%Y%m%d%H%M%S")
      output <- paste0(output, ".", output_ext)

  } else {

    if( (var.exists(list.options$output)) && (var.exists(list.options$extension)) ){
      output <- paste0(list.options$ouput, ".", list.options$extension)
    }
  }


  option.convert <- ""

  if(!var.exists(list.options$operation))
  list.options$operation <- "auto"

  if(list.options$operation=="resize") {

    if(var.exists(list.options$ratio)){

      ratio <- as.double(ratio)
      if(ration <=1)
      ratio <- ratio * 100

      resize_opt <- ratio
    } else if( (var.exists(output_dim[1])) || (var.exists(output_dim[2])) ) {

      resize_opt <- paste(output_dim, collapse = "x")

    }

    option.convert <- paste0(" -resize ", resize_opt)


  } else if(list.options$operation=="crop"){

    if(!var.exists(list.options$crop.gravity))
      list.options$crop.gravity <- "Center"

    if(!var.exists(list.options$crop.offset))
      list.options$crop.offset <- c(0, 0)
    else if(length(list.options$crop.offset) !=2)
      stop("list.options$crop.offset must only have a integer vector of length 2")

    offset <- "\\!"
    if(  (list.options$crop.offset[1] != -1) && (list.options$crop.offset[2] != -1) )
    offset <- paste0("+", paste(list.options$crop.offset, collapse = "+"))


    option.convert <- paste0("-gravity ", list.options$crop.gravity, " -crop ", paste(output_dim, collapse = "x"), offset)

  } else if(list.options$operation=="flip" || list.options$operation =="flop"){

    rotation_angle <- 90
  if(var.exists(list.options$rotation))
    rotation_angle <- list.options$rotation

option.convert <- paste0("-", list.options$operation, " -rotate ", rotation_angle)

  }
  else if(list.options$operation=="animation"){
    #convert   -delay 20   -loop 0   sphere*.gif   animatespheres.gif

    if(!var.exists(list.options$delay))
      list.options$delay <- 20

    if(!var.exists(list.options$loop))
      list.options$loop <- 0

    if(length(filename) > 1)
      filename <- paste(filename, collapse = " ")

    option.convert <- paste("-delay", list.options$delay, "-loop", list.options$loop)

  }

  else { #default operation

    if( (var.exists(output_dim[1])) || (var.exists(output_dim[2])) )
      option.convert <- paste(" -size ", paste(output_dim, collapse = "x"))
  }

  cmd <- paste(convert_path, shQuote(filename), option.convert, shQuote(output))

  print(cmd)

  if(.Platform$OS.type == "windows")
    shell(cmd)
  else
    system(cmd)

  invisible(NULL)
}

has_valid_ext <- function (filename){
  needle <- paste(".((", paste(file_formats, collapse=")|("), "))", sep="")
  if(gregexpr(needle, filename, ignore.case=T, perl=T)[[1]] >=0)
    return(T)
  else
    return(F)
}



var.exists <- function (x){
  return(!is.null(x))
}