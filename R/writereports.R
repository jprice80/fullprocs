#' Write Formatted Output to a *.docx File
#'
#' @description The write.report function will produce an MS Word document using the \code{\link{officer}}.
#'
#' @param object a R object from an aca analysis.
#' @param path the location to save the docx file.
#' @param tablefont Tdesignates the font to use in report creation. A list of available font families can be obtained with the  \code{\link{sys_fonts()}} command.
#' @param tablefontsize designates the font size to use in report creation.
#' @param digits he number of decimal digits to be reported in each table. If this value is not specified, but rounding was performed on the aca object, the number of rounded digits will be used. If no value is specified, 4 digits will be used by default.
#'
#' @return
#' @export
#'
#' @examples
write.report<-function(object, path, tablefont="Arial", tablefontsize=10, digits=NULL){

  c1<-class(object)[1]
  c2<-class(object)[2]

  if(c1 != "fpr"){
    stop(paste(object, "not an fpr object"))
  }

  if(c2 == "desc"){

    write_desc(object, path, tablefont=tablefont, tablefontsize=tablefontsize, digits=digits)

  } else if (c2 == "norm") {

    write_norm(object, path, plots=FALSE, tablefont=tablefont, tablefontsize=tablefontsize, digits=digits)

  } else {

    stop(paste("This object cannot be written to an MS Word report."))

  }
}
