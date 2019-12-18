######################################## Summary Function #####################################

#' @export
summary.fpr<-function(object, plots=FALSE){

  c1<-class(object)[1]
  c2<-class(object)[2]

  if(c1 != "fpr"){
    stop(paste(object, "not an fpr_desc object"))
  }

  if(c2 == "desc"){

    summary_desc(object)

  } else if(c2 == "norm"){

    summary_norm(object, plots)
  }
}


######################################## Print Function #####################################

#' @export
print.fpr<-function(object, ...){

  summary.fpr(object, ...)

}


######################################## Plot Function #######################################

#' @export
plot.fpr<-function(object, ...){

  c1<-class(object)[1]
  c2<-class(object)[2]

  if(c2=="norm"){
    plot_normality(object, ...)
  }
}
