#' Equality of Variances
#'
#' @param data A \code{\link{data.frame}} containing columns of variables.
#' @param formula An object of class \code{\link{formula}}.  The left side of the formula should be a numeric continuous response.  The right side of the formula should consist of categorical factors. The variables on the right-hand-side of the model must all be factors and must be completely crossed.
#' @param center The name of a function to compute the center of each group; mean gives the original Levene's test; the default, median, provides a more robust test.
#'
#' @return Returns a \code{\link{data.frame}} meant to be printed showing the results of the test.
#' @note This function primarily utilizes functionality from the \code{\link{car}} package
#' @references
#'
#' Fox, J. (2016) Applied Regression Analysis and Generalized Linear Models, Third Edition. Sage.
#'
#' Fox, J. and Weisberg, S. (2019) An R Companion to Applied Regression, Third Edition, Sage.
#'
#' @export
#' @examples
#' equalvar(data=mtcars, mpg~am*vs, center="median")
equalvar<-function(data, formula, center="median"){

  #Check to insure data is a dataframe
  if(!is.data.frame(data)){
    stop(paste("The object", data, "is not a dataframe"));
  }

  #Setup output template
  #Start actual function
  out<-list()
  out$meta<-list()
  out$output<-list()

  #Define formula, Y, and Xs
  formula<-as.formula(formula)
  x<-attr(terms.formula(formula), "term.labels")
  y<-deparse(formula[[2]])

  # check to see if all y is numeric
  temp <- data %>% select(y)
  temp <- as.vector(sapply(temp,is.numeric))

  if(all(temp)==FALSE){
    stop(paste("Dependent variable is not numeric"));
  }

  # check to see if X variables are factors, if not, make them a factor, and create a new dataset
  remove <-grepl(":", x)
  tempx  <- x[remove==FALSE]
  temp   <- data %>% select(tempx)
  temp   <- as.vector(sapply(temp,is.factor))

  vars<-c(y,tempx)
  data2 <- data %>% select(vars)

  for(i in 1:length(tempx)){
    if(temp[i] == FALSE){
      data2[[tempx[i]]]<-as.factor(data2[[tempx[i]]])
    }
  }

  out[["meta"]][["data"]]<-data2
  out[["meta"]][["formula"]]<-formula

  #Levene's centered median equal variances and fligner
  eqvar<-data.frame()
  for(i in 1:length(x)){

    #test for an interaction term
    grp<-as.factor(as.vector(data2[[x[i]]]))
    form<-as.formula(paste0(y, "~",x[i]))

    levmed <-car::leveneTest(form, center=center, data=data2)

    test <-grepl(":", x[i])

    if(test==TRUE){
      grp<-unlist(strsplit(x[i], ":"))
    }

    fctvals<-data2 %>% group_by(.dots = x[i]) %>% summarize(StdChg=sd(!!sym(y), na.rm=TRUE), VarChg=var(!!sym(y), na.rm=TRUE))

    if(nrow(fctvals) > 1){
      minY<-min(fctvals$StdChg, na.rm = TRUE)
      maxY<-max(fctvals$StdChg, na.rm = TRUE)
      minR<-min(fctvals$VarChg, na.rm = TRUE)
      maxR<-max(fctvals$VarChg, na.rm = TRUE)
      SD.ratio<-round(maxY/minY,2)
      Var.ratio<-round(maxR/minR,2)

      cur<-data.frame(Effect=x[i], SD.ratio, Var.ratio,
                      dfnum=levmed[1,1], dfden=levmed[2,1], F=round(levmed[1,2],2),
                      pvalue=round(levmed[1,3],4))
    } else {
      cur<-data.frame(Effect=x[i], SD.ratio=fctvals$StdChg, Var.ratio=fctvals$VarChg,
                      dfnum=levmed[1,1], dfden=levmed[2,1], F=round(levmed[1,2],2),
                      pvalue=round(levmed[1,3],4))
    }

    eqvar<-rbind(eqvar, cur)
  }

  colnames(eqvar) <- c("Effect", "SDratio", "VarRatio", "NumDF", "DenDF", "Fvalue", "Pr(>F)")
  row.names(eqvar)<-NULL
  out$output<-eqvar
  class(out)<-c("fpr", "eqvar")

  return(out)
}

#====================================== Summary ===================================================


summary_eqvar<-function(object){

  print(object$output, row.names=FALSE)
  cat("\n")
  return(invisible(object))
}

#====================================== Reports ===================================================


report_eqvar<-function(object, style="multiline", plots=FALSE, split.tables=110, keep.trailing.zeros=TRUE,  ...){

  cat("\n")
  cat("=======================================================================================================================")
  cat("\n")
  cat("Levene's Test for Homogeneity of Variance")
  cat("\n")
  cat("=======================================================================================================================")
  cat("\n")

  out<-object$output
  pander::pandoc.table(out, style=style, split.tables=split.tables, keep.trailing.zeros=keep.trailing.zeros, ...)
}

