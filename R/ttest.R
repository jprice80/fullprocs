
ttest<-data(data, formula, conf.level=0.95, plots=FALSE, assumptions=TRUE){

  #Check to insure data is a dataframe
  if(!is.data.frame(data)){
    stop(paste("The object", data, "is not a dataframe"));
  }

  out[["meta"]][["data"]]<-data
  out[["meta"]][["formula"]]<-formula
  out[["meta"]][["conf.level"]]<-conf.level


  y<-attr(attr(terms(formula), "factors"), "dimnames")[[1]][1]
  x<-attr(terms(formula), "term.labels")

  desc<-descriptives(data = data, vars=y, groupby=x, round=2)

  norm<-normality(data = data, vars=y, groupby=x, plots=plots)
  ev<-equalvar(data=data, formula=formula, plots=plots)

  pooled<-t.test(formula=formula, data=data, var.equal=TRUE, conf.level=conf.level)
  welch<-t.test(formula=formula, data=data, var.equal=FALSE, conf.level=conf.level)


}
