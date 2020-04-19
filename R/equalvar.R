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

      cur<-data.frame(Effect=x[i], dfnum=levmed[1,1], dfden=levmed[2,1], `F value`=round(levmed[1,2],4),
                      `Pr(>F)`=round(levmed[1,3],4), SD.ratio, Var.ratio)
    } else {
      cur<-data.frame(Effect=x[i], Levene.pval=round(levmed$'Pr(>F)',4)[1],SD.ratio=fctvals$StdChg, Var.ratio=fctvals$VarChg)
    }

    eqvar<-rbind(eqvar, cur)
  }

  class(out)<-c("fpr", "lev")

  return(eqvar)
}
