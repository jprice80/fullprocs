#' @title Descriptive Statistics on variables within a data frame
#'
#' @description The \code{descriptives} function is used to perform descriptive statistics on a set of variables in a \code{\link{data.frame}}.
#' This can include descriptive statistics by group when including a vector of categorical variables in the groupby= argument.
#'
#' @param data A \code{\link{data.frame}} containing columns of variables.
#' @param vars A \code{\link{vector}} of variable names existing in the dataframe to perform descriptive statistics on.
#' @param groupby An optional \code{\link{vector}} containing factor names within a dataframe used for by group processing.
#' @param conf.level The confidence level for mean and median confidence intervals. The default value is 0.95.
#' @param medianCI A TRUE/FALSE boolean value indicating whether or not to produce bootstrapped confidence intervals on the median (computationally intensive).
#' @param R The number of bootstrap replications to use if medianCI = TRUE. Otherwise has no effect.
#' @param round The number of digits to round the final result.
#'
#' @return A \code{\link{list}} containing descriptive statistics for each variable listed in the \code{vars} statement.
#' @export
#'
#' @examples
#' descriptives(data=mtcars, vars=c("mpg","disp"), groupby=c("vs","am"))
descriptives<-function(data, vars, groupby=NULL, conf.level=0.95, medianCI=FALSE, R=2000, round=NULL){

  #Check to insure data is a dataframe
  if(!is.data.frame(data)){
    stop(paste("The object", data, "is not a dataframe"));
  }

  # check to see if all vars are numeric
  temp <- data %>% select(vars)
  temp <- as.vector(sapply(temp,is.numeric))

  if(all(temp)==FALSE){
    stop(paste("All variables listed in the vars= statement are not numeric"));
  }


  #Start actual function
  out<-list()
  out$meta<-list()
  out$output<-list()

  out[["meta"]][["vars"]]<-vars
  out[["meta"]][["groupby"]]<-groupby
  out[["meta"]][["conf.level"]]<-conf.level
  out[["meta"]][["medianCI"]]<-medianCI
  out[["meta"]][["R"]]<-R
  out[["meta"]][["round"]]<-round

  for(i in 1:length(vars)){

    varname<-vars[i]

    if(!is.null(groupby)){

      dat<-data %>% group_by(.dots=groupby) %>%
        summarise(ntotal = n(),
                  nmiss = sum(is.na(!!sym(vars[i]))),
                  mean = mean(!!sym(vars[i]), na.rm = TRUE),
                  sd = sd(!!sym(vars[i]), na.rm = TRUE),
                  stderr = sd/sqrt(n()),
                  median = median(!!sym(vars[i]), na.rm = TRUE),
                  min = min(!!sym(vars[i]), na.rm = TRUE),
                  max = max(!!sym(vars[i]), na.rm = TRUE),
                  pct25 = quantile(!!sym(vars[i]), probs=0.25, na.rm=TRUE),
                  pct75 = quantile(!!sym(vars[i]), probs=0.75, na.rm=TRUE),
                  IQR = pct75 - pct25)
      #LCL = mean - qt(1 - (0.05 / 2), ntotal - 1) * stderr,
      #UCL = mean + qt(1 - (0.05 / 2), ntotal - 1) * stderr,
      #LCLmed = MedianCI(!!sym(vars[i]), method="boot", na.rm=TRUE)[2],
      #UCLmed = MedianCI(!!sym(vars[i]), method="boot", na.rm=TRUE)[3])

      #CL mean calcs
      clm<-data %>% group_by(.dots=groupby) %>% summarise(list(DescTools::MeanCI(!!sym(vars[i]), method="classic", na.rm=TRUE, conf.level = conf.level)))
      clm<-clm[[length(clm)]]
      clm<-unlist(clm)
      nms<-names(clm)
      clm<-data.frame(names=nms, clm)

      lwrm<-clm %>% filter(names=="lwr.ci")
      uprm<-clm %>% filter(names=="upr.ci")

      quantile

      if(medianCI==TRUE){
        #CL median calcs
        clmed<-data %>% group_by(.dots=groupby) %>% summarise(list(DescTools::MedianCI(!!sym(vars[i]), method="boot", na.rm=TRUE, R=2000, conf.level = conf.level)))
        clmed<-clmed[[length(clmed)]]
        clmed<-unlist(clmed)
        nms<-names(clmed)
        clmed<-data.frame(names=nms, clmed)

        lwrmed<-clmed %>% filter(names=="lwr.ci")
        uprmed<-clmed %>% filter(names=="upr.ci")

        #combine CLmean and CLmed components
        confint<-data.frame(LCLmean=lwrm[,2], UCLmean=uprm[,2], LCLmed=lwrmed[,2], UCLmed=uprmed[,2])
        dat<-cbind(as.data.frame(dat), confint)
      } else {
        confint<-data.frame(LCLmean=lwrm[,2], UCLmean=uprm[,2])
        dat<-cbind(as.data.frame(dat), confint)
      }


    } else {

      dat<-data %>%
        summarise(ntoal = n(),
                  nmiss = sum(is.na(!!sym(vars[i]))),
                  mean = mean(!!sym(vars[i]), na.rm = TRUE),
                  sd = sd(!!sym(vars[i]), na.rm = TRUE),
                  stderr = sd/sqrt(n()),
                  median = median(!!sym(vars[i]), na.rm = TRUE),
                  min = min(!!sym(vars[i]), na.rm = TRUE),
                  max = max(!!sym(vars[i]), na.rm = TRUE),
                  pct25 = quantile(!!sym(vars[i]), probs=0.25, na.rm=TRUE),
                  pct75 = quantile(!!sym(vars[i]), probs=0.75, na.rm=TRUE),
                  IQR = pct75 - pct25)
      #LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
      #UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
      #LCLmed = MedianCI(!!sym(vars[i]), method="exact", na.rm=TRUE)[2],
      #UCLmed = MedianCI(!!sym(vars[i]), method="exact", na.rm=TRUE)[3])

      #CL mean calcs
      clm<-data %>% summarise(list(DescTools::MeanCI(!!sym(vars[i]), method="classic", na.rm=TRUE, conf.level = conf.level)))
      clm<-clm[[length(clm)]]
      clm<-unlist(clm)
      nms<-names(clm)
      clm<-data.frame(names=nms, clm)

      lwrm<-clm %>% filter(names=="lwr.ci")
      uprm<-clm %>% filter(names=="upr.ci")

      if(medianCI==TRUE){

        #CL median calcs
        clmed<-data %>% summarise(list(DescTools::MedianCI(!!sym(vars[i]), method="boot", na.rm=TRUE, R=R, conf.level = conf.level)))
        clmed<-clmed[[length(clmed)]]
        clmed<-unlist(clmed)
        nms<-names(clmed)
        clmed<-data.frame(names=nms, clmed)

        lwrmed<-clmed %>% filter(names=="lwr.ci")
        uprmed<-clmed %>% filter(names=="upr.ci")

        #combine CLmean and CLmed components
        confint<-data.frame(LCLmean=lwrm[,2], UCLmean=uprm[,2], LCLmed=lwrmed[,2], UCLmed=uprmed[,2])
        dat<-cbind(as.data.frame(dat), confint)
      } else {
        confint<-data.frame(LCLmean=lwrm[,2], UCLmean=uprm[,2])
        dat<-cbind(as.data.frame(dat), confint)
      }
    }

    dat<-cbind(variable=varname, as.data.frame(dat))
    row.names(dat)<-NULL

    out$output[[varname]]<-dat
  }

  if(!is.null(round)){
    out$output<-round_list(out$output, round)
  }

  class(out)<-c("fpr", "desc")

  return(out)
}

#====================================== Summary ===================================================

summary_desc<-function(object){

  groupby<-object$meta$groupby

  if(is.null(groupby)){
    print(suppressWarnings(bind_rows(object$output)), row.names=FALSE)
  } else{
    for(i in 1:length(object$output)){
      print(object$output[[i]], row.names=FALSE)
      cat("\n")
    }
  }

  return(invisible(object))
}

#====================================== Reports ===================================================

report_desc<-function(object, style="multiline", split.tables=110, keep.trailing.zeros=TRUE, ...){

  c1<-class(object)[1]
  c2<-class(object)[2]
  groupby<-object$meta$groupby

  if(c1 != "fpr"){
    stop(paste(object, "not an fpr object"))
  }

  cat("\n")
  cat("=======================================================================================================================")
  cat("\n")
  cat("Descriptive Statistics")
  cat("\n")
  cat("=======================================================================================================================")
  cat("\n")

  if(is.null(groupby)){
    out<-suppressWarnings(bind_rows(object$output))
    pander::pandoc.table(out, style=style, split.tables=split.tables, keep.trailing.zeros=keep.trailing.zeros, ...)
  } else {

    for(i in 1:length(object$output)){
      out<-object$output[[i]]
      pander::pandoc.table(out, style=style, split.tables=split.tables, keep.trailing.zeros=keep.trailing.zeros, ...)
    }
  }
}

#====================================== Write Reports ===================================================

write_desc<-function(object, path, tablefont="Arial", tablefontsize=10, digits=NULL){

  #Set table value as integer
  tablefontsize<-as.integer(tablefontsize)

  #determine the correct number of digits to print
  if(is.null(digits)){
    if(is.null(object$meta$round)){
      digits<-as.integer(4)
    } else {
      digits<-as.integer(object$meta$round)
    }
  } else {
    digits<-as.integer(digits)
  }

  format_table_title<-fp_text(color="black", font.size=16, bold=TRUE, italic=FALSE, underlined = FALSE,
                              font.family = "Arial", vertical.align = "baseline", shading.color = "transparent")

  doc <- read_docx()
  doc %>% body_add_fpar(fpar(ftext('Descriptive Statistics', prop=format_table_title)), style='centered')
  doc %>% body_add_par("")

  for(i in 1:length(object$output)){
    #doc %>% body_add_table(object[[i]], style = "table_template", no_vband=FALSE)

    ft <- flextable(object$output[[i]])
    ft <- fontsize(ft, size = tablefontsize, part = "all")
    ft <- font(ft, fontname = tablefont, part = "all")
    ft <- colformat_num(ft, digits = digits, col_keys = names(object$output[[i]]))
    ft <- autofit(ft, add_w = 0, add_h = 0)

    doc <- body_add_flextable(doc, value = ft)
    doc %>% body_add_par("")
  }

  doc %>% body_end_section_landscape()
  print(doc, target = path)
}
