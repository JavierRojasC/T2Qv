#' @import ca
#' @import highcharter
#' @import dplyr
#' @import stringr
#'
globalVariables(c("AC.SUM1...5.","AC.SUM1...8."))
#'
#' @title ACM in one point
#'
#' @description Multiple correspondence analysis applied to a specific table.
#' @param base Data set
#' @param IndK Character with the name of the column that specifies the partition of the data set in k tables.
#' @param PointTable Table indicator. A character or number that is part of the \code{IndK} registers. This argument specifies the table to which the analysis will be performed.
#' @param interactive If it is TRUE, the graph will be shown interactively. If FALSE, the graph is displayed flat. FALSE is the default.
#' @return A Multiple Correspondence Analysis graph of the table specified in \code{PointTable}.
#' @examples
#' data(Datak10Contaminated)
#' ACMpoint(Datak10Contaminated,"GroupLetter", PointTable="j", interactive=FALSE)
#' @export
ACMpoint <- function(base, IndK, PointTable, interactive=FALSE){

  Table <- list()
  Ind <- base%>% pull(IndK)
  groupFactor=as.factor(Ind)

  k_item=match(PointTable,levels(groupFactor))

  for (i in 1:length(levels(groupFactor))){
    Table[[i]] <- subset(base, Ind==as.character(levels(groupFactor)[i]))
  }


  for (i in 1:length(levels(groupFactor))){
    Table[[i]] <- Table[[i]][,!names(Table[[i]]) %in% IndK, drop = F]}



  AC <- mjca(Table[[k_item]])
if (interactive==TRUE){
  AC.SUM <- summary(AC)
  AC.SUM1 <- AC.SUM$columns
  Coord <- data.frame(AC.SUM1[,5],AC.SUM1[,8])/1000

  Nombres <- data.frame(AC.SUM1$name)
  xs <- str_split(Nombres$AC.SUM1.name, ":")
  XSDF <- as.data.frame(xs[1:length(xs)])
  XSDF_t <- as.data.frame(t(XSDF))
  Nombres <- XSDF_t$V1
  Nombrecorto <- XSDF_t$V2
  Coord <- data.frame(Coord,Nombrecorto,Nombres)
title=paste("Point",PointTable)


  highchart()%>%
    hc_add_series(Coord, type='scatter', hcaes(x=AC.SUM1...5., y=AC.SUM1...8., name=Nombrecorto, group=Nombres),
                  dataLabels=list(format="{point.name}",enabled=TRUE),
                  tooltip = list(pointFormat = "{point.name}"))%>%
    hc_xAxis(
      title = list(text = "Dim 1"),
      plotLines = list(list(
        value = 0,
        color = '#1D4B5E',
        width = 3,
        zIndex = 4,
        label = list(text = "",
                     style = list( color = '#1D4B5E', fontWeight = 'bold' )))))%>%
    hc_yAxis(
      title = list(text = "Dim 2"),
      plotLines = list(list(
        value = 0,
        color = '#1D4B5E',
        width = 3,
        zIndex = 4,
        label = list(text = "",
                     style = list( color = '#1D4B5E', fontWeight = 'bold' )))))%>%
    hc_exporting(enabled = TRUE,
                 filename = "")%>%
    hc_credits(
      enabled = TRUE,
      text = "",
      href = ""
    )%>%
    hc_subtitle(text="Multiple correspondence analysis")%>%
    hc_title(text=title)
} else {
  plot(AC)
}
}
