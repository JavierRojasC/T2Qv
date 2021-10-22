#' @import ca
#' @import highcharter
#' @import dplyr
#' @import stringr
#'
globalVariables(c("AC.SUM1...5.","AC.SUM1...8."))
#' @title ACM Consensus
#'
#' @description Multiple correspondence analysis applied to a consensus table.
#' @param base Data set
#' @param IndK Character with the name of the column that specifies the partition of the data set in k tables.
#' @param interactive If it is TRUE, the graph will be shown interactively. If FALSE, the graph is displayed flat. FALSE is the default.
#' @return A Multiple Correspondence Analysis graph of the consensus table.
#' @examples
#' data(Datak10Contaminated)
#' ACMconsensus(Datak10Contaminated,"GroupLetter", interactive = FALSE)
#' @export
ACMconsensus <- function(base, IndK, interactive=FALSE){


  Table <- base

  Table <- Table[,!names(Table) %in% IndK, drop = F]

  # mjcatable <- function(base){
  #   MJCA <- mjca(base,nd = 2)
  #   BURT <- MJCA$Burt
  #   P <- BURT/sum(as.matrix(BURT))
  #   r <- apply(P, 1, sum)
  #   c <- apply(P, 2, sum)
  #   D_r_inv_squ <- diag(1/sqrt(r))
  #   D_c_inv_squ <- diag(1/sqrt(c))
  #   S <- D_r_inv_squ%*%(P-r%*%t(c))%*%D_c_inv_squ
  #   SVD <- svd(S)
  #   U <- SVD$u
  #   V <- SVD$v
  #   F <- D_r_inv_squ%*%U
  #   C <- D_c_inv_squ%*%V
  #   return(C)
  # }



  AC <- mjca(Table)
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



  highchart()%>%
    hc_add_series(Coord, type='scatter', hcaes(x=AC.SUM1...5., y=AC.SUM1...8., name=Nombrecorto, group=Nombres),
                  dataLabels=list(format="{point.name}",enabled=TRUE),
                  tooltip = list(pointFormat = "{point.name}"))%>%
    hc_xAxis(
      title = list(text = paste0("Dim 1 - ",round(AC$inertia.e*100,2)[1],"%")),
      plotLines = list(list(
        value = 0,
        color = '#1D4B5E',
        width = 3,
        zIndex = 4,
        label = list(text = "",
                     style = list( color = '#1D4B5E', fontWeight = 'bold' )))))%>%
    hc_yAxis(
      title = list(text = paste0("Dim 2 - ",round(AC$inertia.e*100,2)[2],"%")),
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
    hc_title(text="Consensus")
  } else {
  plot(AC)
}

}
