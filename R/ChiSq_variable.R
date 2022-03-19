#' @import ca
#' @import highcharter
#' @import dplyr
#' @import tables
#' @import stringr
#'
globalVariables(c("pchisq","Variable","Chi.Squared"))

#' @title Chi squared variable from point table.
#'
#' @description Contains Chi square distance between the column masses of the table specified in \code{PointTable} and the consensus table. It allows to identify which mode is responsible for the anomaly in the table in which it is located.
#' @param base Data set
#' @param IndK Character with the name of the column that specifies the partition of the data set in k tables.
#' @param PointTable Table indicator. A character or number that is part of the \code{IndK} registers. This argument specifies the table to which the analysis will be performed.
#' @param interactive If it is TRUE, the graph will be shown interactively. If FALSE, the graph is displayed flat. FALSE is the default.
#' @param ylim ylim

#' @return A table with Chi square distances between the column masses of the table specified in \code{PointTable} and the consensus table. It has an indicator of significance with stars based on a scale.
#' @examples
#' data(Datak10Contaminated)
#' ChiSq_variable(Datak10Contaminated, "GroupLetter", PointTable="j", ylim=5)
#' @export
ChiSq_variable <- function(base, IndK, PointTable, interactive=FALSE, ylim=5){

  Table <- list()
  Ind <- base%>% pull(IndK)
  groupFactor=as.factor(Ind)

  k_item=match(PointTable,levels(groupFactor))

  for (i in 1:length(levels(groupFactor))){
    Table[[i]] <- subset(base, Ind==as.character(levels(groupFactor)[i]))
  }


  for (i in 1:length(levels(groupFactor))){
    Table[[i]] <- Table[[i]][,!names(Table[[i]]) %in% IndK, drop = F]}
  BaseCons <- base[,!names(base) %in% IndK, drop = F]
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





  AC_Point <- mjca(Table[[k_item]], dim)
  AC_Cons <- mjca(BaseCons, dim)

  #AC_Point$Burt

  MassPoint <- data.frame(modalities=AC_Point$levelnames,AC_Point$colmass)
  MassConsCero <- data.frame(modalities=AC_Cons$levelnames)




  Point <- merge(MassPoint,MassConsCero, id='modalities', all.y=TRUE)
  Point$AC_Point.colmass[is.na(Point$AC_Point.colmass)] <- 0
  #sum(AC_Point$colmass)
  #chi <- (((((Point$AC_Point.colmass)*nrow(base))-((AC_Cons$colmass)*nrow(base)))^2/))*max()
  rango_punto <- max(Point$AC_Point.colmass)-min(Point$AC_Point.colmass)
  rango_cons <- max(AC_Cons$colmass)-min(AC_Cons$colmass)

  #(Point$AC_Point.colmass)-(AC_Cons$colmass)

  for (i in 1:length( unique(AC_Cons$factors[,1]) )){
    MassPoint
  }

  chi <- (((Point$AC_Point.colmass)-(AC_Cons$colmass))^2)/(AC_Cons$colmass)
  chiDF <- data.frame(Point$modalities,chi)
  xs <- str_split(Point$modalities, ":")
  XSDF <- as.data.frame(xs[1:length(xs)])
  XSDF_t <- as.data.frame(t(XSDF))
  Nombres <- XSDF_t$V1
  Nombrecorto <- XSDF_t$V2
  Chi <- data.frame(chi,Nombrecorto,Nombres)

  chigroup <- Chi%>%
    group_by(Nombres)%>%
    summarise(Sum=sum(chi))

  if (interactive==FALSE){
    chigroup <- data.frame(chigroup)
    names(chigroup) <- c("Variables","ChiSq")
    chigroup

  } else {

  highchart()%>%
    hc_add_series(chigroup, type='column', hcaes(x=Nombres,y=Sum), color='#FF7575',name='ChiSq Distance')%>%
    hc_xAxis(categories=chigroup$Nombres)%>%
    #  hc_add_series(TableChi,type='line',hcaes(x=Variable,y=1),color='#FF7575' ,name='*')%>%
    #  hc_add_series(TableChi,type='line',hcaes(x=Variable,y=1.5),color='#CA3100' ,name='**')%>%
    #  hc_add_series(TableChi,type='line',hcaes(x=Variable,y=2),color='#8A2200' ,name='***')%>%
    hc_plotOptions(
      line = list(
        marker = list(
          enabled=FALSE

        )
      )
    )%>%
    hc_yAxis(max = ylim)%>%
    hc_title(text='Chi-squared distance between the column masses of the k table and the consensus')%>%
    hc_subtitle(text="Signif. Codes 0 '***' 0.001 '**' 0.05 '*'")

}


  #print(TableCHI, quote = FALSE,row.names = FALSE)


}
