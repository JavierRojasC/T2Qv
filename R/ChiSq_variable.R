#' @import ca
#' @import highcharter
#' @import dplyr
#' @import tables
#'
globalVariables(c("pchisq"))

#' @title Chi squared variable from point table.
#'
#' @description Contains Chi square distance between the column masses of the table specified in \code{PointTable} and the consolidated table. It allows to identify which mode is responsible for the anomaly in the table in which it is located.
#' @param base Data set
#' @param IndK Character with the name of the column that specifies the partition of the data set in k tables.
#' @param PointTable Table indicator. A character or number that is part of the \code{IndK} registers. This argument specifies the table to which the analysis will be performed.
#' @return A table with Chi square distances between the column masses of the table specified in \code{PointTable} and the consolidated table. It has an indicator of significance with stars based on a scale.
#' @examples
#' data(Datak10Contaminated)
#' ChiSq_variable(Datak10Contaminated, "GroupLetter", PointTable="j")
#' @export
ChiSq_variable <- function(base, IndK, PointTable, interactive=FALSE){

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



  AC_Point <- mjca(Table[[k_item]])
  AC_Cons <- mjca(BaseCons)
  chi <- sqrt(((((AC_Point$colmass)*nrow(base))-((AC_Cons$colmass)*nrow(base)))^2)/((AC_Cons$colmass)*nrow(base)))
  valp=pchisq(chi,1,lower.tail=FALSE)
  starInd <- c()

  for (i in 1:length(valp)){

    if (valp[i]<=0.05 & valp[i]>=0.01){
      star="*"} else if (valp[i]<0.01 & valp[i]>=0.001){
        star="**"
      } else if (valp[i]<0.001){
        star="***"
      } else {
        star=" "
      }
    starInd <- c(starInd,star)
  }
result <- list()


  if (interactive==FALSE){
    TableChi <- data.frame(Variable=AC_Point$levelnames, `Chi-Squared`=chi, `val-p`=valp,Signif=starInd)
    result$`Difference between modalities - Chi square` <- TableChi
    result$`Signif. codes` <- paste("0 '***' 0.001 '**' 0.05 '*'")
    result
  } else {
    TableChi <- data.frame(Variable=AC_Point$levelnames, `Chi-Squared`=chi, `val-p`=valp,Signif=starInd)

    ColorBars <- c()

    for (i in 1:length(valp)){
      if (valp[i]<=0.05 & valp[i]>=0.01){
        ColorBar="#FF7575"} else if (valp[i]<0.01 & valp[i]>=0.001){
          ColorBar="#CA3100"
        } else if (valp[i]<0.001){
          ColorBar="#8A2200"
        } else {
          ColorBar="#A7A7A7"
        }
      ColorBars <- c(ColorBars,ColorBar)
    }

    #chi0.05 <- qchisq(0.05, 1, lower.tail = FALSE)
    #chi0.01 <- qchisq(0.01, 1, lower.tail = FALSE)
    #chi0.001 <- qchisq(0.001, 1, lower.tail = FALSE)
    TableChi <- data.frame(TableChi, ColorBars)
    highchart()%>%
      hc_add_series(TableChi, type='column', hcaes(x=Variable,y=Chi.Squared, color=ColorBars), color='#FFFFFF',name='ChiSq Distance')%>%
      hc_xAxis(categories=TableChi$Variable)%>%
      hc_add_series(TableChi,type='line',hcaes(x=Variable,y=3.841459),color='#FF7575' ,name='*')%>%
      hc_add_series(TableChi,type='line',hcaes(x=Variable,y=6.634897),color='#CA3100' ,name='**')%>%
      hc_add_series(TableChi,type='line',hcaes(x=Variable,y=10.82757),color='#8A2200' ,name='***')%>%
      hc_plotOptions(
        line = list(
          marker = list(
            enabled=FALSE

          )
        )
      )%>%
      hc_title(text='Difference between modalities - Chi square')%>%
      hc_subtitle(text="Signif. Codes 0 '***' 0.001 '**' 0.05 '*'")

  }

  #print(TableCHI, quote = FALSE,row.names = FALSE)


}
