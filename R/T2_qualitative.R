#' @import ca
#' @import highcharter
#' @import dplyr
#'
globalVariables(c("var","qchisq","sd","abline"))

#' @title Multivariate control chart for qualitative variables
#'
#' @description Multivariate control chart T2 Hotelling applicable for qualitative variables.
#' @param base Data set
#' @param IndK Character with the name of the column that specifies the partition of the data set in k tables.
#' @param dim Dimension taken for reduction. Initial dimension - 1 is recommended.
#' @param interactive If it is TRUE, the graph will be shown interactively. If FALSE, the graph is displayed flat. FALSE is the default.
#' @param alpha Type I error, it is recommended to reach this value by using the ARL.
#' @return A control chart made with the T2 hotelling statistic, applied to detect anomalies in any of the K tables obtained with the specification of \code{IndK}. The control limit of the graph is obtained from the number of dimensions \code{dim} and the type I error \code{alpha}.
#' @examples
#' data(Datak10Contaminated)
#' T2_qualitative(Datak10Contaminated,"GroupLetter",9, FALSE,0.0027)
#' @export
T2_qualitative <- function(base, IndK, dim, interactive=FALSE, alpha=0.0027){

  Table <- list()
  Ind <- base%>% pull(IndK)
  groupFactor=as.factor(Ind)

  for (i in 1:length(levels(groupFactor))){
    Table[[i]] <- subset(base, Ind==as.character(levels(groupFactor)[i]))
  }



  for (i in 1:length(levels(groupFactor))){
    Table[[i]] <- Table[[i]][,!names(Table[[i]]) %in% IndK, drop = F]}

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
  mjcatable <- function(base, dime){

    MJCA <- mjca(base,nd = dime)
    Col <- data.frame(MJCA$colcoord[,1:dime])
    rownames(Col) <- MJCA$levelnames
    return(Col)
  }
  colcoor <- list()



  for (i in 1:length(levels(groupFactor))){
    colcoor[[i]] <- mjcatable(Table[[i]],dim)
  }

  NormalizacionAFM <- function(mjca){
    SVD <- svd(mjca)
    ACP1_VALUE <- (SVD$d[1])
    return(mjca/ACP1_VALUE)
  }
  coornorm <- list()
  for (i in 1:length(levels(groupFactor))){
    coornorm[[i]] <- NormalizacionAFM(abs(colcoor[[i]]))
    colnames(coornorm[[i]]) <- paste0("V",1:ncol(coornorm[[i]]))
  }


  colnum <- c()
  for (i in 1:length(coornorm)){
    colnum[i] <- ncol(coornorm[[i]])
  }

  General <- data.frame(coornorm[[1]][,1:min(colnum)])

  for (i in 2:length(levels(groupFactor))){
    General <- rbind(General,coornorm[[i]][,1:min(colnum)])
  }

  mu00 <- apply(General,2, 'median')
  muii <- list()
  n <- list()
  for (i in 1:length(levels(groupFactor))){
    muii[[i]] <- apply(coornorm[[i]][,1:min(colnum)],2,'median')
    n[[i]] <- nrow(coornorm[[i]])
  }

  t2 <- list()
  sigma <- var(General)

  for (i in 1:length(levels(groupFactor))){
    t2[[i]]=n[[i]]*(t(muii[[i]]-mu00)%*%solve(sigma)%*%(muii[[i]]-mu00))
  }

  T2 <- as.data.frame(as.matrix(t2))
  DtGraph <- data.frame(table=seq(1:nrow(T2)),hote=as.numeric(as.matrix(T2$V1)))
p=ncol(base)
m=length(colcoor)

#alpha2 <- 1-(1-alpha)^dim
alpha2 <- alpha

LC <- qchisq(p=alpha2,df=dim, lower.tail = FALSE)

 # LC <-( (p*(m+1)*(m-1))/(m*(m-p)))*qf(alpha,p,(m-p))
  if (max(DtGraph$hote)>LC){
    YLIM=max(DtGraph$hote)+sd(DtGraph$hote)
  } else {
    YLIM=LC+sd(DtGraph$hote)
  }

  Categories <- levels(groupFactor)
  if (interactive==FALSE){
    plot(DtGraph, type='l', main="Multivariate Control Chart", sub=paste0("UCL = ",round(LC,2), ", alpha = ",alpha,", ARL = ",round(1/alpha)),xlab="k Table", ylab="T2 Hotelling",ylim=c(0,YLIM))+
      abline(h=LC, col="blue")
    message('If you want the interactive chart specify interactive = TRUE')

  }else{
    highchart()%>%
      hc_add_series(DtGraph, type='line',hcaes( y='hote'), name="T2 Hotelling", color="#1C3F63")%>%
      hc_title(text="Multivariate Control Chart")%>%
      hc_subtitle(text=paste0("UCL = ",round(LC,2), ", alpha = ",alpha,", ARL = ",round(1/alpha)))%>%
      hc_xAxis(categories=Categories)%>%
      hc_yAxis(max=YLIM,
               plotLines = list(list(
                 value = LC,
                 color = '#821D1D',
                 width = 3,
                 zIndex = 4,
                 label = list(text = "",
                              style = list( color = '#821D1D', fontWeight = 'bold' )))))
   # %>%
   #   hc_annotations(
   #     list(labelOptions = list(y = 35, x = 0, backgroundColor = '#E6EEFF', borderColor = "#1D4B5E"),
   #          labels = list(
   #            list(style = list(color = '#1D4B5E', fontSize = 8),
   #                 useHTML = TRUE,
   #                 point = list(x = 1, y = LC+2*sd(DtGraph$hote), xAxis = 0, yAxis = 0),text = paste0("UL = ",round(LC,2), "<br/> alpha = ",alpha,"<br/> ARL = ",round(1/alpha)))
   #          )
   #     )
   #   )
  }
}
