#' @import ca
#' @import highcharter
#' @import dplyr
#' @import tables
#' @import stringr
#' @import tidyr
#' @import purrr
#'
globalVariables(c("Variable","Chi.Squared"))

#' @title Chi squared variable from point table.
#'
#' @description Contains Chi square distance between the column masses of the table specified in \code{PointTable} and the consensus table. It allows to identify which mode is responsible for the anomaly in the table in which it is located.
#' @param base Data set
#' @param IndK Character with the name of the column that specifies the partition of the data set in k tables.
#' @param PointTable Table indicator. A character or number that is part of the \code{IndK} registers. This argument specifies the table to which the analysis will be performed.
#' @param interactive If it is TRUE, the graph will be shown interactively. If FALSE, the graph is displayed flat. FALSE is the default.
#' @param ylim y-axis limit.

#' @return A table with Chi square distances between the column masses of the table specified in \code{PointTable} and the consensus table.
#' @examples
#' data(Datak10Contaminated)
#' ChiSq_variable(Datak10Contaminated, "GroupLetter", PointTable="j", ylim=5)
#' @export
ChiSq_variable <- function(base, IndK, PointTable, interactive=FALSE, ylim=0.09){
  names(base) <- str_replace(names(base), " ",".")
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
    summarise(Sum=round(sum(chi),2))

  if (interactive==FALSE){
    names(chigroup) <- c("Variables","ChiSq")
    chigroup
  } else {

    chigroup

    Tabs <- Table[[k_item]]

    Tables <- list()

    Tabs2 <- data.frame(names(Tabs)[1],list(as.data.frame(table(Tabs[1]))),prop.table(table(Tabs[1])))
    colnames(Tabs2)=c("Nombres","cat","freq","cat2","prop")
    Tabs2 <- data.frame(Nombres=Tabs2$Nombres,cat=Tabs2$cat,freq=Tabs2$freq,cat2=paste(Tabs2$cat2,round(Tabs2$prop,2)))
    for (i in 2:ncol(Tabs)){
      Tabss=data.frame(names(Tabs)[i],data.frame(table(Tabs[i])),prop.table(table(Tabs[i])))
      colnames(Tabss)=c("Nombres","cat","freq","cat2","prop")
      Tabss <- data.frame(Nombres=Tabss$Nombres,cat=Tabss$cat,freq=Tabss$freq,cat2=paste(Tabss$cat2,round(Tabss$prop,2)))

      Tabs2 <- rbind(Tabs2,Tabss)

      #Tables[[names(Tabs)[i]]] <- as.data.frame(table(Tabs[i]))

      #A <- map(A, mutate_mapping, hcaes(x = Var1, y = Freq), drop = TRUE)
      #Tables[[names(Tabs)[i]]]
    }



    gp2 <- Tabs2 %>%
      select(Nombres, cat2, freq) %>%
      nest(-Nombres) %>%
      mutate(
        data = map(data, mutate_mapping, hcaes(name=cat2,x = cat2, y = round(freq,3)), drop = TRUE),
        data = map(data, list_parse)
      ) %>%
      rename(ttdata = data)

    gptot <- left_join(chigroup, gp2, by = "Nombres")







    Tabs.1 <- BaseCons


    Tables.1 <- list()

    Tabs2.1 <- data.frame(names(Tabs.1)[1],list(as.data.frame(table(Tabs.1[1]))),prop.table(table(Tabs.1[1])))
    colnames(Tabs2.1)=c("Nombres","cat","freq","cat2","prop")
    Tabs2.1 <- data.frame(Nombres=Tabs2.1$Nombres,cat=Tabs2.1$cat,freq=Tabs2.1$freq,cat2=paste(Tabs2.1$cat2,round(Tabs2.1$prop,2)))
    for (i in 2:ncol(Tabs.1)){
      Tabss.1=data.frame(names(Tabs.1)[i],data.frame(table(Tabs.1[i])),prop.table(table(Tabs.1[i])))
      colnames(Tabss.1)=c("Nombres","cat","freq","cat2","prop")
      Tabss.1 <- data.frame(Nombres=Tabss.1$Nombres,cat=Tabss.1$cat,freq=Tabss.1$freq,cat2=paste(Tabss.1$cat2,round(Tabss.1$prop,2)))

      Tabs2.1 <- rbind(Tabs2.1,Tabss.1)

      #Tables[[names(Tabs)[i]]] <- as.data.frame(table(Tabs[i]))

      #A <- map(A, mutate_mapping, hcaes(x = Var1, y = Freq), drop = TRUE)
      #Tables[[names(Tabs)[i]]]
    }



    gp2.1 <- Tabs2.1 %>%
      select(Nombres, cat2, freq) %>%
      nest(-Nombres) %>%
      mutate(
        data = map(data, mutate_mapping, hcaes(name=cat2,x = cat2, y = freq), drop = TRUE),
        data = map(data, list_parse)
      ) %>%
      rename(ttdata = data)

    chigroup.1=data.frame(chigroup,sd=sd(chigroup$Sum)/2)

    gptot.1 <- left_join(chigroup.1, gp2.1, by = "Nombres")




    gptot$Sum=round(gptot$Sum,2)


    highchart()%>%
      hc_add_series(gptot, type='column', hcaes(x=Nombres,y=Sum), color='#1A578F',name='ChiSq Distance')%>%
      hc_add_series(gptot.1, type='scatter', hcaes(x=Nombres,y=Sum+sd(chigroup$Sum)/4),
                    color='#A1A1A1',name='Consensus reference')%>%
      hc_xAxis(categories=chigroup$Nombres) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "<b>{point.key}</b>",
        pointFormatter = tooltip_chart(accesor = "ttdata",width = 350, height = 220,
                                       hc_opts = list(chart = list(type = "pie"),
                                                      xAxis = list(title = list(text = "lifeExp")))))%>%
      hc_yAxis(max=ylim)




}


  #print(TableCHI, quote = FALSE,row.names = FALSE)


}
