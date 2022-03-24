#' @import ca
#' @import highcharter
#' @import dplyr
#' @import stringr
#' @import htmltools
#' @importFrom shiny column radioButtons textOutput checkboxInput fileInput fluidRow htmlOutput icon numericInput reactive renderPrint renderTable renderText renderUI runApp selectInput shinyApp sliderInput stopApp tableOutput tabPanel uiOutput withMathJax verbatimTextOutput
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard menuItem
#' @importFrom shinydashboard menuSubItem
#' @importFrom shinydashboard tabItem
#' @importFrom shinydashboard tabItems
#' @importFrom shinydashboard dashboardBody
#' @import shinydashboardPlus shinycssloaders
#'
globalVariables(c("base","HTML","h2","var","qchisq","sd","abline","AC.SUM1...5.","AC.SUM1...8."))

#' @title Full Panel T2 Qualitative
#'
#' @description A shiny panel complete with the multivariate control chart for qualitative variables, the two ACM charts and the modality distance table. Within the dashboard, arguments such as type I error and dimensionality can be modified.
#' @param base Data set
#' @param IndK Character with the name of the column that specifies the partition of the data set in k tables.
#' @return A complete panel with the multivariate control chart for qualitative variables, the two ACM charts and the modality distance table.
#' @examples
#' \dontrun{
#' data(Datak10Contaminated)
#' Full_Panel(Datak10Contaminated, "GroupLetter")
#' }
#' @export
Full_Panel <- function(base,IndK ) {

  diminitial=ncol(base)-2
  options <- unique(as.data.frame(base[,IndK]))[,1]
  # left_footer <- fluidRow(
  #   column(
  #     width = 6,
  #     align = "left",
  #     a(
  #       href = "http://www.fcnm.espol.edu.ec/",
  #       target = "_blank",
  #       img(src = "https://github.com/JavierRojasC/JavierRCam/blob/master/fcnm.png?raw=true", height = "30px"),
  #       class = "dropdown",
  #       title = "Facultad de Ciencias Naturales y Matematicas")
  #   )
  # )

  app <- list(
    ui = dashboardPage(
      # preloader = list(html = tagList(spin_three_bounce(), h3("cargando ...")), color = "#5dd0da"),

      title =  '' ,
      dashboardHeader(title = "T2Qv"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Multivariate Control Chart", tabName = "home", startExpanded = TRUE,icon = icon("unity")),

          #uiOutput("dimension"),
          numericInput("dim","Dimension", diminitial),
          numericInput("alpha","Type I Error", 0.0027)


        )),

      dashboardBody( tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #DEDEDE;
                                color: #12203C
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #D6EFFF;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #12203C;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #12203C;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #A8A8A8;

                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #8B8989;
                                color: #151515;
                                style:"font-family:verdana";
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #6F6F6F;
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #DDDDDD;
                                }

                             /* body */
                                 .skin-blue .main-body .content-wrapper, .right-side {
                                background-color: #F3F3F3;
                                 }

                                .box.box-solid.box-primary>.box-header{
  background: rgb(0, 129, 201);
  color: #57A184;
    font-size: 18px;
  font-weight; bold;
}

.box.box-solid.box-primary{
  font-family: OpenSans;
  font-size: 16px;
  text-align: left;
  color: #AA3B3B;
}

                                '))),
                     tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),

                     tabItems(
                       tabItem(tabName= "home",

                               fluidRow(align="center",
                                        withMathJax(h2('$$ T^2 Hotelling $$'))),
                               fluidRow(
                                 withSpinner(highchartOutput('ControlGraph',  height = "450px"), type = 7, color='#C7D5EB')),
                               box(title='Select Table:',width=7,
                                   selectInput('point',' ',options)),
                               fluidRow(align="center",
                                        h2("Comparison with Multiple Correspondence Analysis")),
                               column(6,
                                      withSpinner(highchartOutput('ACMconsolidate',  height = "550px"), type = 7, color='#C7D5EB')),
                               column(6,
                                      withSpinner(highchartOutput('ACMpoint',  height = "550px"), type = 7, color='#C7D5EB')),
                               fluidRow(align='center',
                                        numericInput("ylim","yLim", 5),
                                        h2('Chi-squared distance between the column masses of the k table and the consensus'),
                                        radioButtons('table', '',c('Table','BarChart'), inline = TRUE),
                                        tableOutput('tableChi'),
                                        highchartOutput('barChi'))
                       )
                     ))),
    dashboardFooter(
      left = NULL,
      right = NULL),

    server = function(input, output) {

      output$ControlGraph <- renderHighchart({
        dim=input$dim
        alpha=input$alpha
        IndK <- IndK
        base <- base
        interactive=TRUE

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
          colcoor[[i]] <- mjcatable(as.data.frame(Table[i]),dim)
        }

        NormalizacionAFM <- function(mjca){
          SVD <- svd(mjca)
          ACP1_VALUE <- (SVD$d[1])
          return(mjca/ACP1_VALUE)
        }
        coornorm <- list()
        for (i in 1:length(levels(groupFactor))){
          coornorm[[i]] <- NormalizacionAFM(abs(colcoor[[i]]))
          coornorm[[i]] - min(coornorm[[i]])/(max(coornorm[[i]]) - min(coornorm[[i]]))
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


        alpha2 <- 1-(1-alpha)^dim
        LC <- qchisq(p=alpha,df=dim, lower.tail = FALSE)
        if (max(DtGraph$hote)>LC){
          YLIM=max(DtGraph$hote)+sd(DtGraph$hote)
        } else {
          YLIM=LC+sd(DtGraph$hote)
        }

        Categories <- levels(groupFactor)
        if (interactive==FALSE){
          plot(DtGraph, type='l', main="Multivariate Control Chart", sub=paste0("UL = ",round(LC,2), ", alpha = ",alpha,", ARL = ",round(1/alpha)),xlab="k Table", ylab="T2 Hotelling",ylim=c(0,YLIM))+
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
      })
      # output$selectPoint <- renderUI({
      #
      #   IndK <- IndK
      #   base <- base
      #   Ind <- base%>% pull(IndK)
      #   Levels=unique(as.factor(Ind))
      #   selectInput('point',' ',Levels)
      # })

      output$dimension <- renderUI({

        IndK <- IndK
        base <- base
        diminitial=ncol(base)-2
        numericInput('dim','Dimension',diminitial)
      })

      output$ACMconsolidate <- renderHighchart({
        dim=input$dim
        #dim=9

        IndK <- IndK
        base <- base



        Table <- base

        Table <- Table[,!names(Table) %in% IndK, drop = F]


        AC <- mjca(Table)

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


      })
      output$ACMpoint <- renderHighchart({
        dim=input$dim
        IndK <- IndK
        base <- base
        dim1=ncol(base)-1
        Table <- vector("list", dim1)
        Ind <- base%>% pull(IndK)
        groupFactor=as.factor(Ind)
        title=paste("Point",input$point)
        PointTable <- input$point

        k_item=match(PointTable,levels(groupFactor))

        for (i in 1:length(levels(groupFactor))){
          Table[[i]] <- as.data.frame(subset(base, Ind==as.character(levels(groupFactor)[i])))
        }

        Tabl <- vector("list", dim1)
        for (i in 1:length(levels(groupFactor))){
          Tabl[[i]] <- Table[[i]][,!names(Table[[i]]) %in% IndK, drop = F]}

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
          colcoor[[i]] <- as.data.frame(mjcatable(as.data.frame(Tabl[i]),dim))
        }
        TabK <- as.data.frame(Tabl[k_item])
        AC <- mjca(TabK)

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
          hc_title(text=title)

      })

      output$tableChi <- renderTable({
        if (input$table=='Table'){
          dim=input$dim
          IndK <- IndK
          base <- base
          PointTable <- input$point
          dim1=ncol(base)-1

          BaseFilt <- as.data.frame(base%>%
                                      filter(base[,IndK]==PointTable))

          Table <- as.data.frame(BaseFilt[,-match(IndK,names(BaseFilt))])

          BaseCons <- as.data.frame(base[,-match(IndK,names(base))])


          AC_Point <- mjca(Table, dim)
          AC_Cons <- mjca(BaseCons, dim)

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
          names(chigroup) <- c("Variables","ChiSq")
          chigroup
      }
      },digits=5)
      output$barChi <- renderHighchart({
        if (input$table=='BarChart'){
          dim=input$dim
          IndK <- IndK
          base <- base
          names(base) <- str_replace(names(base), " ",".")

          PointTable <- input$point
          dim1=ncol(base)-1

          BaseFilt <- as.data.frame(base%>%
                                      filter(base[,IndK]==PointTable))

          Table <- as.data.frame(BaseFilt[,-match(IndK,names(BaseFilt))])

          BaseCons <- as.data.frame(base[,-match(IndK,names(base))])


          AC_Point <- mjca(Table, dim)
          AC_Cons <- mjca(BaseCons, dim)

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


          Tabs <- Table

          Tables <- list()

          Tabs2 <- data.frame(names(Tabs)[1],list(as.data.frame(table(Tabs[1]))),prop.table(table(Tabs[1])))
          colnames(Tabs2)=c("Nombres","cat","freq","cat2","prop")
          Tabs2 <- data.frame(Nombres=Tabs2$Nombres,cat=Tabs2$cat,freq=Tabs2$freq,cat2=paste(Tabs2$cat2,Tabs2$prop))
          for (i in 2:ncol(Tabs)){
            Tabss=data.frame(names(Tabs)[i],data.frame(table(Tabs[i])),prop.table(table(Tabs[i])))
            colnames(Tabss)=c("Nombres","cat","freq","cat2","prop")
            Tabss <- data.frame(Nombres=Tabss$Nombres,cat=Tabss$cat,freq=Tabss$freq,cat2=paste(Tabss$cat2,Tabss$prop))

            Tabs2 <- rbind(Tabs2,Tabss)

            #Tables[[names(Tabs)[i]]] <- as.data.frame(table(Tabs[i]))

            #A <- map(A, mutate_mapping, hcaes(x = Var1, y = Freq), drop = TRUE)
            #Tables[[names(Tabs)[i]]]
          }



          gp2 <- Tabs2 %>%
            select(Nombres, cat2, freq) %>%
            nest(-Nombres) %>%
            mutate(
              data = map(data, mutate_mapping, hcaes(name=cat2,x = cat2, y = freq), drop = TRUE),
              data = map(data, list_parse)
            ) %>%
            rename(ttdata = data)

          gptot <- left_join(chigroup, gp2, by = "Nombres")

          highchart()%>%
            hc_add_series(gptot, type='column', hcaes(x=Nombres,y=Sum), color='#1A578F',name='ChiSq Distance')%>%
            hc_xAxis(categories=chigroup$Nombres) %>%
            hc_tooltip(
              useHTML = TRUE,
              headerFormat = "<b>{point.key}</b>",
              pointFormatter = tooltip_chart(accesor = "ttdata",width = 350, height = 220,
                                             hc_opts = list(chart = list(type = "pie"),
                                                            xAxis = list(title = list(text = "lifeExp")))))




        }
      })
    })
  runApp(app)
}
