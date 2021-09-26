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
globalVariables(c("base","HTML","h2","var","qchisq","sd","abline","AC.SUM1...5.","AC.SUM1...8.","pchisq"))

#' @title Full Panel T2 Categorical
#'
#' @description A shiny panel complete with the multivariate control chart for categorical variables, the two ACM charts and the modality distance table. Within the dashboard, arguments such as type I error and dimensionality can be modified.
#' @param base Data set
#' @param IndK Character with the name of the column that specifies the partition of the data set in k tables.
#' @return A complete panel with the multivariate control chart for categorical variables, the two ACM charts and the modality distance table.
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
                                      withSpinner(highchartOutput('ACMconsolidate',  height = "450px"), type = 7, color='#C7D5EB')),
                               column(6,
                                      withSpinner(highchartOutput('ACMpoint',  height = "450px"), type = 7, color='#C7D5EB')),
                               fluidRow(align='center',
                                        h2('Difference between modalities - Chi square'),
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
          coornorm[[i]] <- NormalizacionAFM(colcoor[[i]])
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

        mu00 <- apply(General,2, 'mean')
        muii <- list()
        n <- list()
        for (i in 1:length(levels(groupFactor))){
          muii[[i]] <- apply(coornorm[[i]][,1:min(colnum)],2,'mean')
          n[[i]] <- nrow(coornorm[[i]])
        }

        t2 <- list()
        sigma <- var(General)

        for (i in 1:length(levels(groupFactor))){
          t2[[i]]=n[[i]]*(t(muii[[i]]-mu00)%*%solve(sigma)%*%(muii[[i]]-mu00))
        }
        T2 <- as.data.frame(as.matrix(t2))
        DtGraph <- data.frame(table=seq(1:nrow(T2)),hote=as.numeric(as.matrix(T2$V1)))

        LC <- qchisq(p=alpha,df=dim)
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
            hc_subtitle(text="T2 Hotelling")%>%
            hc_xAxis(categories=Categories)%>%
            hc_yAxis(max=YLIM,
                     plotLines = list(list(
                       value = LC,
                       color = '#1D4B5E',
                       width = 3,
                       zIndex = 4,
                       label = list(text = "",
                                    style = list( color = '#1D4B5E', fontWeight = 'bold' )))))%>%
            hc_annotations(
              list(labelOptions = list(y = 35, x = 0, backgroundColor = '#E6EEFF', borderColor = "#1D4B5E"),
                   labels = list(
                     list(style = list(color = '#1D4B5E', fontSize = 8),
                          useHTML = TRUE,
                          point = list(x = 1, y = LC+2*sd(DtGraph$hote), xAxis = 0, yAxis = 0),text = paste0("UL = ",round(LC,2), "<br/> alpha = ",alpha,"<br/> ARL = ",round(1/alpha)))
                   )
              )
            )
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

        chi <- sqrt(((((Point$AC_Point.colmass)*nrow(base))-((AC_Cons$colmass)*nrow(base)))^2)/((AC_Cons$colmass)*nrow(base)))
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
        TableCHI <- data.frame(Variable=AC_Cons$levelnames, `Chi-Squared`=chi, `val-p`=signif(valp,6),Signif=starInd)
        TableCHI
      }
      },digits=5)
      output$barChi <- renderHighchart({
        if (input$table=='BarChart'){
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

          chi <- sqrt(((((Point$AC_Point.colmass)*nrow(base))-((AC_Cons$colmass)*nrow(base)))^2)/((AC_Cons$colmass)*nrow(base)))

          valp=pchisq(chi,1,lower.tail=FALSE)
          TableChi <- data.frame(Variable=AC_Cons$levelnames, `Chi-Squared`=chi, `val-p`=valp)

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
            # hc_title(text='Difference between modalities - Chi square')%>%
            hc_subtitle(text="Signif. Codes 0 '***' 0.001 '**' 0.05 '*'")

        }
      })
    })
  runApp(app)
}
