#rm(list=ls())
library(plyr)
library(data.table)
library(shiny)
library(sp)
library(spdep)
library(scales)
library(leaflet)
library(rgeos)
library(raster)
library(maptools)
library(ggplot2)
library(httr)
library(ape)
library(RCurl)
library(digest)
library(shinythemes)

url <- "http://api.cepesp.io/api/consulta/tse"

#mun <- readRDS("C:/Users/Jonny/Google Drive/Academic/FGV-SP/New Analysis 2017/mun2.rds")
mun <- readRDS("mun2.rds")
mun$GEOCOD6 <- as.numeric(substr(mun$GEOCOD,1,6))

#d_uniq <- read.csv("C:/Users/Jonny/Google Drive/Academic/FGV-SP/New Analysis 2017/May 2017/d_uniq_all_new.csv")
d_uniq <- read.csv("d_uniq_all_new.csv")
colnames(d_uniq)[colnames(d_uniq)=="ANO_ELEICAO"] <- "anoEleicao"
colnames(d_uniq)[colnames(d_uniq)=="UF"] <- "sigla_UF"

mouse <- d_uniq[1,]

#Year <- 2014
#State <- "SE"
#Party <-"PRB"
#Candidate <- "PASTOR JONY"
#Candidate <- "JOANA DARC"

### Load state voting totals
ui <- navbarPage("ZIKA VIRUS",id="nav",theme=shinytheme("flatly"),
                 tabPanel("Mapa",div(class="outer",
                                    tags$head(
                                      includeCSS("styles.css")
                                    ),
                                    
                                    tags$style(type="text/css",
                                               ".shiny-output-error { visibility: hidden; }",
                                               ".shiny-output-error:before { visibility: hidden; }"
                                    ),
                                    leafletOutput("map",width="100%",height="100%")),
                          bootstrapPage(absolutePanel(id = "note", class = "panel panel-default", fixed = TRUE,
                                                      draggable = TRUE, top = 60, left = "auto", right = 30, bottom = "auto",
                                                      width = 330, height = "auto",HTML('<button data-toggle="collapse" data-target="#demo">Info</button>'),
                                                      tags$div(id = 'demo',  class="collapse in",
                                                               htmlOutput("Note"))
                          ))
                 ),
                 tabPanel("Clusters",
                          column(width=4,""),
                          column(width=4,htmlOutput("Num_clusters"),tableOutput("Clusters_agg"),tableOutput("Clusters")),
                          column(width=4,leafletOutput("map_clusters",width="500px",height="400px"))
                 ),
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = FALSE, top = 120, left = 10, right = "auto", bottom = "auto",
                               width = 330, height = "auto",
                               h4("PESQUISAR"),
                               selectInput("State", 
                                           label = "Estado",
                                           choices = c("AC","AM","AL","AP","BA","CE","DF","ES","GO","MA","MS","MG","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO"),
                                           selected = "AC"),
                               uiOutput(""),
                               radioButtons("Colours",
                                            label = "Descrição:",
                                            choices = list("Areas de Concentração","Indice de Infestação"),
                                            selected = "Areas de Concentração"),
                               htmlOutput("Result"),
                               htmlOutput("G_Index"),
                               htmlOutput("moran")
                 )
)





server <- function(input, output, session) {
  
  state_totals <- reactive({
    vars_state <- list("UF","ANO_ELEICAO","QTDE_VOTOS")
    names(vars_state) <- rep("selected_columns[]",length(vars_state))
    filter <- list()
    consulta_state <- append(append(list(cached=TRUE,anos=2014,uf="all",agregacao_regional=2,agregacao_politica=2,cargo=6),vars_state),filter)
    #consulta_state <- append(append(list(cached=TRUE,anos=Year,uf="all",agregacao_regional=2,agregacao_politica=2,cargo=6),vars_state),filter)
    state_temp <- content(GET(url,query=consulta_state))
    state_temp <- state_temp[,c("UF","QTDE_VOTOS")]
    colnames(state_temp)[colnames(state_temp)=="QTDE_VOTOS"] <- "Tot_State"
    state_totals <- state_temp
  })
  
  mun_totals <- reactive({
    ### Load municipal voting totals
    vars_mun <- list("UF","ANO_ELEICAO","COD_MUN_TSE","QTDE_VOTOS")
    names(vars_mun) <- rep("selected_columns[]",length(vars_mun))
    filter <- list("columns[0][name]"="UF","columns[0][search][value]"=input$State)
    #filter <- list("columns[0][name]"="UF","columns[0][search][value]"=State)
    consulta_mun <- append(append(list(cached=TRUE,anos=2014,uf=input$State,agregacao_regional=6,agregacao_politica=2,cargo=6),vars_mun),filter)
    #consulta_mun <- append(append(list(cached=TRUE,anos=Year,uf=State,agregacao_regional=6,agregacao_politica=2,cargo=6),vars_mun),filter)
    
    mun_temp <- content(GET(url,query=consulta_mun))
    mun_temp <- mun_temp[,c("COD_MUN_TSE","QTDE_VOTOS")]
    colnames(mun_temp)[colnames(mun_temp)=="QTDE_VOTOS"] <- "Tot_Mun"
    mun_totals <- mun_temp
  })
  
  d <- reactive({
    vars <- list("UF","NUMERO_PARTIDO","COD_MUN_TSE","ANO_ELEICAO","QTDE_VOTOS","UF","NUMERO_CANDIDATO","SIGLA_PARTIDO","NUMERO_PARTIDO","NOME_URNA_CANDIDATO","DESC_SIT_TOT_TURNO","COD_MUN_IBGE")
    names(vars) <- rep("selected_columns[]",length(vars))
    filter <- list("columns[0][name]"="UF","columns[0][search][value]"=input$State,"columns[1][name]"="NUMERO_PARTIDO","columns[1][search][value]"=switch("PMDB","PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90))
    #filter <- list("columns[0][name]"="UF","columns[0][search][value]"=State,"columns[1][name]"="NUMERO_PARTIDO","columns[1][search][value]"=switch(Party,"PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90))
    
    consulta <- append(append(list(cached=TRUE,anos = 2014,uf=input$State,party=switch("PMDB","PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),agregacao_regional=6, agregacao_politica=2, cargo=6),vars),filter)
    #consulta <- append(append(list(cached=TRUE,anos = Year,uf=State,party=switch(Party,"PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),agregacao_regional=6, agregacao_politica=2, cargo=6),vars),filter)
    d <- content(GET(url,query=consulta))
    d <- data.table(d)

    #Merge in CODMUN.IBGE Codes
    #d <- merge(d,de_para,by.x="COD_MUN_TSE",by.y="TSE_2004")
    
    #Trim IBGE Codes to 6 digits
    d$COD_MUN_IBGE <- as.numeric(substr(d$COD_MUN_IBGE,1,6))
    
    #Ideally will be faster when can request specific state
    setkeyv(d,c('ANO_ELEICAO','COD_MUN_IBGE','NUMERO_CANDIDATO'))
    
    #### Aggregations
    d <- merge(d,mun_totals(),by="COD_MUN_TSE")
    #d <- merge(d,mun_totals,by="COD_MUN_TSE")
    d <- merge(d,state_totals(),by="UF")
    #d <- merge(d,state_totals,by="UF")
    
    #d[,Tot_Mun := sum(QTDE_VOTOS),by=.(ANO_ELEICAO,COD_MUN_IBGE)]
    #d[,Tot_State := sum(QTDE_VOTOS),by=.(ANO_ELEICAO,UF)]
    d[,Tot_Deputado := sum(QTDE_VOTOS),by=.(ANO_ELEICAO,UF,NUMERO_CANDIDATO)] #Seems wrong; is titulo repeated across states??
    d[,Mun_Vote_Share := QTDE_VOTOS/Tot_Mun]
    
    #### G-Index Calcs
    d[,G_temp := (QTDE_VOTOS/Tot_Deputado - Tot_Mun/Tot_State)^2]
    d[,G_Index := sum(G_temp),by=.(ANO_ELEICAO,UF,NUMERO_CANDIDATO)] #Correct? CHECK
    
    #### LQ Calcs
    d[,LQ := (QTDE_VOTOS/Tot_Deputado)/(Tot_Mun/Tot_State),by=.(ANO_ELEICAO,UF,NUMERO_CANDIDATO)] #Correct?
    
    #Remove NULO line from selectable candidates, though is included in calculations of total statewide and municipal votes above
    d <- d[NOME_URNA_CANDIDATO!="#NULO#"]
  })
  
  output$cand <- renderUI({
    candidates <- sort(unique(d()[,"NOME_URNA_CANDIDATO"])[[1]])
    #candidates <- sort(unique(d[,"NOME_URNA_CANDIDATO"])[[1]])
  })
  
  

  
  mun_state_contig <- reactive({
    names(mun)[which(names(mun)=="UF")] <- "UF_shape"
    mun_state <- mun[mun$UF_shape==input$State,]
    #mun_state <- mun[mun$UF_shape==State,]
    state_nb <- poly2nb(mun_state)
    if (any(card(state_nb)==0)){
      mun_state_contig <- mun_state[-which(card(state_nb)==0),]  
    } else {
      mun_state_contig <- mun_state
    }
  })
  
  dz3 <- reactive({
    dz2 <- d()[NOME_URNA_CANDIDATO==sort(unique(d()[,"NOME_URNA_CANDIDATO"])[[1]])[1]]
    #dz2 <- d[NOME_URNA_CANDIDATO==Candidate]
    dz3_temp <- merge(mun_state_contig(),dz2, by.x="GEOCOD6",by.y="COD_MUN_IBGE",all.x=TRUE,all.y=FALSE)
    #dz3_temp <- merge(mun_state_contig,dz2, by.x="GEOCOD6",by.y="COD_MUN_IBGE",all.x=TRUE,all.y=FALSE)
    dz3_temp@data[is.na(dz3_temp@data[,"LQ"])==TRUE,"LQ"] <- 0
    dz3_temp@data[is.na(dz3_temp@data[,"QTDE_VOTOS"])==TRUE,"QTDE_VOTOS"] <- 0
    dz3 <- dz3_temp
  })
  
  state_nb2listw <- reactive({
    state_nb2 <- poly2nb(mun_state_contig()) #Necessary to remove 'islands' as causes problems
    #state_nb2 <- poly2nb(mun_state_contig) #Necessary to remove 'islands' as causes problems
    state_nb2listw <- nb2listw(state_nb2,zero.policy=TRUE)
  })
  
  dz5 <- reactive({
    dz4 <- dz3()
    #dz4 <- dz3
    
    lisa <- as.data.frame(localmoran(dz4$LQ,state_nb2listw()))
    #lisa <- as.data.frame(localmoran(dz4$LQ,state_nb2listw))
    
    dz4$LISA_I <- lisa[,"Ii"]
    dz4$LISA_p <- lisa[,"Pr(z > 0)"]
    dz4$LQ_stdzd <- as.vector(scale(dz4$LQ))
    dz4$LQ_stdzd_lag <- lag.listw(state_nb2listw(),dz4$LQ_stdzd, NAOK=TRUE) #NAOK here helps or hinders?
    #dz4$LQ_stdzd_lag <- lag.listw(state_nb2listw,dz4$LQ_stdzd, NAOK=TRUE)
    
    dz4$category <- "Insignificant"
    dz4$category[dz4$LISA_p<0.05 & dz4$LQ_stdzd>=0 & dz4$LQ_stdzd_lag>=0] <- "High-High"
    dz4$category[dz4$LISA_p<0.05 & dz4$LQ_stdzd>=0 & dz4$LQ_stdzd_lag<=0] <- "High-Low"
    dz4$category[dz4$LISA_p<0.05 & dz4$LQ_stdzd<=0 & dz4$LQ_stdzd_lag>=0] <- "Low-High"
    dz4$category[dz4$LISA_p<0.05 & dz4$LQ_stdzd<=0 & dz4$LQ_stdzd_lag<=0] <- "Low-Low"
    dz4$category <- as.factor(dz4$category)
    dz5 <- dz4
  })
  
  state_shp <- reactive({
    state_shp <- unionSpatialPolygons(dz5(),IDs=dz5()@data[,"UF_shape"])
    #state_shp <- unionSpatialPolygons(dz5,IDs=dz5@data[,"UF_shape"])
  })
  
  output$map <- renderLeaflet({
    if (input$Colours=="Areas de Concentração"){
      pal <- colorBin(palette=c("white","light blue","#fcbba1","#fb6a4a","#ef3b2c","#cb181d"),domain=c(0,100), bins=c(0,0.01,1,5,10,50,100), na.color="white")
    } else {
      pal <- colorBin(palette=c("light blue","white","dark red"),domain=c(0,100), bins=c(100,16,8,2,1,0.5,0.1,0.05,0), na.color="white")
    }
    
      popup_text <- paste0("<br> Índice de Infestação Predial: ",dz5()@data[,"Tot_Mun"]," (",round((dz5()@data[,"Tot_Mun"]/dz5()@data[,"Tot_State"])*100,1),"% da Totalidade do Estado)")
    #popup_text <- paste0(dz5@data[,"NOME"],"<br> Valid Votes: ",dz5@data[,"Tot_Mun"]," (",round((dz5@data[,"Tot_Mun"]/dz5@data[,"Tot_State"])*100,1),"% of State Total)","<br>",dz5@data[,"NOME_URNA_CANDIDATO"]," received ",dz5@data[,"QTDE_VOTOS"]," votes (",round((dz5@data[,"QTDE_VOTOS"]/dz5@data[,"Tot_Deputado"])*100,1),"% of their Statewide Total)","<br> QL Score: ",round(dz5@data[,"LQ"],3))
    
    popup_text_hihi <- paste0(dz5()@data[dz5()@data$category=="High-High","NOME"],"<br> Valid Votes: ",dz5()@data[dz5()@data$category=="High-High","Tot_Mun"]," (",round((dz5()@data[dz5()@data$category=="High-High","Tot_Mun"]/dz5()@data[dz5()@data$category=="High-High","Tot_State"])*100,1),"% of State Total)","<br>",dz3()@data[,"NOME_URNA_CANDIDATO"]," received ",dz5()@data[dz5()@data$category=="High-High","QTDE_VOTOS"]," votes (",round((dz5()@data[dz5()@data$category=="High-High","QTDE_VOTOS"]/dz5()@data[dz5()@data$category=="High-High","Tot_Deputado"])*100,1),"% of their Statewide Total)","<br> QL Score: ",round(dz5()@data[dz5()@data$category=="High-High","LQ"],3))
    
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% clearBounds() %>% addPolygons(data=state_shp(),fillOpacity=0,weight=3,color="black",fillColor=NULL) %>% addPolygons(data=dz5(), layerId=dz5()@data[,"LQ"],fillOpacity=0.8,weight=0.1,color=NA,fillColor=pal(dz5()@data[,"LQ"]), popup=popup_text) %>% addLegend(position="bottomright", pal=pal,values=round(dz5()@data[,"LQ"],0),opacity=0.8)  %>% addPolygons(data=dz5()[dz5()@data$category=="High-High",], layerId=dz5()@data[dz5()@data$category=="High-High",],fillOpacity=0,weight=2,color="green",stroke=TRUE,popup=popup_text_hihi)
    #leaflet() %>% addProviderTiles("CartoDB.Positron") %>% clearBounds() %>% addPolygons(data=state_shp,fillOpacity=0,weight=3,color="black",fillColor=NULL) %>% addPolygons(data=dz5, layerId=dz5@data[,"LQ"],fillOpacity=0.8,weight=0.1,color=NA,fillColor=pal(dz5@data[,"LQ"]), popup=popup_text) %>% addLegend(position="bottomleft", pal=pal,values=dz5@data[,"LQ"],opacity=0.8)  %>% addPolygons(data=dz5[dz5@data$category=="High-High",], layerId=dz5@data[dz5@data$category=="High-High",],fillOpacity=0,weight=3,color="green",stroke=TRUE)
  })
  
  clusters <- reactive({
    #Identify clusters
    dz5_HH <- dz5()[dz5()$category=="High-High",]
    #dz5_HH <- dz5[dz5$category=="High-High",]
    if (dim(dz5_HH)[1]!=0){
      state_nb2_HH <- poly2nb(dz5_HH)
      state_nb2listw <- nb2listw(state_nb2_HH,zero.policy=TRUE)
      clusters <- gUnion(dz5_HH,dz5_HH)  
    } else {
      clusters <- NULL
    }
  })
  
  clusters_sp <- reactive({
    if (!(is.null(clusters()))){
      clusters_sep <- slot(clusters()@polygons[[1]],"Polygons")
      #clusters_sep <- slot(clusters@polygons[[1]],"Polygons")
      
      polygons_list <- list()
      for (i in 1:length(clusters_sep)){
        polygons_list[[i]] <- Polygons(list(clusters_sep[[i]]),"test")  
        polygons_list[[i]]@ID <- paste0(i)
      }
      clusters_sp <- SpatialPolygons(polygons_list)
    }
  })
  
  clusters_sp_cent_table <- reactive({
    if (!(is.null(clusters()))){
      clusters_sp_cent <- gCentroid(clusters_sp(),byid=TRUE)
      #clusters_sp_cent <- gCentroid(clusters_sp,byid=TRUE)
      clusters_sp_cent_table_temp <- as.data.frame(clusters_sp_cent@coords)
      clusters_sp_cent_table_temp$Cluster_num <- rownames(clusters_sp_cent_table_temp)
      clusters_sp_cent_table <- clusters_sp_cent_table_temp
    }
  })
  
  clusters_list <- reactive({
    if (!(is.null(clusters()))){
      clusters_list_temp <- list()
      num_clust <- length(clusters_sp())
      #num_clust <- length(clusters_sp)
      
      for (i in 1:num_clust){
        clusters_list_temp[[i]] <- intersect(dz5()[dz5()$category=="High-High",],clusters_sp()[i])
        #clusters_list_temp[[i]] <- intersect(dz5[dz5$category=="High-High",],clusters_sp[i])
        clusters_list_temp[[i]]@data$Cluster_Num <- i
      }
      clusters_list <- clusters_list_temp
    }
  })
  
  output$Num_clusters <- renderUI({
    if (!(is.null(clusters()))){
      Num_clusters <- paste0("<b> Number of High-High Clusters: ",length(clusters_list()),"<b>")
      #Num_clusters <- paste0("<b> Number of High-High Clusters: ",length(clusters_list),"<b>")
      HTML(Num_clusters)
    } else {
      HTML(paste0("<b> No clusters <b>"))
    }
  })
  
  cluster_table <- reactive({
    if (!(is.null(clusters()))){
      clusters_table_temp <- rbind.fill(lapply(clusters_list(),slot,'data'))
      #clusters_table_temp <- rbind.fill(lapply(clusters_list,slot,'data'))
      clusters_table_temp$pct_votes_from_mun <- clusters_table_temp$QTDE_VOTOS/clusters_table_temp$Tot_Deputado
      clusters_table_temp <- clusters_table_temp[,c("Cluster_Num","NOME","QTDE_VOTOS","pct_votes_from_mun","LQ")]
      clusters_table_temp$pct_votes_from_mun <- round(clusters_table_temp$pct_votes_from_mun*100,1)
      clusters_table_temp$LQ <- round(clusters_table_temp$LQ,1)
      cluster_table <- clusters_table_temp
    }
  })
  
  output$Clusters <- renderTable({
    if (!(is.null(clusters()))){
      table_temp <- cluster_table()
      #table_temp <- cluster_table
      colnames(table_temp) <- c("Cluster Number","Municipality","Votes","% Candidate Votes","LQ")
      table_temp[,"Votes"] <- round(table_temp[,"Votes"],0)
      table_temp[,"% Candidate Votes"] <- round(table_temp[,"% Candidate Votes"],1)
      Clusters <- table_temp
    }
  })
  
  output$Clusters_agg <- renderTable({
    if (!(is.null(clusters()))){
      agg <- as.data.frame(as.data.table(cluster_table())[,.(sum(QTDE_VOTOS), sum(pct_votes_from_mun)),by=Cluster_Num])
      #agg <- as.data.frame(as.data.table(cluster_table)[,.(sum(QTDE_VOTOS), sum(pct_votes_from_mun)),by=Cluster_Num])
      colnames(agg) <- c("Cluster Number","Total de casos registrados","Total % Candidate Votes")
      agg[,"Total de casos registrados"] <- round(agg[,"Total de casos registrados"],0)
      agg[,"Total % Candidate Votes"] <- round(agg[,"Total % Candidate Votes"],1)
      Clusters_agg <- agg
    }
  })
  
  output$map_clusters <- renderLeaflet({
    if (input$Colours=="Areas de Concentração"){
      pal <- colorBin(palette=c("white","light blue","#fcbba1","#fb6a4a","#ef3b2c","#cb181d"),domain=c(0,100), bins=c(0,0.01,1,5,10,50,100), na.color="white")
    } else {
      pal <- colorBin(palette=c("light blue","white","dark red"),domain=c(0,100), bins=c(100,16,8,2,1,0.5,0.1,0.05,0), na.color="white")
    }
    
    #markers <- awesomeIcons(icon='record',markerColor = 'blue',library='glyphicon')
    
    #popup_text <- paste0(dz5()@data[,"NOME"],"<br> Valid Votes: ",dz5()@data[,"Tot_Mun"]," (",round((dz5()@data[,"Tot_Mun"]/dz5()@data[,"Tot_State"])*100,1),"% of State Total)","<br>",dz3()@data[,"NOME_URNA_CANDIDATO"]," received ",dz5()@data[,"QTDE_VOTOS"]," votes (",round((dz5()@data[,"QTDE_VOTOS"]/dz5()@data[,"Tot_Deputado"])*100,1),"% of their Statewide Total)","<br> QL Score: ",round(dz5()@data[,"LQ"],3))
    if (!(is.null(clusters()))){
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        clearBounds() %>% 
        addPolygons(data=state_shp(),fillOpacity=0,weight=3,color="black",fillColor=NULL) %>% 
        addPolygons(data=dz5()[dz5()@data$category=="High-High",], layerId=dz5()@data[dz5()@data$category=="High-High",],fillOpacity=0,weight=3,color="green",stroke=TRUE) %>% 
        #addAwesomeMarkers(data=clusters_sp_cent_table(),~x,~y,label = ~Cluster_num,icon=markers,popup=paste0("Cluster #",clusters_sp_cent_table()[,"Cluster_num"]),clusterOptions = markerClusterOptions())          
        addMarkers(data=clusters_sp_cent_table(),~x,~y)#,label = ~Cluster_num)#,labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize="25px"))
      #addLabelOnlyMarkers(data=clusters_sp_cent_table(),~x*0.99,~y,label = ~Cluster_num,labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize="25px"))
    } else {
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        clearBounds() %>% 
        addPolygons(data=state_shp(),fillOpacity=0,weight=3,color="black",fillColor=NULL)
    }
    #leaflet() %>% addProviderTiles("CartoDB.Positron") %>% clearBounds() %>% addPolygons(data=state_shp,fillOpacity=0,weight=3,color="black",fillColor=NULL) %>% addPolygons(data=dz5[dz5@data$category=="High-High",], layerId=dz5@data[dz5@data$category=="High-High",],fillOpacity=0,weight=3,color="green",stroke=TRUE) %>%  addLabelOnlyMarkers(data=clusters_sp_cent_table,~x*0.99,~y,label = ~Cluster_num,labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize="15px"))
    
    #%>% addPolygons(data=dz5(), layerId=dz5()@data[,"LQ"],fillOpacity=0.8,weight=0.1,color=NA,fillColor=pal(dz5()@data[,"LQ"]), popup=popup_text)  %>% addLegend(position="bottomleft", pal=pal,values=dz5()@data[,"LQ"],opacity=0.8)     
    
  })
  
  
  d_G <- reactive({
    d_G <- d()[,unique(G_Index),by=.(UF,NUMERO_CANDIDATO,NOME_URNA_CANDIDATO,NUMERO_PARTIDO,DESC_SIT_TOT_TURNO)]
    #d_G <- d[,unique(G_Index),by=.(UF,NUMERO_CANDIDATO,NOME_URNA_CANDIDATO,NUMERO_PARTIDO,DESC_SIT_TOT_TURNO)]
  })
  
  output$Result <- renderUI({
    str_Result <- paste0("Resultado:  <br> Total de casos registrados: ",unique(dz3()@data$Tot_Deputado[is.na(dz3()@data$Tot_Deputado)==FALSE])," <br> % de Casos por Região: ",round((unique(dz3()@data$Tot_Deputado[is.na(dz3()@data$Tot_Deputado)==FALSE])/unique(dz3()@data$Tot_State[is.na(dz3()@data$Tot_State)==FALSE]))*100,1),"%")
    #str_Result <- paste0("Resultado: ",unique(dz3@data$DESC_SIT_TOT_TURNO[is.na(dz3@data$DESC_SIT_TOT_TURNO)==FALSE])," <br> Votos Total: ",unique(dz3@data$Tot_Deputado[is.na(dz3@data$Tot_Deputado)==FALSE])," <br> Percentagem de Votos: ",round((unique(dz3@data$Tot_Deputado[is.na(dz3@data$Tot_Deputado)==FALSE])/unique(dz3@data$Tot_State[is.na(dz3@data$Tot_State)==FALSE]))*100,1),"%")
    HTML(str_Result)
  })
  
  output$G_Index <- renderUI({
    str_G_Index <- paste0("<b> G Index: ",round(unique(dz3()@data$G_Index[is.na(dz3()@data$G_Index)==FALSE]),3),"<b>")
    #str_G_Index <- paste0("<b> G Index: ",round(unique(dz3@data$G_Index[is.na(dz3@data$G_Index)==FALSE]),3),"<b>")
    HTML(str_G_Index)
  })
  
  output$chart_LQ <- renderPlot({
    ggplot() + geom_density(data=dz3()@data,aes(x=LQ),fill="light blue",colour=NA,alpha=0.5) + xlab("Log of QL Score") + theme_classic() + ylab("Density") + scale_x_log10()
    #ggplot() + geom_density(data=dz3@data,aes(x=LQ),fill="purple",colour=NA,alpha=0.5) + xlab("Log of QL Score") + theme_classic() + ylab("Density") + scale_x_log10()
  })
  
  output$chart_scatter <- renderPlot({
    ggplot() + geom_point(aes(x=dz3()@data$Tot_Mun,y=dz3()@data$LQ),color="dark green")  + xlab("Log of Municipal Voting Population") + ylab("LQ Score") + theme_classic() + scale_x_log10()
    #ggplot() + geom_point(aes(x=dz3@data$Tot_Mun,y=dz3@data$LQ),color="dark green")  + xlab("Log of Municipal Voting Population") + ylab("LQ Score") + theme_classic() + scale_x_log10()
  })
  
  output$Note <- renderUI({
    note <- paste0("")
    HTML(note)
  })
  
  output$moran <- renderUI({
    dz3_nb <- poly2nb(dz3())
    #dz3_nb <- poly2nb(dz3)
    dz3_nb_list <- nb2listw(dz3_nb,zero.policy=TRUE)
    moran <- moran(dz3()$QTDE_VOTOS,dz3_nb_list,n=length(dz3_nb),Szero(dz3_nb_list),zero.policy=TRUE,NAOK=TRUE)  
    #moran <- moran(dz3$QTDE_VOTOS,dz3_nb_list,n=length(dz3_nb),Szero(dz3_nb_list),zero.policy=TRUE,NAOK=TRUE)  
    str_moran <- paste0("<b> Moran's I: ",round(unique(moran$I),3),"<b>")
    HTML(str_moran)
  })
  
  #  output$moran2 <- renderUI({
  #    centroids <- gCentroid(dz3(),byid=TRUE)
  #    dists <- as.matrix(dist(cbind(centroids$x, centroids$y)))
  #    dists <- 1/dists
  #    diag(dists) <- 0
  
  #    moran2 <- Moran.I(dz3()$LQ,dists)
  #    str_moran <- paste0("<b> Moran's I (Inv Dist): ",round(unique(moran2$observed),3),"<b>")
  #    HTML(str_moran)
  #  })
  
  output$quadrant <- renderPlot({
    ggplot() + 
      geom_point(data=d_uniq[d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State,],aes(x=G_Index,y=MoranI,size=V2),color="blue",alpha=0.2)+ 
      geom_point(data=d_uniq[d_uniq$NOME_URNA_CANDIDATO!=sort(unique(d()[,"NOME_URNA_CANDIDATO"])[[1]])[1] & d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State & d_uniq$Party_Number==switch("PMDB","PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),],aes(x=G_Index,y=MoranI,size=V2),color="red",alpha=0.8) + 
      geom_point(data=d_uniq[d_uniq$NOME_URNA_CANDIDATO==sort(unique(d()[,"NOME_URNA_CANDIDATO"])[[1]])[1] & d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State & d_uniq$Party_Number==switch("PMDB","PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),],aes(x=G_Index,y=MoranI,size=V2),color="dark green",alpha=1) + 
      theme_classic() + 
      geom_vline(xintercept=median(d_uniq[d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State,"G_Index"],na.rm=TRUE),lty=2) +
      geom_hline(yintercept=median(d_uniq[d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State,"MoranI"],na.rm=TRUE),lty=2) 
    #ggplot() + geom_point(data=d_uniq[d_uniq$NOME_URNA_CANDIDATO==Candidate & d_uniq$anoEleicao==Year & d_uniq$sigla_UF==State & d_uniq$Party_Number==switch(Party,"PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),],aes(x=G_Index,y=MoranI,size=V2),color="dark green",alpha=1) + geom_point(data=d_uniq[d_uniq$anoEleicao==Year & d_uniq$sigla_UF==State,],aes(x=G_Index,y=MoranI,size=V2),color="blue",alpha=0.2) + geom_point(data=d_uniq[d_uniq$NOME_URNA_CANDIDATO!=Candidate & d_uniq$anoEleicao==Year & d_uniq$sigla_UF==State & d_uniq$Party_Number==switch(Party,"PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),],aes(x=G_Index,y=MoranI,size=V2),color="red",alpha=0.6) + theme_classic() + geom_vline(xintercept=median(d_uniq[d_uniq$anoEleicao==Year & d_uniq$sigla_UF==State,"G_Index"],na.rm=TRUE),lty=2) + geom_hline(yintercept=median(d_uniq[d_uniq$anoEleicao==Year & d_uniq$sigla_UF==State,"MoranI"],na.rm=TRUE),lty=2) 
  })
  
  mouse <- reactive({
    if (is.null(input$plot_click)){
      mouse_temp  <- d_uniq[d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State & d_uniq$NOME_URNA_CANDIDATO==sort(unique(d()[,"NOME_URNA_CANDIDATO"])[[1]])[1],][1,]
    } else {
      mouse_temp <- nearPoints(d_uniq[d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State,],input$plot_click,threshold=10,maxpoints=1)
      #nearPoints(d_uniq[d_uniq$ANO_ELEICAO==Year & d_uniq$UF==State,],plot_click,threshold=10,maxpoints=1)
      #mouse <- d_uniq[d_uniq$anoEleicao==Year & d_uniq$sigla_UF==State,][1,]
    }
    mouse <- mouse_temp
  })
  
  G_Quadrant <- reactive({
    if(d_uniq[d_uniq$NUMERO_CANDIDATO==mouse()$NUMERO_CANDIDATO & d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State & d_uniq$Party_Number==switch("PMDB","PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),"G_Index"]>median(d_uniq[d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State,"G_Index"],na.rm=TRUE)){
      G_Quadrant_temp <-"above"
    } else {
      G_Quadrant_temp <- "below"
    }
    G_Quadrant <- G_Quadrant_temp
  })
  
  G_desc <- reactive({
    if(d_uniq[d_uniq$NUMERO_CANDIDATO==mouse()$NUMERO_CANDIDATO & d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State & d_uniq$Party_Number==switch("PMDB","PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),"G_Index"]>median(d_uniq[d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State,"G_Index"],na.rm=TRUE)){
      G_desc_temp <-"concentrated"
    } else {
      G_desc_temp <- "dispersed"
    }
    G_desc <- G_desc_temp
  })
  
  Moran_Quadrant <- reactive({
    if(d_uniq[d_uniq$NUMERO_CANDIDATO==mouse()$NUMERO_CANDIDATO & d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State & d_uniq$Party_Number==switch("PMDB","PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),"MoranI"]>median(d_uniq[d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State,"MoranI"],na.rm=TRUE)){
      Moran_Quadrant_temp <-"above"
    } else {
      Moran_Quadrant_temp <- "below"
    }
    Moran_Quadrant <- Moran_Quadrant_temp
  })
  
  Moran_desc <- reactive({
    if(d_uniq[d_uniq$NUMERO_CANDIDATO==mouse()$NUMERO_CANDIDATO & d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State & d_uniq$Party_Number==switch("PMDB","PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90),"MoranI"]>median(d_uniq[d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State,"MoranI"],na.rm=TRUE)){
      Moran_desc_temp <-"contiguous"
    } else {
      Moran_desc_temp <- "non-contiguous"
    }
    Moran_desc <- Moran_desc_temp
    
  })
  
  mouse_cand <- reactive({
    d_uniq[d_uniq$NUMERO_CANDIDATO==mouse()$NUMERO_CANDIDATO & d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State,"NOME_URNA_CANDIDATO"]
  })
  
  classify_note_text <- reactive({
    classify_note_text <- paste0("Each point represents a <font color=\"blue\"> Candidate </font> in this election, with the size proportionate to their total number of votes received. <font color=\"red\"> Red </font> points indicate votes for the selected party. The <font color=\"green\"> Green </font> point is the selected candidate. Click on <font color=\"red\">Red </font> or <font color=\"green\"> Green </font> points to view the distribution of QL scores for that candidate. <br> <br>")
  })
  
  classify_note_text_2 <- reactive({
    classify_note_text_2 <- paste0("The currently selected candidate (on the chart above), ", mouse_cand() ," has a G-Index <b>",  G_Quadrant(),"</b> the median and a Moran's I <b>", Moran_Quadrant(), "</b> the median, indicating that the candidate's support is more <b>",G_desc(),"</b> and <b>",Moran_desc(),"</b> than average.")
  })
  
  output$Classify_Note <- renderUI({
    Classify_Note <- HTML(classify_note_text())
  })
  
  output$Classify_Note2 <- renderUI({
    Classify_Note2 <- HTML(classify_note_text_2())
  })
  
  dy3 <- reactive({
    dy2 <- d()[NUMERO_CANDIDATO==mouse()$NUMERO_CANDIDATO]
    #dy2 <- d2()[UF==input$State & NUMERO_PARTIDO==switch("PMDB","PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90) & NUMERO_CANDIDATO==mouse()$NUMERO_CANDIDATO]
    #dy2 <- d[NOME_URNA_CANDIDATO==Candidate]
    #mun <- mun[mun$UF_shape==input$State,]
    #mun <- mun[mun$UF_shape==State,]
    dy3_temp <- merge(mun_state_contig(),dy2, by.x="GEOCOD6",by.y="CODMUN.IBGE",all.x=TRUE)
    #dy3_temp <- merge(mun_state_contig,dy2, by.x="GEOCOD6",by.y="CODMUN.IBGE",all.x=TRUE)
    #dy3_temp@data[is.na(dy3_temp@data[,"LQ"])==TRUE,"LQ"] <- 0
    #dy3_temp@data[is.na(dy3_temp@data[,"QTDE_VOTOS"])==TRUE,"QTDE_VOTOS"] <- 0
    dy3 <- dy3_temp
  })
  
  output$map_selected <- renderLeaflet({
    pal <- colorBin(palette=c("white","light blue","#fcbba1","#fb6a4a","#ef3b2c","#cb181d"),domain=c(0,100), bins=c(100,50,10,5,1,0.01,0), na.color="white")
    popup_text <- paste0(dy3()@data[,"NOME"],"<br> Valid Votes: ",dy3()@data[,"Tot_Mun"]," (",round((dy3()@data[,"Tot_Mun"]/dy3()@data[,"Tot_State"])*100,1),"% of State Total)","<br>",dy3()@data[,"NOME_URNA_CANDIDATO"]," received ",dy3()@data[,"QTDE_VOTOS"]," votes (",round((dy3()@data[,"QTDE_VOTOS"]/dy3()@data[,"Tot_Deputado"])*100,1),"% of their Statewide Total)","<br> QL Score: ",round(dy3()@data[,"LQ"],3))
    #popup_text <- paste0(dy3@data[,"NOME"],"<br> Valid Votes: ",dy3@data[,"Tot_Mun"]," (",round((dy3@data[,"Tot_Mun"]/dy3@data[,"Tot_State"])*100,1),"% of State Total)","<br>",dy3@data[,"NOME_URNA_CANDIDATO"]," received ",dy3@data[,"QTDE_VOTOS"]," votes (",round((dy3@data[,"QTDE_VOTOS"]/dy3@data[,"Tot_Deputado"])*100,1),"% of their Statewide Total)","<br> QL Score: ",round(dy3@data[,"LQ"],3))
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>% clearBounds() %>% addPolygons(data=state_shp(),fillOpacity=0,weight=3,color="black",fillColor=NULL) %>% addPolygons(data=dy3(), layerId=dy3()@data[,"LQ"],fillOpacity=0.8,weight=0.1,color=NA,fillColor=pal(dy3()@data[,"LQ"]), popup=popup_text) %>% addLegend(position="bottomleft", pal=pal,values=dy3()@data[,"LQ"],opacity=0.8) 
    #leaflet() %>% addProviderTiles("CartoDB.Positron") %>% clearBounds() %>% addPolygons(data=state_shp,fillOpacity=0,weight=3,color="black",fillColor=NULL) %>% addPolygons(data=dy3, layerId=dy3@data[,"LQ"],fillOpacity=0.8,weight=0.1,color=NA,fillColor=pal(dy3@data[,"LQ"]), popup=popup_text) %>% addLegend(position="bottomleft", pal=pal,values=dy3@data[,"LQ"],opacity=0.8) 
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(d_uniq[d_uniq$anoEleicao==2014 & d_uniq$sigla_UF==input$State,], hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(255, 255, 255, 0); border-color: rgba(255, 255, 255, 0); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> ", point$NOME_URNA_CANDIDATO, "<br/>",
                    "<b> ", point$NUMERO_PARTIDO, "<br/>")))
    )
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
