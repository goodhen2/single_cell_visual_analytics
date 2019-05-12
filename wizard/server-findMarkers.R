
observe({

  if(!is.null(tsneReactive()))
  {
    pbmc = tsneReactive()$pbmc
    updateSelectizeInput(session,'clusterNum',
                         choices=levels(pbmc@ident), selected=NULL)
    updateSelectizeInput(session,'clusterNumVS1',
                         choices=levels(pbmc@ident), selected=NULL)
    updateSelectizeInput(session,'clusterNumVS2',
                         choices=levels(pbmc@ident), selected=NULL)

    updateSelectizeInput(session,'genesToPlotVln',
                         choices=values$clusterGenes, selected=NULL)

    updateSelectizeInput(session,'genesToFeaturePlot',
                         choices=values$clusterGenes, selected=NULL)
  }

})


observe({
  findClusterMarkersReactive()
})

findClusterMarkersReactive <- eventReactive(input$findClusterMarkers, {
  withProgress(message = "Processing , please wait",{
    pbmc = tsneReactive()$pbmc
    js$addStatusIcon("findMarkersTab","loading")

    shiny::setProgress(value = 0.4, detail = "Finding cluster markers ...")

    cluster.markers <- FindMarkers(object = pbmc, ident.1 = input$clusterNum, min.pct = input$minPct, test.use = input$testuse, only.pos = input$onlypos)

    shiny::setProgress(value = 0.8, detail = "Done.")

    if(is.null(values$clusterGenes))
      values$clusterGenes = rownames(cluster.markers)
    else
      values$clusterGenes = c(values$clusterGenes, rownames(cluster.markers) )

    
    # 
    # if(is.null(values$clusterGenes))
    #   values$clusterGenes <- rownames(pbmc@raw.data)
    
    
    
    values$clusterGenes = unique(values$clusterGenes)
    # 
    # 
    # 
    # 
    updateSelectizeInput(session,'genesToPlotVln',
                         choices=pbmc@var.genes, selected=NULL)

    updateSelectizeInput(session,'genesToFeaturePlot',
                         choices=pbmc@var.genes, selected=NULL)
    
    
    
    # updateSelectizeInput(session,'genesToPlotVln',
    #                      choices=values$clusterGenes, selected=NULL)
    # 
    # updateSelectizeInput(session,'genesToFeaturePlot',
    #                      choices=values$clusterGenes, selected=NULL)

    js$addStatusIcon("findMarkersTab","done")
    
    
    # logs$FMC <- logs$FMC + 1
    # 
    # cat(logs$FMC, file="logs\\FMC.txt", append=FALSE)
    
    
    return(list("clustername" = paste0("cluster",input$clusterNum),"clustermarkers"=cluster.markers))
  })
})

output$clusterMarkers <- renderDataTable({
  tmp <- findClusterMarkersReactive()

  if(!is.null(tmp)){
    tmp$clustermarkers
  }

})

output$downloadClusterMarkersCSV <- downloadHandler(
  filename = function()  {paste0(findClusterMarkersReactive()$clustername,".csv")},
  content = function(file) {
    write.csv(findClusterMarkersReactive()$clustermarkers, file, row.names=TRUE)}
)

output$clusterMarkersAvailable <-
  reactive({
    return(!is.null(findClusterMarkersReactive()$clustername))
  })
outputOptions(output, 'clusterMarkersAvailable', suspendWhenHidden=FALSE)


observe({
  findClusterMarkersVSReactive()
})

findClusterMarkersVSReactive <- eventReactive(input$findClusterMarkersVS, {
  withProgress(message = "Processing , please wait",{
    pbmc = tsneReactive()$pbmc

    js$addStatusIcon("findMarkersTab","loading")

    shiny::setProgress(value = 0.4, detail = "Finding cluster markers ...")
    cluster.markers <- FindMarkers(object = pbmc, ident.1 = input$clusterNumVS1, ident.2 = input$clusterNumVS2, min.pctvs = input$minPct, test.use = input$testuseVS, only.pos = input$onlyposVS)

    if(is.null(values$clusterGenes))
      values$clusterGenes = rownames(cluster.markers)
    else
      values$clusterGenes = c(values$clusterGenes, rownames(cluster.markers) )
    
    
    
    # if(is.null(values$clusterGenes))
    #   values$clusterGenes <- rownames(pbmc@raw.data)
    
    values$clusterGenes = unique(values$clusterGenes)
    
    # updateSelectizeInput(session,'genesToPlotVln',
    #                      choices=values$clusterGenes, selected=NULL)
    # 
    # updateSelectizeInput(session,'genesToFeaturePlot',
    #                      choices=values$clusterGenes, selected=NULL)

    shiny::setProgress(value = 0.8, detail = "Done.")
    js$addStatusIcon("findMarkersTab","done")
    
    # 
    # logs$FMV <- logs$FMV + 1
    # 
    # cat(logs$FMV, file="logs\\FMV.txt", append=FALSE)


    return(list("clustername" = paste0("cluster",input$clusterNumVS1,"_vs_clusters_",paste(input$clusterNumVS2, collapse = "_")),"clustermarkers"=cluster.markers))

  })
})

output$clusterMarkersVS <- renderDataTable({
  tmp <- findClusterMarkersVSReactive()

  if(!is.null(tmp)){
    tmp$clustermarkers
  }

})

output$downloadClusterMarkersVSCSV <- downloadHandler(
  filename = function()  {paste0(findClusterMarkersVSReactive()$clustername,".csv")},
  content = function(file) {
    write.csv(findClusterMarkersVSReactive()$clustermarkers, file, row.names=TRUE)
    }
)

output$clusterMarkersVSAvailable <-
  reactive({
    return(!is.null(findClusterMarkersVSReactive()$clustername))
  })
outputOptions(output, 'clusterMarkersVSAvailable', suspendWhenHidden=FALSE)


# ALL MARKERS


observe({
  findClusterMarkersAllReactive()
})

findClusterMarkersAllReactive <- eventReactive(input$findClusterMarkersAll, {
  withProgress(message = "Processing , please wait",{
    pbmc = tsneReactive()$pbmc

    js$addStatusIcon("findMarkersTab","loading")

    shiny::setProgress(value = 0.4, detail = "Finding cluster markers ...")

    cluster.markers <- FindAllMarkers(object = pbmc, min.pctvs = input$minPctAll, test.use = input$testuseAll, only.pos = input$onlyposAll, logfc.threshold = input$threshAll)

    # load(file="allmarkers")
    # cluster.markers <- all.markers
    
    # if(input$numGenesPerCluster > 0)
    #   cluster.markers = cluster.markers %>% group_by(cluster) %>% top_n(input$numGenesPerCluster, avg_logFC)

    
    #save(cluster.markers, file = "cluster.markers")

    if(is.null(values$clusterGenes))
      values$clusterGenes = rownames(cluster.markers)
    else
      values$clusterGenes = c(values$clusterGenes, rownames(cluster.markers) )

    
    # if(is.null(values$clusterGenes))
    #   values$clusterGenes <- rownames(pbmc@raw.data)
    
    values$clusterGenes = unique(values$clusterGenes)
    
    
    # updateSelectizeInput(session,'genesToPlotVln',
    #                      choices=values$clusterGenes, selected=NULL)
    # 
    # updateSelectizeInput(session,'genesToFeaturePlot',
    #                      choices=values$clusterGenes, selected=NULL)

    shiny::setProgress(value = 0.8, detail = "Done.")
    js$addStatusIcon("findMarkersTab","done")
    
    
    # logs$FMA <- logs$FMA + 1
    # 
    # cat(logs$FMA, file="logs\\FMA.txt", append=FALSE)

    return(list("clustername" = paste0("allClusterMarkers"),"clustermarkers"=cluster.markers))

  })
})

output$clusterMarkersAll <- renderDataTable({
  
  req(input$numGenesPerCluster)
  
  cluster.markers <- findClusterMarkersAllReactive()$clustermarkers
  
  
  
  
  if(input$numGenesPerCluster > 0)
    cluster.markers = cluster.markers %>% group_by(cluster) %>% top_n(input$numGenesPerCluster, avg_logFC)
  
  

  if(!is.null(cluster.markers)){
    cluster.markers
  }

})

output$downloadClusterMarkersAllCSV <- downloadHandler(
  filename =  function() {paste0(findClusterMarkersAllReactive()$clustername,".csv")},
  content = function(file) {
    write.csv(findClusterMarkersAllReactive()$clustermarkers, file, row.names=TRUE)}
)

output$clusterMarkersAllAvailable <-
  reactive({
    return(!is.null(findClusterMarkersAllReactive()$clustername))
  })
outputOptions(output, 'clusterMarkersAllAvailable', suspendWhenHidden=FALSE)


output$VlnMarkersPlot = renderPlot({

  if(input$plotVlns < 1)
    return()

  isolate({
    validate(
      need(length(input$genesToPlotVln) > 0, message = "Select atleast one gene")
    )

    pbmc = tsneReactive()$pbmc
    
    
    # logs$VMV <- logs$VMV + 1
    # 
    # cat(logs$VMV, file="logs\\VMV.txt", append=FALSE)

    VlnPlot(object = pbmc, features.plot = input$genesToPlotVln, use.raw = input$useRaw, y.log = input$ylog)
  })

})

output$FeatureMarkersPlot = renderPlot({

  if(input$plotFeatureMarkers < 1)
    return()

  isolate({
    validate(
      need(length(input$genesToFeaturePlot) > 0, message = "Select atleast one gene")
    )

    pbmc = tsneReactive()$pbmc
    
    
    # logs$VMF <- logs$VMF + 1
    # 
    # cat(logs$VMF, file="logs\\VMF.txt", append=FALSE)

    FeaturePlot(object = pbmc, features.plot = input$genesToFeaturePlot, cols.use = c("gray88", "blue"),reduction.use = input$reducUseFeature, pt.size = 2)
  })

})

output$ExpressionHeatmap = renderPlot({
  
  
  
  req(input$numGenesPerCluster2)
  
  
  
  
  #cluster.markers <- findClusterMarkersAllReactive()$clustermarkers
  
  cluster.markers <- exploreReactive()$allmarkers
  
  
  
  topn = cluster.markers %>% group_by(cluster) %>% top_n(input$numGenesPerCluster2, avg_logFC)
  
  pbmc = tsneReactive()$pbmc
  
  
  
  # logs$VME <- logs$VME + 1
  # 
  # cat(logs$VME, file="logs\\VME.txt", append=FALSE)
  
  
  DoHeatmap(object = pbmc, genes.use = topn$gene, slim.col.label = TRUE, remove.key = TRUE)
  
  
})
