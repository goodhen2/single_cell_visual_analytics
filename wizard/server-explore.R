
v <- reactiveValues()


observe({
  exploreReactive()
})

exploreReactive <-
  eventReactive(input$runExplore, {
    withProgress(message = "Processing , please wait",{
      print("Running Explore")
      
      js$addStatusIcon("exploreTab","loading")
      
      
      
      load("pbmc")
      #load("markers")
      #pbmc <- tsneReactive()$pbmc
      
      
      
      shiny::setProgress(value = 0.4, detail = "Running Explore ...")
      
      
      
      
      #markers <- FindAllMarkers(object = pbmc, logfc.threshold = -Inf, return.thresh = Inf)
      
      
      cluster_cells <- list()
      
      cells <- names(pbmc@ident)
      
      n_clusters <- nlevels(pbmc@ident)
      
      for (i in 1:n_clusters){
        
        
        cluster_cells[[i]] <- cells[pbmc@ident==(i-1)]
      }
      
      
      cluster_matrices <- list()
      
      for (i in 1:n_clusters){
        
        
        cluster_matrices[[i]] <- pbmc@raw.data[, cluster_cells[[i]]]
      }
      
      
      n_genes <- length(rownames(cluster_matrices[[1]]))
      
      ratio <- matrix(NA, nrow=n_genes, ncol=n_clusters)
      
      
      for (i in 1:n_clusters){
        
        counts <- Matrix::rowSums(!is.na(cluster_matrices[[i]]) & cluster_matrices[[i]]!=0)
        cluster_length <- length(cluster_cells[[i]])
        
        for (j in 1:n_genes){
          
          ratio[j,i] <- counts[j] / cluster_length
          
        }
      }
      
      rownames(ratio) <- rownames(cluster_matrices[[1]])
      
      
      v$genenames <- rownames(cluster_matrices[[1]])
      
      
      
       # all.markers <- FindAllMarkers(object = pbmc, only.pos = TRUE, min.pct = 0.25,
       #                                thresh.use = 0.25)
      # 
      # save(all.markers, file = "allmarkers")
      
      load(file = "allmarkers")
      
      
      ####################################################################################
      
      cell_markers <- read.csv("cell_markers.csv")
      
      
      
      dfs <- list()
      
      for(i in 1:n_clusters){
        genelist <- ratio[, as.numeric(i)]

        
        genes <- names(genelist)
        
        Gene <- c()
        ratio2 <- c()
        
        
        nrow_df <- length(genes)
        
        
        for (j in 1:nrow_df){
          Gene[j] <- toString(genes[j])
          ratio2[j] <- as.numeric(genelist[j])
        }
        

      
        #a <- 1:length(Gene)  
        df <- data.frame(Gene, ratio2)
        #df <- df[, 1, drop=F]
        colnames(df) <- c("Gene", "Ratio")
        
        
        #nrow_df <- nrow(df)
        
        
        ##########################################################################
        
        
        cluster_markers <- subset(all.markers, cluster==(i-1))
        
        logFC <- c()
        
        for (j in 1:nrow_df){
   
          
          logFC[j] <- cluster_markers$avg_logFC[match(df[j, 1], cluster_markers$gene)]
          
        
        }

        df$avg_logFC <- logFC


        
        #################################################################
        
        df$CellType <- rep(NA, times=nrow_df)
        df$Tissue <- rep(NA, times=nrow_df)
        df$Cancer <- rep(NA, times=nrow_df)
        
        for(j in 1:nrow_df){
          gene <- df[j, 1]
          
          if(gene %in% cell_markers$geneSymbol){
            
            index <- match(gene, cell_markers$geneSymbol)
            
            df$CellType[j] <- toString(cell_markers$cellName[index])
            df$Tissue[j] <- toString(cell_markers$tissueType[index])
            df$Cancer[j] <- toString(cell_markers$cellType[index])
          }
        }
        
        
        #######################################################################
        
        # df2 <- df[c(order(-df$avg_logFC)),]
        # 
        # rownames(df2) <- 1:nrow_df
        
        df2 <- df[c(order(-df$avg_logFC)),]
        
        dfs[[i]] <- df2
      }
      
      
      
      
      
      shiny::setProgress(value = 0.9, detail = "Done.")
      
    
      
      
      js$addStatusIcon("exploreTab","done")
      
      
      #logs$Explore <- logs$Explore + 1
      
      #cat(logs$Explore, file="logs\\Explore.txt", append=FALSE)
  
      
      return(list('pbmc'=pbmc, 'dfs'=dfs, 'allmarkers'=all.markers))
    })}
  )




output$exploreAvailable <- reactive({
  return(!is.null(exploreReactive()$pbmc))
})
outputOptions(output, 'exploreAvailable', suspendWhenHidden=FALSE)


output$plotly <- renderPlotly({
  
  pbmc <- exploreReactive()$pbmc
  
  
  
  TSNEPlot2(object = pbmc, do.hover=TRUE, do.identify=FALSE, point=NULL, do.label=TRUE, source="plotly")
})




output$genelist <- DT::renderDataTable({
  
  # markers <- exploreReactive()$markers
  # 
  # s <- event_data("plotly_click")
  # 
  # req(s)
  # 
  # genelist <- subset(markers, cluster==s$key & pct.1>=input$threshold)
  # 
  # genelist <- subset(genelist, select=gene)
  # 
  # 
  # 
  # rownames(genelist) <- c(1:nrow(genelist))
  # 
  # v$genelist <- genelist
  # 
  # 
  # v$genelist
  
  
  # ratio <- exploreReactive()$ratio
  # 
  # s <- event_data("plotly_click")
  # 
  # req(s)
  # 
  # 
  # genelist <- ratio[, (as.numeric(s$key)+1)]
  # 
  # genelist <- genelist[genelist>=input$threshold]
  # 
  # 
  # genes <- names(genelist)
  # 
  # Gene <- c()
  # 
  # for (i in 1:length(genes)){
  #   Gene[i] <- toString(genes[i])
  # }
  # 
  # #Gene <- c(unlist(Gene))
  # a <- 1:length(Gene)  
  # df <- data.frame(Gene, a)
  # df <- df[, 1, drop=F]
  # names(df)[1] <- "Genes"
  
  
  
  dfs <- exploreReactive()$dfs
  
  s <- event_data("plotly_click", source="plotly")
  
  req(s)
  

  
  df <- subset(dfs[[(as.numeric(s$key)+1)]], Ratio>=input$threshold)
  
  
  
  rownames(df) <- 1:nrow(df)
  
  
  v$df <- df
  
  
 v$df
  
  #df %>% datatable() %>% formatRound(columns=c('Ratio', 'avg_logFC'), digits=3)
  
  ###########################################################
  
  # all.markers <- exploreReactive()$all.markers
  # 
  # 
  # all.markers <- subset(all.markers, cluster==s$key)
  # 
  # logFC <- c()
  # 
  # for (i in 1:nrow(df)){
  #   
  #   # FC <- match(df[i, 1], all.markers$gene)
  #   # 
  #   # if (is.na(FC)){
  #   #   logFC[i] <- 0
  #   # }
  #   # 
  #   # else{
  #   # 
  #   #   logFC[i] <- all.markers$avg_logFC[FC]
  #   # }
  #   
  #   
  #   logFC[i] <- all.markers$avg_logFC[match(df[i, 1], all.markers$gene)]
  #   
  #   #logFC[i] <- match(toString(df[i, 1]), all.markers$gene)
  #   
  #   #logFC[i] <- toString(df[i, 1])
  # }
  # 
  # 
  # #df$avg_logFC <- c(unlist(logFC))
  # df$avg_logFC <- logFC
  # 
  # 
  # 
  # # df
  # 
  # # all.markers$avg_logFC <- all.markers$avg_logFC %>%
  # #   mutate_if(., is.numeric, as.numeric)
  # 
  # # df %>% as.data.table() %>% dplyr::arrange(obs_id, user_id, scroll_id, timestamp)
  # 
  # #df2 <- df %>% as.data.table() %>% dplyr::arrange(desc(avg_logFC))
  # 
  # #df2 <- dplyr::arrange(df, desc(avg_logFC))
  # 
  # 
  # 
  # 
  # 
  # df2 <- df[c(order(-df$avg_logFC)),]
  # 
  # rownames(df2) <- 1:nrow(df2)
  # 
  # df2
  # 
  # 
  # 
  # v$df <- df2
 
  
  
}, selection=list(mode="single", target="cell") )


output$test <- renderPrint({
  ratio <- exploreReactive()$ratio
  
  s <- event_data("plotly_click")
  
  req(s)
  
  
  genelist <- ratio[, (as.numeric(s$key)+1)]
  
  genelist <- genelist[genelist>=input$threshold]
  
  
  genes <- names(genelist)
  
  Gene <- list()
  
  for (i in 1:length(genes)){
    Gene[i] <- toString(genes[i])
  }
  
  Gene <- c(unlist(Gene))
  a <- 1:length(Gene)  
  df <- data.frame(Gene, a)
  df <- df[, 1, drop=F]
  names(df)[1] <- "Genes"
  
  
  ##########################
  
  all.markers <- exploreReactive()$all.markers

  
  all.markers <- subset(all.markers, cluster==s$key)

  logFC <- list()

  for (i in 1:nrow(df)){

    # FC <- match(df[i, 1], all.markers$gene)
    # 
    # if (is.na(FC)){
    #   logFC[i] <- 0
    # }
    # 
    # else{
    # 
    #   logFC[i] <- all.markers$avg_logFC[FC]
    # }


    logFC[i] <- all.markers$avg_logFC[match(df[i, 1], all.markers$gene)]
    
    #logFC[i] <- match(toString(df[i, 1]), all.markers$gene)
    
   #logFC[i] <- toString(df[i, 1])
  }

  
  df$avg_logFC <- c(unlist(logFC))
  

  
   # df
  
  # all.markers$avg_logFC <- all.markers$avg_logFC %>%
  #   mutate_if(., is.numeric, as.numeric)
  
  # df %>% as.data.table() %>% dplyr::arrange(obs_id, user_id, scroll_id, timestamp)

  #df2 <- df %>% as.data.table() %>% dplyr::arrange(desc(avg_logFC))
  
  #df2 <- dplyr::arrange(df, desc(avg_logFC))

  
  
  
  
  df2 <- df[c(order(-df$avg_logFC)),]

  rownames(df2) <- 1:nrow(df2)
  
  df2
  
  #df$avg_logFC
  
  #c(order(list(df$avg_logFC)))
})


output$circles <- renderPlotly({
  #output$circles <- renderPrint({  
  
  req(input$genelist_cells_selected)
  
  pbmc <- exploreReactive()$pbmc
  
  selected_gene <- toString(v$df$Gene[c(input$genelist_cells_selected)[1]])
  
  h0 <- list()
  h1 <- list()
  
  for (i in 1:nlevels(pbmc@ident)){
    h0[[i]] <- list()
  }
  
  for (i in 0:(nlevels(pbmc@ident)-1)){
    again = FALSE
    
    
    for (j in 0:(nlevels(pbmc@ident)-1)){
      
      if (i==j){
        next
      }
      
      markers <- FindMarkers(pbmc, i, ident.2 = j, genes.use = selected_gene, logfc.threshold = -Inf, return.thresh = Inf, min.pct = -Inf, min.cells.gene = -Inf, min.cells.group = -Inf, print.bar=FALSE)
      
 
      if(markers[1,2] >= input$threshold2){
        
        h0[[i+1]] <- c(h0[[i+1]], j)
   
        if(again==FALSE){
          h1 <- c(h1, i)
          again <- TRUE
        }
        
      }
    }

  }
  
  for (i in h1){
    for (j in 1:length(h0[[i+1]])){
      
      h1[h1==as.numeric(h0[[i+1]][j])] <- NULL
      
    }
  }
 
  
  FeaturePlot2(object = pbmc, features.plot = selected_gene, cols.use = c("grey", "blue"),
               reduction.use = "tsne", do.hover=TRUE, clusters=h1, no.legend=FALSE, source=NULL)
})



output$bar_chart <- renderPlotly({

  
  req(v$df)
  
  df <- v$df
  
  cell_table <- table(df$CellType)
  
  #cell_table <- sort(cell_table)
  
   bar_x <- as.vector(names(cell_table))
   bar_y <- as.vector(as.numeric(cell_table))
   
   
   data <- data.frame(bar_x, bar_y, stringsAsFactors = FALSE)
   data$bar_x <- factor(data$bar_x, levels = unique(data$bar_x)[order(data$bar_y, decreasing = FALSE)])
  
  # table_length <- length(cell_table)
  # 
  # bar_x <- names(cell_table)
  # 
  # bar_y <- c()
  # 
  # for(i in 1:length(bar_x)){
  #   
  #   
  #   y <- y_table[names(y_table)==bar_x[i]]
  # }
  
   ax <- list(
     title = ""

    
   )

   
   p <- plot_ly(
     data,
     x = ~bar_x,
     y = ~bar_y,
     name = "CellType",
     type = "bar",
     source = "bar_chart"
   ) %>%
     layout(xaxis = ax, yaxis = ax, margin = list(b = 200))
   

  # p <- plot_ly(
  #   x = bar_x,
  #   y = bar_y,
  #   name = "cellName",
  #   type = "bar",
  #   source = "bar_chart"
  # )  
  
  p
  
})



output$genelist2 <- renderPrint({

  s <- event_data("plotly_click", source="bar_chart")
  
  req(s)
  
  cell <- s[1, 3]
  
  df <- v$df
  
  df2 <- subset(df, CellType==cell)
  
  as.vector(df2$Gene)
  
  
  
  
  
})
