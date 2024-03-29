
output$filteredGenesFound <- renderUI({
  myValues$exprOut

})

myValues <- reactiveValues()

observe({
  if(input$addExpr > 0){

    validate(
      need( isolate(!(input$regexFilter %in% myValues$exprList)) ,
            message = "Regex already exists"),
      need( isolate(!(input$regexFilterLabel %in% names(myValues$exprList))) ,
            message = "Regex already exists")
    )

    test = isolate( c(input$regexFilter) )
    test = isolate( setNames(test,input$regexFilterLabel) )
    isolate({
      myValues$exprList <- c(myValues$exprList, test)
      updateSelectizeInput(session,'filterExpressions',
                           choices=myValues$exprList, selected=myValues$exprList)
    })


    updateTextInput(session, "regexFilter", value = "")
    updateTextInput(session, "regexFilterLabel", value = "")

    myValues$exprOut = ""

  }
})

observeEvent(input$filterExpressions,ignoreNULL=FALSE, {

  if (length(input$filterExpressions) < length(myValues$exprList))
  {
    myValues$exprList = myValues$exprList[ myValues$exprList %in% input$filterExpressions]
    updateSelectizeInput(session,'filterExpressions',
                         choices=myValues$exprList, selected=myValues$exprList)
  }

})


observe({

  pbmc = initSeuratObjReactive()$pbmc

  updateSelectizeInput(session,'filterSpecGenes',
                       choices=rownames(x = pbmc@data), server = TRUE)
})

observe({
  if(input$testExpr > 0 & isolate(input$regexFilter != "")){

    pbmc = initSeuratObjReactive()$pbmc

    test = grep(pattern = isolate(input$regexFilter), x = rownames(x = pbmc@data), value = TRUE)


    genes = unlist(lapply(test, function(x){ paste("<div class='col-sm-3'>", x, "</div>")}))

    genes = c("<h4>num of genes: ",length(genes),"</h4>",genes)
    myValues$exprOut <- HTML(genes)

  }
})


observe({
  analyzeDataReactive()
})

analyzeDataReactive <-
  eventReactive(input$submit_data,
                ignoreNULL = FALSE, {
                  withProgress(message = "Analyzing Single Cell data, please wait",{
                    print("analysisCountDataReactive")

                    pbmc <- initSeuratObjReactive()$pbmc

                    js$addStatusIcon("qcFilterTab","loading")

                    shiny::setProgress(value = 0.3, detail = " Applying Filters ...")

                    #######

                    if(length(myValues$exprList) > 0)
                    {


                      for (i in 1:length(myValues$exprList)) {

                        exprPattern = myValues$exprList[i]
                        exprName = names(myValues$exprList[i])

                        pattern.genes <- grep(pattern = exprPattern, x = rownames(x = pbmc@data), value = TRUE)
                        percent.pattern <- Matrix::colSums(pbmc@raw.data[pattern.genes, ])/Matrix::colSums(pbmc@raw.data)

                        pbmc <- AddMetaData(object = pbmc, metadata = percent.pattern, col.name = paste("percent.",exprName, sep = ""))

                      }
                    }

                    if(length(input$filterSpecGenes) > 0)
                    {

                      percent.pattern <- Matrix::colSums(pbmc@raw.data[input$filterSpecGenes, ])/Matrix::colSums(pbmc@raw.data)

                      pbmc <- AddMetaData(object = pbmc, metadata = percent.pattern, col.name = paste0("percent.",input$customGenesLabel))

                    }

                    if(length(input$filterPasteGenes) > 0)
                    {

                      percent.pattern <- Matrix::colSums(pbmc@raw.data[input$filterPasteGenes, ])/Matrix::colSums(pbmc@raw.data)

                      pbmc <- AddMetaData(object = pbmc, metadata = percent.pattern, col.name = paste0("percent.",input$pasteGenesLabel))

                    }

                    #######

                    shiny::setProgress(value = 0.9, detail = "Done.")

                    shinyjs::show(selector = "a[data-value=\"vlnplot\"]")

                    ###update subsetNames
                    subsetNames = c("nGene")

                    if(length(myValues$exprList) > 0)
                      subsetNames = c(paste("percent.",names(myValues$exprList), sep = ""), subsetNames)
                    if(length(input$filterSpecGenes) > 0)
                      subsetNames = c(paste0("percent.",input$customGenesLabel), subsetNames)
                    if(length(input$filterPasteGenes) > 0)
                      subsetNames = c(paste0("percent.",input$pasteGenesLabel), subsetNames)

                    updateSelectizeInput(session,'subsetNames',
                                         choices=subsetNames, selected=subsetNames)
                    #####


                    js$addStatusIcon("qcFilterTab","done")

                    js$addStatusIcon("vlnplot","next")
                    
                    
                    #logs$QCFilter <- logs$QCFilter + 1
                    
                    #cat(logs$QCFilter, file="logs\\QCFilter.txt", append=FALSE)

                    return(list('pbmc'=pbmc))
                  })
                  }
  )


observe({

  filterTextReactive()
})

filterTextReactive <- eventReactive(input$addFilterPaste, {

  validate(
    need(input$listPasteGenes != "", message = "list of genes empty"),
    need(input$pasteGenesLabel != "", message = "genes label empty")
  )

  pbmc <- initSeuratObjReactive()$pbmc

  genes = unlist(strsplit(input$listPasteGenes,input$delimeter))

  genes = gsub("^\\s+|\\s+$", "", genes)
  genes = gsub("\\n+$|^\\n+", "", genes)
  genes = gsub("^\"|\"$", "", genes)
  genesNotFound = genes[!(genes %in% rownames(x = pbmc@data))]

  if(length(genesNotFound) == 0){
    updateSelectizeInput(session,'filterPasteGenes',
                         choices=genes, selected=genes)

    updateTextInput(session, "listPasteGenes", value = "")
  }

  return(list('genes' =genes,'notfound'=genesNotFound))
})



output$value <- renderText(
  {

    if(length(filterTextReactive()$notfound) > 0)
      paste(filterTextReactive()$notfound, collapse='\n' )
  }
)

output$genesNotFound <-
  reactive({
    return(length(filterTextReactive()$notfound) > 0)
  }
)
outputOptions(output, 'genesNotFound', suspendWhenHidden=FALSE)
