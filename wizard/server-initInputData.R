
observe({

  shinyjs::hide(selector = "a[data-value=\"qcFilterTab\"]")
  shinyjs::hide(selector = "a[data-value=\"vlnplot\"]")
  shinyjs::hide(selector = "a[data-value=\"filterNormSelectTab\"]")
  shinyjs::hide(selector = "a[data-value=\"dispersionPlot\"]")
  shinyjs::hide(selector = "a[data-value=\"runPcaTab\"]")
  shinyjs::hide(selector = "a[data-value=\"vizPcaPlot\"]")
  shinyjs::hide(selector = "a[data-value=\"pcaPlot\"]")
  shinyjs::hide(selector = "a[data-value=\"heatmapPlot\"]")
  shinyjs::hide(selector = "a[data-value=\"jackStrawPlot\"]")
  shinyjs::hide(selector = "a[data-value=\"clusterCells\"]")
  shinyjs::hide(selector = "a[data-value=\"tsneTab\"]")
  shinyjs::hide(selector = "a[data-value=\"finishTab\"]")
  shinyjs::hide(selector = "a[data-value=\"findMarkersTab\"]")
  shinyjs::hide(selector = "a[data-value=\"vizMarkersTab\"]")
  #shinyjs::hide(selector = "a[data-value=\"exploreTab\"]")

  # Check if example selected, or if not then ask to upload a file.
  # shiny:: validate(
  #   need((input$data_file_type=="examplecounts")|((!is.null(input$rdatafile))|(!is.null(input$datafile))),
  #        message = "Please select a file")
  # )
  inputDataReactive()

  #inFile <- input$datafile

})


inputDataReactive <- reactive({

  print("inputting data")

  query <- parseQueryString(session$clientData$url_search)

  # Check if example selected, or if not then ask to upload a file.
  shiny:: validate(
    need( identical(input$data_file_type,"examplecounts")|(!is.null(input$datafile))|(!is.null(query[['countsdata']])),
         message = "Please select a file")
  )

  if (!is.null(query[['countsdata']]) ) {
    inFile = decryptUrlParam(query[['countsdata']])

    shinyjs::show(selector = "a[data-value=\"datainput\"]")
    shinyjs::disable("data_file_type")
    shinyjs::disable("datafile")
    #js$collapse("uploadbox")

  }
  else
  {
    inFile <- input$datafile
    # if (is.null(inFile))
    #   return(NULL)
    #
    # inFile = inFile$datapath
  }

  #inFile <- input$datafile

  if (!is.null(inFile) && !is.null(query[['countsdata']])) {
    #js$addStatusIcon("datainput","loading")
    seqdata <- read.csv(inFile, header=TRUE, sep=",", row.names = 1)
    print('uploaded seqdata')
    if(ncol(seqdata)==1) { # if file appears not to work as csv try tsv
      seqdata <- read.tsv(inFile, header=TRUE, row.names = 1)
      print('changed to tsv, uploaded seqdata')
    }
    shiny::validate(need(ncol(seqdata)>1,
                         message="File appears to be one column. Check that it is a comma or tab delimited (.csv) file."))

    return(list('data'=seqdata))
  }
  else if(!is.null(inFile) & input$data_file_type=="upload10x")
  {
    js$addStatusIcon()
    shiny:: validate(
      need(dim(inFile)[1] == 3,
           message = "need 3 files, 1 .mtx and 2 .tsv")
    )



    filesdir = dirname(inFile[1,4])

    #inFile = inFile$datapath

    file.rename(inFile$datapath[1],paste0(filesdir,'/',inFile$name[1]))
    file.rename(inFile$datapath[2],paste0(filesdir,'/',inFile$name[2]))
    file.rename(inFile$datapath[3],paste0(filesdir,'/',inFile$name[3]))

    pbmc.data <- Read10X(data.dir = filesdir)

    js$collapse("uploadbox")
    return(list('data'=pbmc.data))
  }
  else if(!is.null(inFile) & input$data_file_type=="uploadNonUmi")
  {
    js$addStatusIcon("datainput","loading")

    inFile = inFile$datapath

    seqdata <- read.csv(inFile[1], header=TRUE, sep=",", row.names = 1)
    print('uploaded seqdata')
    if(ncol(seqdata) < 2) { # if file appears not to work as csv try tsv
      seqdata <- read.csv(inFile[1], header=TRUE, sep="\t", row.names = 1)
      print('changed to tsv, uploaded seqdata')
    }

    js$addStatusIcon("datainput","done")
    js$collapse("uploadbox")
    return(list('data'=seqdata))
  }
  else{
    if(input$data_file_type=="examplecounts")
    {
      js$addStatusIcon("datainput","loading")
      pbmc.data <- Read10X(data.dir = "www/hg19/")

      js$addStatusIcon("datainput","done")
      js$collapse("uploadbox")
      return(list('data'=pbmc.data))
    }
    return(NULL)
  }
})

decryptUrlParam = function (cipher)
{
  keyHex <- readr::read_file("private.txt")

  key = hex2bin(keyHex)
  cipher = hex2bin(cipher)

  orig <- simple_decrypt(cipher, key)

  unserialize(orig)
}

output$example_counts_file <- downloadHandler(filename="examplecounts_short.csv",
                                              content=function(file){
                                                file.copy("www/examplecounts_short.csv",file)
                                              })


output$countdataDT <- renderDataTable({
  tmp <- inputDataReactive()

  if(!is.null(tmp))
  {
    if(ncol(tmp$data) > 20)
     return(as.matrix(tmp$data[,1:20]))

    return(as.matrix(tmp$data))

  }

},
options = list(scrollX = TRUE))

output$inputInfo <- renderText({

  tmp <- inputDataReactive()

  if(!is.null(tmp))
  {
    outStr = paste0(
      paste("dense size: ", object.size(x = as.matrix(x = tmp$data))),
      '\n',
      paste("sparse size: ", object.size(x = tmp$data)))

  }


})


# check if a file has been uploaded and create output variable to report this
output$fileUploaded <- reactive({

  return(!is.null(inputDataReactive()))

})
outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)


observe({
  initSeuratObjReactive()
})

initSeuratObjReactive <-
  eventReactive(input$upload_data,
                ignoreNULL = TRUE,
                {
                  withProgress(message = "Initializing Seurat Object, please wait",{


                    updateCollapse(session,id =  "input_collapse_panel", open="analysis_panel",
                                   style = list("analysis_panel" = "success",
                                                "data_panel"="primary"))

                    rawData = inputDataReactive()$data
                    js$addStatusIcon("datainput","loading")

                    pbmc <- CreateSeuratObject(raw.data = rawData, min.cells = input$mincells, min.genes = input$mingenes,
                                               project = input$projectname)


                    shiny::setProgress(value = 0.8, detail = "Done.")
                    js$addStatusIcon("datainput","done")
                    shinyjs::show(selector = "a[data-value=\"qcFilterTab\"]")
                    
                    
                    #logs$InputData <- logs$InputData + 1
                    
                    #cat(logs$InputData, file="logs\\InputData.txt", append=FALSE)

                    return(list('pbmc'=pbmc))
                  })})

