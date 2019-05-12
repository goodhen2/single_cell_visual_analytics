
#max upload 300mb
options(shiny.maxRequestSize = 300*1024^2)


logs <- reactiveValues(InputData=as.numeric(0), QCFilter=as.numeric(0), VlnPlot=as.numeric(0), FilterNormalize=as.numeric(0), 
                      ScalePCA=as.numeric(0), JackStraw=as.numeric(0), ClusterCells=as.numeric(0), TSNE=as.numeric(0), 
                      Explore=as.numeric(0), Download=as.numeric(0), FMC=as.numeric(0), FMV=as.numeric(0), FMA=as.numeric(0),
                      VMV=as.numeric(0), VMF=as.numeric(0), VME=as.numeric(0))

SetIfNull <- function(x, default) {
  if(is.null(x = x)){
    return(default)
  } else {
    return(x)
  }
}

SetXAxisGG <- function(x = 16, y = "#990000", z = "bold", x2 = 12) {
  return(theme(
    axis.title.x = element_text(face = z, colour = y, size = x),
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = x2)
  ))
}

SetYAxisGG <- function(x = 16, y = "#990000", z = "bold", x2 = 12) {
  return(theme(
    axis.title.y = element_text(face = z, colour = y, size = x),
    axis.text.y = element_text(angle = 90, vjust = 0.5, size = x2)
  ))
}

SetLegendPointsGG <- function(x = 6) {
  return(guides(colour = guide_legend(override.aes = list(size = x))))
}


SetLegendTextGG <- function(x = 12, y = "bold") {
  return(theme(legend.text = element_text(size = x, face = y)))
}

no.legend.title <- theme(legend.title = element_blank())

NoGrid <- function(...) {
  no.grid <- theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    ...
  )
  return(no.grid)
}


GGpointToBase <- function(plot, do.plot = TRUE, ...) {
  plot.build <- ggplot2::ggplot_build(plot = plot)
  build.data <- plot.build$data[[1]]
  plot.data <- build.data[, c('x', 'y', 'colour', 'shape', 'size')]
  names(x = plot.data) <- c(
    plot.build$plot$labels$x,
    plot.build$plot$labels$y,
    'color',
    'pch',
    'cex'
  )
  if (do.plot) {
    PlotBuild(plot.data = plot.data, ...)
  }
  return(plot.data)
}


HoverLocator2 <- function(
  plot,
  data.plot,
  features.info = NULL,
  dark.theme = FALSE,
  point = NULL,
  circles = NULL,
  clusters = NULL,
  source = NULL,
  ...
) {
  #   Use GGpointToBase because we already have ggplot objects
  #   with colors (which are annoying in plotly)
  plot.build <- GGpointToBase(plot = plot, do.plot = FALSE)
  rownames(x = plot.build) <- rownames(data.plot)
  #   Reset the names to 'x' and 'y'
  names(x = plot.build) <- c(
    'x',
    'y',
    names(x = plot.build)[3:length(x = plot.build)]
  )
  #   Add the names we're looking for (eg. cell name, gene name)
  if (is.null(x = features.info)) {
    plot.build$feature <- rownames(x = data.plot)
  } else {
    info <- apply(
      X = features.info,
      MARGIN = 1,
      FUN = function(x, names) {
        return(paste0(names, ': ', x, collapse = '<br>'))
      },
      names = colnames(x = features.info)
    )
    
    ident <- apply(
      X = features.info,
      MARGIN = 1,
      FUN = function(x) {
        return(paste0( x, collapse = '<br>'))
      }
      
    )
    
    
    data.info <- data.frame(
      feature = paste(rownames(x = features.info), info, sep = '<br>'),
      row.names = rownames(x = features.info)
    )
    plot.build <- merge(x = plot.build, y = data.info, by = 0)
    
    #point <- data.info
  }
  #   Set up axis labels here
  #   Also, a bunch of stuff to get axis lines done properly
  xaxis <- list(
    title = names(x = data.plot)[1],
    showgrid = FALSE,
    zeroline = FALSE,
    showline = TRUE
  )
  yaxis <- list(
    title = names(x = data.plot)[2],
    showgrid = FALSE,
    zeroline = FALSE,
    showline = TRUE
  )
  #   Check for dark theme
  if (dark.theme) {
    title <- list(color = 'white')
    xaxis <- c(xaxis, color = 'white')
    yaxis <- c(yaxis, color = 'white')
    plotbg <- 'black'
  } else {
    title = list(color = 'black')
    plotbg = 'white'
  }
  #   Start plotly and pipe it into layout for axis modifications
  #   The `~' means pull from the data passed (this is why we reset the names)
  #   Use I() to get plotly to accept the colors from the data as is
  #   Set hoverinfo to 'text' to override the default hover information
  #   rather than append to it
  
  
  
  if(is.null(circles)){
    plotly::plot_ly(
      data = plot.build,
      x = ~x,
      y = ~y,
      type = 'scatter',
      mode = 'markers',
      color = ~I(color),
      hoverinfo = 'text',
      text = ~feature,
      key = ~ident,
      source = source
    )%>% plotly::layout(
      xaxis = xaxis,
      yaxis = yaxis,
      titlefont = title,
      paper_bgcolor = plotbg,
      plot_bgcolor = plotbg,
      ...
    )
  }
  
  else{
    
    
    shape_list <- list()
    
    num <- 1
    for(i in clusters){
      circle <- list(type = 'circle',
                     xref = 'x', x0 = circles[[i+1]][1], x1 = circles[[i+1]][2],
                     yref = 'y', y0 = circles[[i+1]][3], y1 = circles[[i+1]][4],
                     line = list(color = 'rgba(255, 0, 0, 1)'))
      
      shape_list <- append(shape_list, list(circle))
      #shape_list <- append(circle, shape_list)
      
      # shape_list[[num]] <- circle
      # num <- num+1
    }
    
    #hape_list <- unlist(shape_list)
    
    #shape_list <- as.list(shape_list)
    
    #return (shape_list)
    
    # for(i in clusters){
    #   circle <- list(type = 'circle',
    #                  xref = 'x', x0 = i*2, x1 = i*3,
    #                  yref = 'y', y0 = i*2, y1 = i*3,
    #                  line = list(color = 'rgba(128, 0, 0, 1)'))
    #   
    #   shape_list <- c(shape_list, circle)
    # }
    # 
    # shape_list <- list(
    #   list(type = 'circle',
    #        xref = 'x', x0 = 0, x1 = 10,
    #        yref = 'y', y0 = 0, y1 = 10,
    #        line = list(color = 'rgba(128, 0, 0, 1)')),
    #   list(type = 'circle',
    #        xref = 'x', x0 = 10, x1 = 20,
    #        yref = 'y', y0 = 10, y1 = 20,
    #        line = list(color = 'rgba(128, 0, 0, 1)'))
    # )
    # 
    # lista <- list(type = 'circle',
    #               xref = 'x', x0 = 0, x1 = 10,
    #               yref = 'y', y0 = 0, y1 = 10,
    #               line = list(color = 'rgba(128, 0, 0, 1)'))
    
    plotly::plot_ly(
      data = plot.build,
      x = ~x,
      y = ~y,
      type = 'scatter',
      mode = 'markers',
      color = ~I(color),
      hoverinfo = 'text',
      text = ~feature,
      key = ~ident,
      source = source
    )%>% plotly::layout(
      xaxis = xaxis,
      yaxis = yaxis,
      titlefont = title,
      paper_bgcolor = plotbg,
      plot_bgcolor = plotbg,
      shapes = shape_list,
      ...
    )
  }
  #return(data.info)
}




DimPlot2 <- function(
  object,
  reduction.use = "pca",
  dim.1 = 1,
  dim.2 = 2,
  cells.use = NULL,
  pt.size = 1,
  do.return = FALSE,
  do.bare = FALSE,
  cols.use = NULL,
  group.by = "ident",
  pt.shape = NULL,
  do.hover = FALSE,
  data.hover = 'ident',
  do.identify = FALSE,
  do.label = FALSE,
  label.size = 4,
  no.legend = FALSE,
  coord.fixed = FALSE,
  no.axes = FALSE,
  dark.theme = FALSE,
  plot.order = NULL,
  cells.highlight = NULL,
  cols.highlight = 'red',
  sizes.highlight = 1,
  plot.title = NULL,
  vector.friendly = FALSE,
  png.file = NULL,
  png.arguments = c(10,10, 100),
  na.value = 'grey50',
  point = NULL,
  source = NULL,
  ...
) {
  #first, consider vector friendly case
  if (vector.friendly) {
    previous_call <- blank_call <- png_call <-  match.call()
    blank_call$pt.size <- -1
    blank_call$do.return <- TRUE
    blank_call$vector.friendly <- FALSE
    png_call$no.axes <- TRUE
    png_call$no.legend <- TRUE
    png_call$do.return <- TRUE
    png_call$vector.friendly <- FALSE
    png_call$plot.title <- NULL
    blank_plot <- eval(blank_call, sys.frame(sys.parent()))
    png_plot <- eval(png_call, sys.frame(sys.parent()))
    png.file <- SetIfNull(x = png.file, default = paste0(tempfile(), ".png"))
    ggsave(
      filename = png.file,
      plot = png_plot,
      width = png.arguments[1],
      height = png.arguments[2],
      dpi = png.arguments[3]
    )
    to_return <- AugmentPlot(plot1 = blank_plot, imgFile = png.file)
    file.remove(png.file)
    if (do.return) {
      return(to_return)
    } else {
      print(to_return)
    }
  }
  embeddings.use <- GetDimReduction(
    object = object,
    reduction.type = reduction.use,
    slot = "cell.embeddings"
  )
  if (length(x = embeddings.use) == 0) {
    stop(paste(reduction.use, "has not been run for this object yet."))
  }
  cells.use <- SetIfNull(x = cells.use, default = colnames(x = object@data))
  dim.code <- GetDimReduction(
    object = object,
    reduction.type = reduction.use,
    slot = "key"
  )
  dim.codes <- paste0(dim.code, c(dim.1, dim.2))
  data.plot <- as.data.frame(x = embeddings.use)
  # data.plot <- as.data.frame(GetDimReduction(object, reduction.type = reduction.use, slot = ""))
  cells.use <- intersect(x = cells.use, y = rownames(x = data.plot))
  data.plot <- data.plot[cells.use, dim.codes]
  ident.use <- as.factor(x = object@ident[cells.use])
  if (group.by != "ident") {
    ident.use <- as.factor(x = FetchData(
      object = object,
      vars.all = group.by
    )[cells.use, 1])
  }
  data.plot$ident <- ident.use
  data.plot$x <- data.plot[, dim.codes[1]]
  data.plot$y <- data.plot[, dim.codes[2]]
  data.plot$pt.size <- pt.size
  if (!is.null(x = cells.highlight)) {
    # Ensure that cells.highlight are in our data.frame
    if (is.character(x = cells.highlight)) {
      cells.highlight <- list(cells.highlight)
    } else if (is.data.frame(x = cells.highlight) || !is.list(x = cells.highlight)) {
      cells.highlight <- as.list(x = cells.highlight)
    }
    cells.highlight <- lapply(
      X = cells.highlight,
      FUN = function(cells) {
        cells.return <- if (is.character(x = cells)) {
          cells[cells %in% rownames(x = data.plot)]
        } else {
          cells <- as.numeric(x = cells)
          cells <- cells[cells <= nrow(x = data.plot)]
          rownames(x = data.plot)[cells]
        }
        return(cells.return)
      }
    )
    # Remove groups that had no cells in our dataframe
    cells.highlight <- Filter(f = length, x = cells.highlight)
    if (length(x = cells.highlight) > 0) {
      if (!no.legend) {
        no.legend <- is.null(x = names(x = cells.highlight))
      }
      names.highlight <- if (is.null(x = names(x = cells.highlight))) {
        paste0('Group_', 1L:length(x = cells.highlight))
      } else {
        names(x = cells.highlight)
      }
      sizes.highlight <- rep_len(
        x = sizes.highlight,
        length.out = length(x = cells.highlight)
      )
      cols.highlight <- rep_len(
        x = cols.highlight,
        length.out = length(x = cells.highlight)
      )
      highlight <- rep_len(x = NA_character_, length.out = nrow(x = data.plot))
      if (is.null(x = cols.use)) {
        cols.use <- 'black'
      }
      cols.use <- c(cols.use[1], cols.highlight)
      size <- rep_len(x = pt.size, length.out = nrow(x = data.plot))
      for (i in 1:length(x = cells.highlight)) {
        cells.check <- cells.highlight[[i]]
        index.check <- match(x = cells.check, rownames(x = data.plot))
        highlight[index.check] <- names.highlight[i]
        size[index.check] <- sizes.highlight[i]
      }
      plot.order <- sort(x = unique(x = highlight), na.last = TRUE)
      plot.order[is.na(x = plot.order)] <- 'Unselected'
      highlight[is.na(x = highlight)] <- 'Unselected'
      highlight <- as.factor(x = highlight)
      data.plot$ident <- highlight
      data.plot$pt.size <- size
      if (dark.theme) {
        cols.use[1] <- 'white'
      }
    }
  }
  if (!is.null(x = plot.order)) {
    if (any(!plot.order %in% data.plot$ident)) {
      stop("invalid ident in plot.order")
    }
    plot.order <- rev(x = c(
      plot.order,
      setdiff(x = unique(x = data.plot$ident), y = plot.order)
    ))
    data.plot$ident <- factor(x = data.plot$ident, levels = plot.order)
    data.plot <- data.plot[order(data.plot$ident), ]
  }
  p <- ggplot(data = data.plot, mapping = aes(x = x, y = y)) +
    geom_point(mapping = aes(colour = factor(x = ident), size = pt.size))
  if (!is.null(x = pt.shape)) {
    shape.val <- FetchData(object = object, vars.all = pt.shape)[cells.use, 1]
    if (is.numeric(shape.val)) {
      shape.val <- cut(x = shape.val, breaks = 5)
    }
    data.plot[, "pt.shape"] <- shape.val
    p <- ggplot(data = data.plot, mapping = aes(x = x, y = y)) +
      geom_point(mapping = aes(
        colour = factor(x = ident),
        shape = factor(x = pt.shape),
        size = pt.size
      ))
  }
  if (!is.null(x = cols.use)) {
    p <- p + scale_colour_manual(values = cols.use, na.value=na.value)
  }
  if(coord.fixed){
    p <- p + coord_fixed()
  }
  p <- p + guides(size = FALSE)
  p2 <- p +
    xlab(label = dim.codes[[1]]) +
    ylab(label = dim.codes[[2]]) +
    scale_size(range = c(min(data.plot$pt.size), max(data.plot$pt.size)))
  p3 <- p2 +
    SetXAxisGG() +
    SetYAxisGG() +
    SetLegendPointsGG(x = 6) +
    SetLegendTextGG(x = 12) +
    no.legend.title +
    theme_bw() +
    NoGrid()
  if (dark.theme) {
    p <- p + DarkTheme()
    p3 <- p3 + DarkTheme()
  }
  p3 <- p3 + theme(legend.title = element_blank())
  if (!is.null(plot.title)) {
    p3 <- p3 + ggtitle(plot.title) + theme(plot.title = element_text(hjust = 0.5))
  }
  if (do.label) {
    data.plot %>%
      dplyr::group_by(ident) %>%
      summarize(x = median(x = x), y = median(x = y)) -> centers
    p3 <- p3 +
      geom_point(data = centers, mapping = aes(x = x, y = y), size = 0, alpha = 0) +
      geom_text(data = centers, mapping = aes(label = ident), size = label.size)
  }
  if (no.legend) {
    p3 <- p3 + theme(legend.position = "none")
  }
  if (no.axes) {
    p3 <- p3 + theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank()
    )
  }
  if (do.identify || do.hover) {
    if (do.bare) {
      plot.use <- p
    } else {
      plot.use <- p3
    }
    if (do.hover) {
      if (is.null(x = data.hover)) {
        features.info <- NULL
      } else {
        features.info <- FetchData(object = object, vars.all = data.hover)
      }
      return(HoverLocator2(
        plot = plot.use,
        data.plot = data.plot,
        features.info = features.info,
        dark.theme = dark.theme,
        point = point,
        source = source
      ))
    } else if (do.identify) {
      return(FeatureLocator(
        plot = plot.use,
        data.plot = data.plot,
        dark.theme = dark.theme,
        ...
      ))
    }
  }
  if (do.return) {
    if (do.bare) {
      return(p)
    } else {
      return(p3)
    }
  }
  if (do.bare) {
    print(p)
  } else {
    print(p3)
  }
}


TSNEPlot2 <- function(
  object,
  do.label = FALSE,
  pt.size=1,
  label.size=4,
  cells.use = NULL,
  colors.use = NULL,
  point = NULL,
  source = NULL,
  ...
) {
  return(DimPlot2(
    object = object,
    reduction.use = "tsne",
    cells.use = cells.use,
    pt.size = pt.size,
    do.label = do.label,
    label.size = label.size,
    cols.use = colors.use,
    point = point,
    source = source,
    ...
  ))
}



###################################################

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


SetQuantile <- function(cutoff, data) {
  if (grepl(pattern = '^q[0-9]{1,2}$', x = as.character(x = cutoff), perl = TRUE)) {
    this.quantile <- as.numeric(x = sub(
      pattern = 'q',
      replacement = '',
      x = as.character(x = cutoff)
    )) / 100
    data <- unlist(x = data)
    data <- data[data > 0]
    cutoff <- quantile(x = data, probs = this.quantile)
  }
  return(as.numeric(x = cutoff))
}

SingleFeaturePlot <- function(
  data.use,
  feature,
  data.plot,
  pt.size,
  pch.use,
  cols.use,
  dim.codes,
  min.cutoff,
  max.cutoff,
  coord.fixed,
  no.axes,
  no.title = FALSE,
  no.legend,
  dark.theme,
  vector.friendly = FALSE,
  png.file = NULL,
  png.arguments=c(10,10,100)
) {
  #first, consider vector friendly case
  if (vector.friendly) {
    previous_call <- blank_call <- png_call <- match.call()
    blank_call$pt.size <- -1
    blank_call$vector.friendly <- FALSE
    png_call$no.axes <- TRUE
    png_call$no.legend <- TRUE
    png_call$vector.friendly <- FALSE
    png_call$no.title <- TRUE
    blank_plot <- eval(blank_call, sys.frame(sys.parent()))
    png_plot <- eval(png_call, sys.frame(sys.parent()))
    png.file <- SetIfNull(x = png.file, default = paste0(tempfile(), ".png"))
    ggsave(filename = png.file, plot = png_plot,
           width = png.arguments[1],
           height = png.arguments[2],
           dpi = png.arguments[3])
    to_return <- AugmentPlot(blank_plot, png.file)
    file.remove(png.file)
    return(to_return)
  }
  data.gene <- na.omit(object = data.frame(data.use[feature, ]))
  #   Check for quantiles
  min.cutoff <- SetQuantile(cutoff = min.cutoff, data = data.gene)
  max.cutoff <- SetQuantile(cutoff = max.cutoff, data = data.gene)
  #   Mask any values below the minimum and above the maximum values
  data.gene <- sapply(
    X = data.gene,
    FUN = function(x) {
      return(ifelse(test = x < min.cutoff, yes = min.cutoff, no = x))
    }
  )
  data.gene <- sapply(
    X = data.gene,
    FUN = function(x) {
      return(ifelse(test = x > max.cutoff, yes = max.cutoff, no = x))
    }
  )
  data.plot$gene <- data.gene
  #   Stuff for break points
  if (length(x = cols.use) == 1) {
    brewer.gran <- brewer.pal.info[cols.use, ]$maxcolors
  } else {
    brewer.gran <- length(x = cols.use)
  }
  #   Cut points
  if (all(data.gene == 0)) {
    data.cut <- 0
  } else {
    data.cut <- as.numeric(x = as.factor(x = cut(
      x = as.numeric(x = data.gene),
      breaks = brewer.gran
    )))
  }
  data.plot$col <- as.factor(x = data.cut)
  #   Start plotting
  p <- ggplot(data = data.plot, mapping = aes(x = x, y = y))
  if (brewer.gran != 2) {
    if (length(x = cols.use) == 1) {
      p <- p + geom_point(
        mapping = aes(color = col),
        size = pt.size,
        shape = pch.use
      ) + scale_color_brewer(palette = cols.use)
    } else {
      p <- p + geom_point(
        mapping = aes(color = col),
        size = pt.size,
        shape = pch.use
      ) + scale_color_manual(values = cols.use)
    }
  } else {
    if (all(data.plot$gene == data.plot$gene[1])) {
      warning(paste0("All cells have the same value of ", feature, "."))
      p <- p + geom_point(color = cols.use[1], size = pt.size, shape = pch.use)
    } else {
      p <- p + geom_point(
        mapping = aes(color = gene),
        size = pt.size,
        shape = pch.use
      ) + scale_color_gradientn(
        colors = cols.use,
        guide = guide_colorbar(title = feature)
      )
    }
  }
  if (dark.theme) {
    p <- p + DarkTheme()
  }
  if (no.axes) {
    p <- p + theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
    if (!no.title) p <- p + labs(title = feature, x ="", y="")
    if (no.title) p <- p + labs(x ="", y="")
  } else {
    if (no.title) p <- p + labs(x = dim.codes[1], y = dim.codes[2])
    if (!(no.title)) p <- p + labs(title = feature, x = dim.codes[1], y = dim.codes[2])
  }
  if (no.legend) {
    p <- p + theme(legend.position = 'none')
  }
  if(coord.fixed) {
    p <- p + coord_fixed()
  }
  
  return(p)
}


FeaturePlot2 <- function(
  object,
  features.plot,
  min.cutoff = NA, 
  max.cutoff = NA,
  dim.1 = 1,
  dim.2 = 2,
  cells.use = NULL,
  pt.size = 1,
  cols.use = c("yellow", "red"),
  pch.use = 16,
  overlay = FALSE,
  do.hover = FALSE,
  data.hover = 'ident',
  do.identify = FALSE,
  reduction.use = "tsne",
  use.imputed = FALSE,
  nCol = NULL,
  no.axes = FALSE,
  no.legend = TRUE,
  coord.fixed = FALSE,
  dark.theme = FALSE,
  do.return = FALSE,
  vector.friendly=FALSE,
  png.file = NULL,
  png.arguments = c(10,10, 100),
  clusters = NULL,
  source = NULL
  
) {
  cells.use <- SetIfNull(x = cells.use, default = colnames(x = object@data))
  if (is.null(x = nCol)) {
    nCol <- 2
    if (length(x = features.plot) == 1) {
      nCol <- 1
    }
    if (length(x = features.plot) > 6) {
      nCol <- 3
    }
    if (length(x = features.plot) > 9) {
      nCol <- 4
    }
  }
  num.row <- floor(x = length(x = features.plot) / nCol - 1e-5) + 1
  if (overlay | do.hover) {
    num.row <- 1
    nCol <- 1
  }
  par(mfrow = c(num.row, nCol))
  dim.code <- GetDimReduction(
    object = object,
    reduction.type = reduction.use,
    slot = 'key'
  )
  dim.codes <- paste0(dim.code, c(dim.1, dim.2))
  data.plot <- as.data.frame(GetCellEmbeddings(
    object = object,
    reduction.type = reduction.use,
    dims.use = c(dim.1, dim.2),
    cells.use = cells.use
  ))
  x1 <- paste0(dim.code, dim.1)
  x2 <- paste0(dim.code, dim.2)
  data.plot$x <- data.plot[, x1]
  data.plot$y <- data.plot[, x2]
  data.plot$pt.size <- pt.size
  names(x = data.plot) <- c('x', 'y')
  
  #return(data.plot)
  
  
  
  data.use <- t(x = FetchData(
    object = object,
    vars.all = features.plot,
    cells.use = cells.use,
    use.imputed = use.imputed
  ))
  #   Check mins and maxes
  min.cutoff <- mapply(
    FUN = function(cutoff, feature) {
      ifelse(
        test = is.na(x = cutoff),
        yes = min(data.use[feature, ]),
        no = cutoff
      )
    },
    cutoff = min.cutoff,
    feature = features.plot
  )
  max.cutoff <- mapply(
    FUN = function(cutoff, feature) {
      ifelse(
        test = is.na(x = cutoff),
        yes = max(data.use[feature, ]),
        no = cutoff
      )
    },
    cutoff = max.cutoff,
    feature = features.plot
  )
  check_lengths = unique(x = vapply(
    X = list(features.plot, min.cutoff, max.cutoff),
    FUN = length,
    FUN.VALUE = numeric(length = 1)
  ))
  if (length(x = check_lengths) != 1) {
    stop('There must be the same number of minimum and maximum cuttoffs as there are features')
  }
  if (overlay) {
    #   Wrap as a list for MutiPlotList
    pList <- list(
      BlendPlot(
        data.use = data.use,
        features.plot = features.plot,
        data.plot = data.plot,
        pt.size = pt.size,
        pch.use = pch.use,
        cols.use = cols.use,
        dim.codes = dim.codes,
        min.cutoff = min.cutoff,
        max.cutoff = max.cutoff,
        coord.fixed = coord.fixed,
        no.axes = no.axes,
        no.legend = no.legend,
        dark.theme = dark.theme
      )
    )
  } else {
    #   Use mapply instead of lapply for multiple iterative variables.
    pList <- mapply(
      FUN = SingleFeaturePlot,
      feature = features.plot,
      min.cutoff = min.cutoff,
      max.cutoff = max.cutoff,
      coord.fixed = coord.fixed,
      MoreArgs = list( # Arguments that are not being repeated
        data.use = data.use,
        data.plot = data.plot,
        pt.size = pt.size,
        pch.use = pch.use,
        cols.use = cols.use,
        dim.codes = dim.codes,
        no.axes = no.axes,
        no.legend = no.legend,
        dark.theme = dark.theme,
        vector.friendly = vector.friendly,
        png.file = png.file,
        png.arguments = png.arguments
      ),
      SIMPLIFY = FALSE # Get list, not matrix
    )
  }
  
  if(do.hover){
    
    #return(clusters)
    
    features.info <- FetchData(object = object, vars.all = data.hover)
    
    
    if (length(clusters)!=0){
      
      
      data.plot$ident <- features.info$ident
      
      #return(data.plot)
      
      circles <- list()
      
      
      for(i in clusters){
        
        
        
        
        cluster <- subset(data.plot, ident==i)
        
        
        #return(colnames(cluster))
        
        
        
        
        # x_min <- min(cluster[, 1])
        # x_max <- max(cluster[, 1])
        # 
        # y_min <- min(cluster[, 2])
        # y_max <- max(cluster[, 2])
        
        
        out_x <- remove_outliers(cluster[, 1])
        out_y <- remove_outliers(cluster[, 2])
        
        #return (out_x)
        
        #out_x <- out_x %>% filter(!is.na())
        
        x_min <- min(out_x, na.rm=TRUE)
        x_max <- max(out_x, na.rm=TRUE)
        
        y_min <- min(out_y, na.rm=TRUE)
        y_max <- max(out_y, na.rm=TRUE)
        
        circles[[i+1]] <- c(x_min, x_max, y_min, y_max)
        
        
      }
      
      #return(circles)
      
      
      
    }
    
    else{
      
      clusters <- NULL
      circles <- NULL
      
    }
    
    return(HoverLocator2(
      plot = pList[[1]],
      data.plot = data.plot,
      features.info = features.info,
      dark.theme = dark.theme,
      title = features.plot,
      circles = circles,
      clusters = clusters,
      source = source
      
    ))
  }
  
  if (do.hover) {
    if (length(x = pList) != 1) {
      stop("'do.hover' only works on a single feature or an overlayed FeaturePlot")
    }
    if (is.null(x = data.hover)) {
      features.info <- NULL
    } else {
      features.info <- FetchData(object = object, vars.all = data.hover)
    }
    
    # data.plot$ident <- feature.info$ident
    # 
    # return(data.plot)
    
    
    #   Use pList[[1]] to properly extract the ggplot out of the plot list
    return(HoverLocator(
      plot = pList[[1]],
      data.plot = data.plot,
      features.info = features.info,
      dark.theme = dark.theme,
      title = features.plot
    ))
    # invisible(readline(prompt = 'Press <Enter> to continue\n'))
  } else if (do.identify) {
    if (length(x = pList) != 1) {
      stop("'do.identify' only works on a single feature or an overlayed FeaturePlot")
    }
    #   Use pList[[1]] to properly extract the ggplot out of the plot list
    return(FeatureLocator(
      plot = pList[[1]],
      data.plot = data.plot,
      dark.theme = dark.theme
    ))
  } else {
    print(x = cowplot::plot_grid(plotlist = pList, ncol = nCol))
  }
  ResetPar()
  if (do.return){
    return(pList)
  }
}


server <- function(input, output, session) {

  source("server-initInputData.R",local = TRUE)

  source("server-qcfilter.R",local = TRUE)

  source("server-vln.R",local = TRUE)

  source("server-normSelect.R",local = TRUE)

  source("server-dispersion.R",local = TRUE)

  source("server-runPca.R",local = TRUE)

  source("server-pcaPlots.R",local = TRUE)

  source("server-jackStraw.R",local = TRUE)

  source("server-clusterCells.R",local = TRUE)

  source("server-tsne.R",local = TRUE)

  source("server-download.R",local = TRUE)

  source("server-findMarkers.R",local = TRUE)
  
  source("server-explore.R",local = TRUE)

}
