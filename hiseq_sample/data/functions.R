


## create heatplot for sequences
## edits distance between sequences
library(ggplot2)
library(dplyr)


# extra code
js <- c(
  "table.on('key', function(e, datatable, key, cell, originalEvent){",
  "  var targetName = originalEvent.target.localName;",
  "  if(key == 13 && targetName == 'body'){",
  "    $(cell.node()).trigger('dblclick.dt');",
  "  }",
  "});",
  "table.on('keydown', function(e){",
  "  var keys = [9,13,37,38,39,40];",
  "  if(e.target.localName == 'input' && keys.indexOf(e.keyCode) > -1){",
  "    $(e.target).trigger('blur');",
  "  }",
  "});",
  "table.on('key-focus', function(e, datatable, cell, originalEvent){",
  "  var targetName = originalEvent.target.localName;",
  "  var type = originalEvent.type;",
  "  if(type == 'keydown' && targetName == 'input'){",
  "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
  "      $(cell.node()).trigger('dblclick.dt');",
  "    }",
  "  }",
  "});"
)


# dom: t, Bfrtip
to_DT_table <- function(df, editable = FALSE, pageLength = 10, dom = "t") {
  DT::datatable(
    df,
    selection = "none",
    editable = editable,
    callback = JS(js),
    # extensions = "KeyTable",
    extensions = 'Buttons',
    options = list(
      keys = TRUE,
      pageLength = pageLength,
      scrollX = TRUE,
      dom = dom, # 'Bfrtip', # "t"
      buttons =
        list('copy',
             list(extend = 'collection',
                  buttons = c('excel', 'csv'),
                  text = 'Download'))))
}


load_index <- function(x = NULL) {
  #from hiseqr packag
  if(is.null(x)) {
    x <- system.file("data", "hiseq_index.rds", package = "hiseqr")
  }

  #read data
  l <- readRDS(x) # named sequence
  # format: truseq.TruSeq_Index1
  a <- unlist(l)
  # format: TruSeq_Index1
  b <- stringr::str_split(names(a), "\\.", n = 2, simplify = T) #
  names(a) <- b[, 2]
  a
}



#' Generate heatmap
#'
#' @param x string name of the P7 index, TruSeq_Index1
#'
get_index_mut_heatmap <- function(x, cutoff = -1) {
  if(! hiseqr::is_sheet_index_id(x)){
    warning("not valid P7 Index")
    return(NULL)
  }
  # full version of p7 index
  hiseq_index <- load_index()
  index_mut_heatmap(hiseq_index[x], cutoff = cutoff)
}


get_index_mut_table <- function(x, cutoff = -1) {
  if(! hiseqr::is_sheet_index_id(x)){
    warning("not valid P7 Index")
    return(NULL)
  }
  # full version of p7 index
  hiseq_index <- load_index()
  index_mut_table(hiseq_index[x], cutoff = cutoff)
}



index_mut_table <- function(x,  labels = NULL, cutoff = -1) {
  # calculate mutations
  if(! all(grepl("^[ACGTN]+$", x, perl = TRUE, ignore.case = TRUE))) {
    stop("input error, [ACGTN] allowed only")
  }

  # calculate
  df <- index_mutation(x, labels)

  ## subset of data.frame
  if(cutoff < 0) {
    cutoff <- 1e10
  }

  # filter by cutoff
  hits <- df %>%
    dplyr::filter(score <= cutoff)

  # subset
  df %>%
    dplyr::filter(x %in% hits$x,
                  y %in% hits$y)
}


#' index_checker
#'
#' @param x vector of index sequences,
#' @param labels vector, get from x names, the same length of x
#' @param ignore.case bool,
#'
#' @export
#'
index_mut_heatmap <- function(x, labels = NULL, cutoff = -1) {
  # # calculate mutations
  # if(! all(grepl("^[ACGTN]+$", x, perl = TRUE, ignore.case = TRUE))) {
  #   stop("input error, [ACGTN] allowed only")
  # }
  #
  # # calculate
  # df <- index_mutation(x, labels)
  #
  # ## subset of data.frame
  # if(cutoff < 0) {
  #   cutoff <- 1e10
  # }
  #
  # # filter by cutoff
  # hits <- df %>%
  #   dplyr::filter(score <= cutoff)
  #
  # # subset
  # df2 <- df %>%
  #   dplyr::filter(x %in% hits$x,
  #                 y %in% hits$y)

  df <- index_mut_table(x, labels, cutoff)
  # heatmap
  if(nrow(df) > 0) {
    p <- mut_heatmap(df)
  } else {
    p <- NULL
  }

  # output
  return(p)
}


str_distance <- function(x,
                         partial = FALSE,
                         ignore.case = FALSE,
                         fixed = TRUE) {
  # sub function
  # str_distance_pair <- function(x, y, partial = FALSE, ignore.case = FALSE, fixed = TRUE) {
  str_distance_pair <- function(x, y, ...) {
    i <- min(nchar(x), nchar(y))
    x <- stringr::str_sub(x, 1, i)
    y <- stringr::str_sub(y, 1, i)
    adist(x, y, fixed = fixed, ignore.case = ignore.case, partial = FALSE)
  }

  # all
  m <- lapply(x, function(i) {
    sapply(x, function(k) {
      str_distance_pair(i, k, partial = partial, ignore.case = igore.case, fixed = fixed)
    }, simplify = TRUE)
  })

  # merge
  ma <- dplyr::bind_rows(m) %>% as.matrix()

  if(is.null(names(x))) {
    name <- x
  } else {
    name <- names(x)
  }
  colnames(ma) <- rownames(ma) <- name
  ma
}






#' calculate the mutations between index
#'
#' @param x vector index sequences
#'
#' @export
index_mutation <- function(x, labels = NULL) {
  # chr
  if(!inherits(x, "character")) {
    stop("Expect character")
  }

  # get labels
  x <- get_labels(x, labels)

  # 1.
  # ma <- str_distance(x, partial = TRUE, ignore.case = TRUE, fixed = TRUE)

  # 2. remove lower part of matrix
  # make sure the same length
  min_len <- min(nchar(x))
  x <- sapply(x, function(i){stringr::str_sub(i, 1, min_len)}) # uniform length

  # distance
  ma <- adist(x, partial = FALSE, ignore.case = TRUE)
  # save half matrix
  # ma[upper.tri(ma, diag = TRUE)] <- NA
  ma[lower.tri(ma, diag = TRUE)] <- NA

  # create matrix/data.frame
  df <- ma %>%
    as.data.frame() %>%
    tibble::rownames_to_column("y") %>%
    tidyr::gather(key = "x", value = "score", -1) %>%
    # dplyr::mutate(score = factor(score)) %>%
    dplyr::mutate(x = factor(x, levels = rownames(ma)),
                  y = factor(y, levels = rev(colnames(ma))))

  # output
  return(df)
}



#' create heatmap plot
#'
#'
#' @param x vector index sequenes
#'
#' @export
#'
mut_heatmap <- function(df) {
  # convert to level
  df <- df %>%
    dplyr::mutate(score = factor(score))

  # chosen colors
  # pre-defined colors:
  # 0:red, 1:brown1, 2:darkorange, 3-:cyan
  mut_colors <- c("red2", "brown1", "darkorange",
                  rep("seagreen3", 98))
  names(mut_colors) <- 0:100
  mut_colors <- mut_colors[levels(df$score)]

  # number of samples
  nsamples <- length(levels(df$x))
  ptitle   <- paste0("Number of index sequences: [ ", nsamples, " ]")

  # create heatmap
  p <- df %>%
    ggplot(aes(x, y, fill = score)) +
    # geom_raster() +
    geom_tile(color = "white") +
    xlab(NULL) + ylab(NULL) +
    # ggtitle("Mutations between sequences") +
    ggtitle(ptitle) +
    geom_text(aes(label = score), color = "grey30", size = 4) +
    scale_fill_manual(values = mut_colors, na.value = "white", guide = FALSE) +
    scale_x_discrete(position = "top") +
    theme_minimal() +
    theme(
      axis.text.x.top = element_text(angle = 90, vjust = 0.5, size = 11),
      axis.ticks.x = element_blank(),
      axis.text.y.left = element_text(size = 11)
    )

  # output
  return(p)
}








#' check if the index is valid
#' 2-30 nt
#' ACGTN
#'
#' @param x vector
#'
#'
#' @return
#'
valid_index <- function(x) {
  tmp <- sapply(x, function(i){
    grepl("^[ACGTN]{2,30}$", i)
  })

  return(all(tmp))
}


#' get labels
#'
#' @param x vector character
#'
#' @export
#'
get_labels <- function(x, labels = NULL) {
  # could be vector, matrix, data.frame
  pre_labels <- c(LETTERS,
                  paste(rep(LETTERS, each = length(LETTERS)),
                        LETTERS,
                        sep = ""))

  # assign, default
  if(is.null(names(x))) {
    names(x) <- pre_labels[1:length(x)]
  }

  # check if "blank" names exists
  bn <- sum(names(x) == "")
  names(x)[names(x) == ""] <- pre_labels[1:bn]

  # check input
  if(! is.null(labels)) {
    warning("labels short than x")
    pre_labels <- c(labels, pre_labels)
    names(x) <- pre_labels[1:length(x)]
  }

  # ouptut
  return(x)
}
