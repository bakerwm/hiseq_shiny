
# parse data from summary

library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(hiseqr)


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


# load data
load_data <- function(x) {
  readxl::read_xlsx(x, sheet = 1)
}


fix_lib_user_db <- function(df, db = "data/db.lib_user_fixed.rds") {
  # # fix lib_user
  # df <- load_data("summary.xlsx")

  # fix lib_user
  df1 <- df %>%
    dplyr::select(lib_number, lib_user) %>%
    dplyr::mutate(lib_user = gsub("\\d+$", "", lib_user, perl = TRUE)) %>%
    unique() %>%
    dplyr::group_by(lib_user) %>%
    dplyr::mutate(m = stringr::str_pad(row_number(), 2, pad = 0)) %>%
    tidyr::unite(lib_user_fix, c(lib_user, "m"), sep = "") %>%
    dplyr::mutate(user_name  = gsub("\\d+", "", lib_user_fix),
                  user_count = gsub("^[A-Za-z]+", "", lib_user_fix))

  # save to file
  saveRDS(df1, db)

  # return
  df1
}


fix_lib_user <- function(lib = "YY01", user = "JG", db = "data/db.lib_user_fixed.rds") {
  df <- readRDS(db) # db
  sapply(seq_len(length(lib)), function(i) {
    j = lib[i]
    k = user[i]
    # load
    df %>%
      dplyr::filter(lib_number == j & user_name == k) %>%
      dplyr::pull(lib_user_fix)
  }, simplify = TRUE)
}


fix_summary_lib_user <- function(x,
                                 db_summary = "data/db.summary.lib_user_fixed.rds",
                                 db_user = "data/db.lib_user_fixed.rds") {
  # fix lib_user
  df <- load_data(x) %>%
    dplyr::mutate(tmp_user = gsub("\\d+", "", lib_user)) %>%
    dplyr::mutate(lib_user_fix = fix_lib_user(lib_number, tmp_user, db_user))

  # write
  saveRDS(df, db_summary)

  db_summary # output
}


## summary: Lane and Date
table_lib_number_date <- function(df) {
  # readRDS("db.summary.lib_user_fixed.rds") %>%
  df %>%
    dplyr::select(lib_number, lib_user_fix, date) %>%
    unique() %>%
    dplyr::group_by(lib_number, date) %>%
    dplyr::summarise(list = paste(sort(lib_user_fix), collapse = ",")) %>%
    dplyr::mutate(date_got_data = date) %>%
    dplyr::rename(date_sent_sample = date) %>%
    dplyr::select(lib_number, list, date_sent_sample, date_got_data)
}


## summary: Lane and Date
table_lib_user_date <- function(df) {
  # readRDS("db.summary.lib_user_fixed.rds") %>%
  df %>%
    dplyr::select(lib_user_fix, date) %>%
    unique() %>%
    dplyr::mutate(date_got_data = date) %>%
    dplyr::rename(date_sent_sample = date)
}


## summary: lib_user, list
table_lib_user <- function(df) {
  # readRDS("db.summary.lib_user_fixed.rds") %>%
  df %>%
    dplyr::select(lib_number, tmp_user, lib_user_fix) %>%
    unique() %>%
    dplyr::mutate(lib_user = paste0(lib_number, ":", lib_user_fix)) %>%
    dplyr::group_by(tmp_user) %>%
    dplyr::summarise(.groups = "drop",
                     count = n(),
                     list = paste(lib_user, collapse = ",")) %>%
    dplyr::rename(user = tmp_user)
}


## summary: for lib_number
plot_timeline_lib_number <- function(df, mode = "x") {
  # input data
  # df <- readRDS("db.summary.lib_user_fixed.rds") %>%
  df1 <- df %>%
    dplyr::select(lib_number, date) %>%
    dplyr::mutate(year = format(date, format = "%Y")) %>%
    unique()

  # labels
  df2 <- df1 %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(m = n()) %>%
    dplyr::mutate(date = paste0(year, "-07-01"),
                  label = paste0(year, "\n(", m, ")")) %>%
    dplyr::mutate(date = as.POSIXct(date, tz = "UTC", format = "%Y-%m-%d"))

  year_lines <- as.POSIXct(c("2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01"), tz = "UTC")

  ## for x-align
  if(mode == "x") {
    # plot
    df1 %>%
      ggplot(aes(date, 0, label = lib_number)) +
      geom_line() +
      geom_point(size = 2, color = "red") +
      geom_vline(xintercept = year_lines, color = "blue", size = .5, linetype = 2) +
      geom_text(data = df2, aes(date, 1.5, label = label)) +
      ylim(-1.5, 1.5) +
      theme_light() +
      theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank()
      ) +
      #facet_wrap(year ~ ., ncol = 2, scales = "free_x") +
      geom_text_repel(
        nudge_y      = rep(c(seq(.1, 1, .2), -seq(.1, 1, .2)), times = 50, length.out = nrow(df1)),
        fontface     = "bold",
        segment.size = 0.2)

  } else if(mode == "y") {
    # plot
    df %>%
      ggplot(aes(0, date, label = lib_number)) +
      geom_line() +
      geom_point(size = 2, color = "red") +
      geom_hline(yintercept = year_lines, color = "blue", size = .5, linetype = 2) +
      geom_text(data = df2, aes(-1.5, date, label = label)) +
      xlim(-1.5, 1.5) +
      theme_light() +
      theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank()
      ) +
      #facet_wrap(year ~ ., ncol = 2, scales = "free_x") +
      geom_text_repel(
        nudge_x      = rep(c(seq(.1, 1, .2), -seq(.1, 1, .2)), times = 50, length.out = nrow(df)),
        fontface     = "bold",
        segment.size = 0.2)
  } else {
    warning("unknown mode=, x, y expected")
  }
}


## summary: for lib_user
plot_timeline_lib_user <- function(df, mode = "x") {
  # input data
  # df <- readRDS("db.summary.lib_user_fixed.rds") %>%
  df <- df %>%
    dplyr::select(lib_user_fix, date) %>%
    dplyr::mutate(year = format(date, format = "%Y")) %>%
    unique()

  # labels
  df2 <- df %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(m = n()) %>%
    dplyr::mutate(date = paste0(year, "-07-01"),
                  label = paste0(year, "\n(", m, ")")) %>%
    dplyr::mutate(date = as.POSIXct(date, tz = "UTC", format = "%Y-%m-%d"))

  year_lines <- as.POSIXct(c("2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01"), tz = "UTC")

  ## for x-align
  if(mode == "x") {
    # plot
    df %>%
      ggplot(aes(date, 0, label = lib_user_fix)) +
      geom_line() +
      geom_point(size = 2, color = "red") +
      geom_vline(xintercept = year_lines, color = "blue", size = .5, linetype = 2) +
      geom_text(data = df2, aes(date, 1.5, label = label)) +
      ylim(-1.5, 1.5) +
      theme_nothing() +
      theme(
        panel.grid = element_blank()
      ) +
      #facet_wrap(year ~ ., ncol = 2, scales = "free_x") +
      geom_text_repel(
        nudge_y      = rep(c(seq(.1, 1, .2), -seq(.1, 1, .2)), times = 50, length.out = nrow(df)),
        fontface     = "bold",
        segment.size = 0.2)

  } else if(mode == "y") {
    # plot
    df %>%
      ggplot(aes(0, date, label = lib_user_fix)) +
      geom_line() +
      geom_point(size = 2, color = "red") +
      geom_hline(yintercept = year_lines, color = "blue", size = .5, linetype = 2) +
      geom_text(data = df2, aes(-1.5, date, label = label)) +
      xlim(-1.5, 1.5) +
      theme_nothing() +
      theme(
        panel.grid = element_blank()
      ) +
      #facet_wrap(year ~ ., ncol = 2, scales = "free_x") +
      geom_text_repel(
        nudge_x      = rep(c(seq(.1, 1, .2), -seq(.1, 1, .2)), times = 50, length.out = nrow(df)),
        fontface     = "bold",
        segment.size = 0.2)
  } else {
    warning("unknown mode=, x, y expected")
  }
}


## plot: user/seq_type
plot_user_seq_type <- function(df) {
  # df = readRDS("db.summary.lib_user_fixed.rds") %>%
  # dplyr::select(tmp_user, lib_type) %>%
  df %>%
    dplyr::group_by(tmp_user, lib_type) %>%
    dplyr::summarise(count = n()) %>%
    ggplot(aes(count, lib_type, label = count, fill = lib_type)) +
    geom_col() +
    xlim(0, 250) +
    geom_text(hjust = -.2) +
    facet_wrap(tmp_user ~ ., ncol = 4, scales = "free_y") +
    theme_bw() +
    theme(
      panel.grid  = element_blank(),
      axis.text.y = element_text(color = "grey10"),
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
}


## plot: lib_type / user
plot_seq_type_user <- function(df) {
  # df = readRDS("db.summary.lib_user_fixed.rds") %>%
  # dplyr::select(tmp_user, lib_type) %>%
  df %>%
    dplyr::group_by(tmp_user, lib_type) %>%
    dplyr::summarise(count = n()) %>%
    ggplot(aes(count, tmp_user, label = count, fill = lib_type)) +
    geom_col() +
    xlim(0, 250) +
    geom_text(hjust = -.2) +
    facet_wrap(lib_type ~ ., ncol = 4, scales = "free_y") +
    theme_bw() +
    theme(
      panel.grid  = element_blank(),
      axis.text.y = element_text(color = "grey10"),
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
}


# heatmap
plot_user_vs_lib_type <- function(df) {
  # df <- readRDS("db.summary.lib_user_fixed.rds")
  df2 <- df %>%
    dplyr::group_by(tmp_user, lib_type) %>%
    dplyr::summarise(count = n())


  p1 <- ggplot(df2, aes(lib_type, tmp_user, fill = count)) +
    geom_tile(color = "white") +
    geom_text(aes(label = count), color = "white", size = 4) +
    xlab(NULL) + ylab(NULL) +
    scale_fill_gradient(low = "blue3", high = "red3") +
    # scale_x_discrete(position = "top") +
    # scale_y_discrete(position = "right") +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
      legend.position = "none"
    )

  # User
  p2 <- df2 %>%
    dplyr::group_by(tmp_user) %>%
    dplyr::summarise(x = sum(count)) %>%
    ggplot(aes(x, tmp_user)) +
    geom_col() +
    geom_text(aes(label = x), hjust = -.2) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 400)) +
    ylab(NULL) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text.y = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(color = "grey30", size = .5)
    )

  # lib_type
  p3 <- df2 %>%
    dplyr::group_by(lib_type) %>%
    dplyr::summarise(x = sum(count)) %>%
    ggplot(aes(lib_type, x)) +
    geom_col() +
    geom_text(aes(label = x), vjust = -.2) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
    xlab(NULL) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.y = element_line(color = "grey30", size = .5)
    )


  p3 + plot_spacer() + p1 + p2 + plot_layout(heights = c(1, 5), widths = c(6, 1))
}


##--------------------------##
## saving data
df1 <- load_data("data/summary.xlsx")

df2 <- fix_summary_lib_user("data/summary.xlsx", "data/db.summary.lib_user_fixed.rds")



