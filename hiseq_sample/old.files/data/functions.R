

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
