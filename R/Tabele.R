describe_df <- function(df) {
  n <- ncol(df)
  
  df_new <- data.frame(
    col_index = seq_len(n),
    col_name = names(df),
    typeof = sapply(df, typeof),
    class = sapply(df, function(x) paste(class(x), collapse = "/")),
    n_unique = sapply(df, function(x) length(unique(x))),
    n_na = sapply(df, function(x) sum(is.na(x))),
    pct_na = sapply(df, function(x) round(mean(is.na(x)) * 100, 2)),
    n_zero = sapply(df, function(x) if(is.numeric(x)) sum(x == 0, na.rm = TRUE) else NA),
    pct_zero = sapply(df, function(x) if(is.numeric(x)) round(mean(x == 0, na.rm = TRUE) * 100, 2) else NA),
    n_empty = sapply(df, function(x) if(is.character(x) | is.factor(x)) sum(trimws(x) == "", na.rm = TRUE) else NA),
    pct_empty = sapply(df, function(x) if(is.character(x) | is.factor(x)) round(mean(trimws(x) == "", na.rm = TRUE) * 100, 2) else NA),
    n_null = sapply(df, function(x) if(is.list(x)) sum(sapply(x, is.null)) else NA),
    pct_null = sapply(df, function(x) if(is.list(x)) round(mean(sapply(x, is.null)) * 100, 2) else NA),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  return(df_new)
}
describe_stats <- function(df) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  mode_func <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  df_new <- data.frame(
    col_index = seq_len(length(numeric_cols)),
    col_name = numeric_cols,
    mean = round(sapply(df[numeric_cols], function(x) mean(x, na.rm = TRUE)), 2),
    mode = sapply(df[numeric_cols], mode_func),
    min = round(sapply(df[numeric_cols], function(x) min(x, na.rm = TRUE)), 2),
    Q1 = round(sapply(df[numeric_cols], function(x) quantile(x, 0.25, na.rm = TRUE)), 2),
    median = round(sapply(df[numeric_cols], function(x) median(x, na.rm = TRUE)), 2),
    Q3 = round(sapply(df[numeric_cols], function(x) quantile(x, 0.75, na.rm = TRUE)), 2),
    max = round(sapply(df[numeric_cols], function(x) max(x, na.rm = TRUE)), 2),
    sd = round(sapply(df[numeric_cols], function(x) sd(x, na.rm = TRUE)), 2),
    IQR = round(sapply(df[numeric_cols], function(x) IQR(x, na.rm = TRUE)), 2),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  
  return(df_new)
}
make_numeric_sliders <- function(sharedData, df, step_quantile = 0.01, width = "100%", digits = 2) {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  sliders <- lapply(numeric_cols, function(col) {
    x <- df[[col]]
    x_no_na <- x[!is.na(x)]
    
    if(length(unique(x_no_na)) > 1) {
      q <- quantile(x_no_na, probs = seq(0, 1, by = step_quantile))
      step_val <- round(min(diff(unique(q))), digits)
      step_val <- max(step_val, 10^-digits)
    } else {
      step_val <- 1
    }
    
    filter_slider(
      id = col,
      label = col,
      sharedData = sharedData,
      column = as.formula(paste0("~", col)),
      ticks = TRUE,
      dragRange = FALSE,
      step = step_val,
      width = width
    )
  })
  
  names(sliders) <- numeric_cols
  sliders
}

make_factor_checkboxes <- function(sharedData, df) {
  factor_cols <- names(df)[sapply(df, function(x) is.factor(x))]
  
  checkboxes <- lapply(factor_cols, function(col) {
    filter_checkbox(
      id = col,
      label = col,
      sharedData = sharedData,
      group = as.formula(paste0("~", col)),
      inline = TRUE
    )
  })
  
  names(checkboxes) <- factor_cols
  checkboxes
}
make_character_selects <- function(sharedData,df) {
  char_cols <- names(df)[sapply(df, is.character)]
  
  selects <- lapply(char_cols, function(col) {
    filter_select(
      id = col,
      label = col,
      sharedData = sharedData,
      group = as.formula(paste0("~", col))
    )
  })
  names(selects) <- char_cols
  selects
}
reactable_counts <- function(df) {
  
  cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  if(length(cols) == 0) return(tags$p("Brak kolumn typu character lub factor."))
  
  tables <- lapply(cols, function(col_name) {
    
    tab <- df %>%
      count(!!sym(col_name), name = "Ilość zliczeń") %>%
      rename(Wartość = !!sym(col_name))
    
    shared_tab <- SharedData$new(tab, key = ~Wartość, group = col_name)
    
    slider_id <- paste0("slider_", col_name)
    
    min_val <- min(tab$`Ilość zliczeń`, na.rm = TRUE)
    max_val <- max(tab$`Ilość zliczeń`, na.rm = TRUE)
    step_val <- round((max_val - min_val)/10000,0)
    step_val <- max(step_val, 1)
    
    slider <- filter_slider(
      id = slider_id,
      label = "Minimalna liczba zliczeń",
      sharedData = shared_tab,
      column = ~`Ilość zliczeń`,
      step = step_val
    )
    
    tbl <- reactable(shared_tab,
                     striped = TRUE,
                     bordered = TRUE,
                     highlight = TRUE,
                     pagination = TRUE,
                     defaultPageSize = 15,
                     columns = list(
                       Wartość = colDef(align = "left"),
                       `Ilość zliczeń` = colDef(align = "center")
                     ))
    
    div(
      style = "margin-bottom: 40px;",
      tags$h3(col_name),
      slider,
      tbl
    )
  })
  
  browsable(tagList(tables))
}
