# ============================================================================== 
# svySE: tablas simples con frecuencias sin expandir y expandidas
# svySE: simple tables with unweighted and weighted frequencies
# Archivo / File: R/simple.R
# ============================================================================== 

#' Calculate simple indicator tables
#'
#' Calcula frecuencias y porcentajes simples, frecuencias expandidas o ambos
#' tipos de resultados para uno o mas indicadores, sin calcular errores
#' muestrales.
#'
#' Calculates unweighted frequencies and percentages, weighted frequencies, or
#' both types of results for one or more indicators, without calculating
#' sampling errors.
#'
#' @param data Base de datos previamente cargada en R.
#' @param indicators Vector de texto con los nombres de los indicadores.
#' @param group_vars Variables utilizadas para agrupar los resultados.
#' @param group_labels Etiquetas de las variables de agrupacion.
#' @param division Variable opcional para generar resultados separados por
#'   categoria. Si es `NULL`, solo se calcula el total.
#' @param weight Variable de peso utilizada para calcular frecuencias
#'   expandidas. Es obligatoria cuando `output = "weighted"` o
#'   `output = "both"`.
#' @param output Tipo de resultado: `"unweighted"`, `"weighted"` o `"both"`.
#' @param target Valor que identifica la categoria de interes.
#' @param valid_values Valores permitidos en los indicadores.
#' @param pct_mult Multiplicador utilizado para expresar los porcentajes.
#' @param na_rm Si es `TRUE`, excluye valores perdidos del indicador.
#' @param verbose Si es `TRUE`, muestra el avance del calculo.
#' @param strict Si es `TRUE`, detiene el calculo ante valores no permitidos.
#'
#' @details
#' Con `output = "unweighted"` se calculan `freq_0`, `pct_0`, `freq_1`,
#' `pct_1`, `freq_total` y `pct_total`.
#'
#' Con `output = "weighted"` se calculan `exp_0`, `exp_1` y `exp_total`.
#' Con `output = "both"` se calculan ambos conjuntos de columnas.
#'
#' Los registros con peso perdido no aportan a las frecuencias expandidas,
#' pero permanecen en las frecuencias sin expandir cuando `output = "both"`.
#'
#' @return Objeto de clase `"svySE_simple_result"`.
#' @export
svySE_simple <- function(
    data,
    indicators,
    group_vars,
    group_labels = group_vars,
    division = NULL,
    weight = NULL,
    output = c("unweighted", "weighted", "both"),
    target = 1,
    valid_values = c(0, 1),
    pct_mult = 100,
    na_rm = TRUE,
    verbose = TRUE,
    strict = FALSE
) {
  
  output <- match.arg(output)
  
  svySE_chk_df(data)
  svySE_chk_chr(indicators, "indicators")
  svySE_chk_chr(group_vars, "group_vars")
  svySE_chk_chr(group_labels, "group_labels")
  svySE_chk_bool(na_rm, "na_rm")
  svySE_chk_bool(verbose, "verbose")
  svySE_chk_bool(strict, "strict")
  
  if (length(target) != 1 || is.na(target)) {
    svySE_abort(
      title = "Valor objetivo invalido / Invalid target value.",
      details = "`target` debe contener un solo valor no perdido.",
      vars = list(target = target)
    )
  }
  
  if (length(valid_values) < 2 || any(is.na(valid_values))) {
    svySE_abort(
      title = "Valores validos incorrectos / Invalid valid values.",
      details = paste(
        "`valid_values` debe contener al menos dos valores no perdidos.",
        "`valid_values` must contain at least two non-missing values."
      ),
      vars = list(valid_values = valid_values)
    )
  }
  
  if (!(target %in% valid_values)) {
    svySE_abort(
      title = "El valor objetivo no es valido / Target is not valid.",
      details = "`target` debe estar incluido dentro de `valid_values`.",
      vars = list(target = target, valid_values = valid_values)
    )
  }
  
  if (
    !is.numeric(pct_mult) ||
    length(pct_mult) != 1 ||
    is.na(pct_mult) ||
    !is.finite(pct_mult) ||
    pct_mult <= 0
  ) {
    svySE_abort(
      title = "Multiplicador de porcentaje invalido / Invalid percentage multiplier.",
      details = "`pct_mult` debe ser un numero positivo y finito.",
      vars = list(pct_mult = pct_mult)
    )
  }
  
  if (!is.null(division)) {
    svySE_chk_chr(division, "division")
    
    if (length(division) != 1) {
      svySE_abort(
        title = "Variable de division invalida / Invalid division variable.",
        details = "`division` debe contener una sola variable.",
        vars = list(division = division)
      )
    }
  }
  
  if (!is.null(weight)) {
    svySE_chk_chr(weight, "weight")
    
    if (length(weight) != 1L) {
      svySE_abort(
        title = "Peso invalido / Invalid weight.",
        details = "`weight` debe contener una sola variable.",
        vars = list(weight = weight)
      )
    }
  }
  
  if (output %in% c("weighted", "both") && is.null(weight)) {
    svySE_abort(
      title = paste(
        "Peso requerido para frecuencias expandidas /",
        "Weight required for weighted frequencies."
      ),
      details = paste(
        "Cuando `output` es `\"weighted\"` o `\"both\"`,",
        "debe especificarse una variable en `weight`."
      ),
      hint = paste(
        "Ejemplo / Example:",
        'svySE_simple(..., weight = "FACTOR_TOTAL", output = "both")'
      )
    )
  }
  
  svySE_chk_vars(data, indicators, "indicators")
  svySE_chk_vars(data, group_vars, "group_vars")
  
  if (!is.null(division)) {
    svySE_chk_vars(data, division, "division")
  }
  
  if (!is.null(weight)) {
    svySE_chk_vars(data, weight, "weight")
  }
  
  if (length(group_labels) != length(group_vars)) {
    svySE_abort(
      title = "Etiquetas de grupo incompatibles / Incompatible group labels.",
      details = paste(
        "`group_labels` debe tener la misma longitud que `group_vars`.",
        "`group_labels` must have the same length as `group_vars`."
      ),
      vars = list(group_vars = group_vars, group_labels = group_labels)
    )
  }
  
  svySE_chk_required_no_na(
    data = data,
    vars = group_vars,
    context = "variables de agrupacion / grouping variables"
  )
  
  if (!is.null(division)) {
    svySE_warn_na_division(data, division)
  }
  
  data <- as.data.frame(data)
  
  if (!is.null(weight)) {
    data <- svySE_prepare_weight(
      data = data,
      weight = weight,
      arg = "weight"
    )
    
    if (
      output %in% c("weighted", "both") &&
      any(is.na(data[[weight]]))
    ) {
      warning(
        paste0(
          "La variable de peso `", weight, "` contiene ",
          sum(is.na(data[[weight]])),
          " valores perdidos. Estos registros no aportaran a las ",
          "frecuencias expandidas."
        ),
        call. = FALSE
      )
    }
  }
  
  data$.__svySE_group_id__ <- do.call(
    paste,
    c(data[group_vars], sep = " | ")
  )
  
  out <- list()
  
  for (ind in indicators) {
    
    if (isTRUE(verbose)) {
      message("Procesando tabla simple / Processing simple table: ", ind)
    }
    
    x <- data[[ind]]
    n_missing <- sum(is.na(x))
    
    if (!isTRUE(na_rm) && n_missing > 0) {
      svySE_abort(
        title = "Indicador con valores perdidos / Indicator contains missing values.",
        details = paste0(
          "El indicador `", ind,
          "` contiene ", n_missing,
          " valores perdidos y `na_rm = FALSE`."
        ),
        vars = list(indicator = ind, n_missing = n_missing, na_rm = na_rm),
        hint = paste(
          "Usa `na_rm = TRUE` para excluir los valores perdidos",
          "del calculo de frecuencias y porcentajes."
        )
      )
    }
    
    invalid_values <- setdiff(
      unique(x[!is.na(x)]),
      valid_values
    )
    
    if (length(invalid_values) > 0) {
      
      msg <- paste0(
        "El indicador `", ind,
        "` contiene valores fuera de `valid_values`: ",
        paste(invalid_values, collapse = ", "),
        ". Estos registros seran excluidos."
      )
      
      if (isTRUE(strict)) {
        svySE_abort(
          title = "Indicador con valores no permitidos / Indicator with invalid values.",
          details = msg,
          vars = list(
            indicator = ind,
            valid_values = valid_values,
            invalid_values = invalid_values,
            observed_values = svySE_observed_values(x)
          )
        )
      } else {
        warning(msg, call. = FALSE)
      }
    }
    
    data_ind <- data[
      data[[ind]] %in% valid_values &
        (isTRUE(na_rm) | !is.na(data[[ind]])),
      ,
      drop = FALSE
    ]
    
    if (nrow(data_ind) == 0) {
      svySE_abort(
        title = "Indicador sin registros validos / Indicator without valid records.",
        details = paste0(
          "El indicador `", ind,
          "` no tiene registros dentro de `valid_values`."
        ),
        vars = list(
          indicator = ind,
          valid_values = valid_values,
          na_rm = na_rm
        )
      )
    }
    
    groups_master <- sort(
      unique(data_ind$.__svySE_group_id__)
    )
    
    if (length(groups_master) == 0) {
      svySE_abort(
        title = "No se encontraron grupos validos / No valid groups found.",
        details = paste0(
          "No quedaron grupos con registros validos para el indicador `",
          ind, "`."
        ),
        vars = list(indicator = ind, group_vars = group_vars)
      )
    }
    
    data_ind$.__svySE_cat__ <- ifelse(
      data_ind[[ind]] == target,
      1,
      0
    )
    
    filters <- list(
      TOTAL = list(
        name = "TOTAL",
        data = data_ind
      )
    )
    
    if (!is.null(division)) {
      
      categories <- sort(unique(data_ind[[division]]))
      categories <- categories[!is.na(categories)]
      
      if (length(categories) == 0) {
        svySE_abort(
          title = paste(
            "Variable de division sin categorias validas /",
            "Division variable without valid categories."
          ),
          vars = list(division = division)
        )
      }
      
      for (category in categories) {
        filters[[as.character(category)]] <- list(
          name = as.character(category),
          data = data_ind[
            data_ind[[division]] == category,
            ,
            drop = FALSE
          ]
        )
      }
    }
    
    simple_list <- list()
    
    for (i in seq_along(filters)) {
      f <- filters[[i]]
      
      simple_list[[f$name]] <- svySE_simple_one(
        data = f$data,
        group_vars = group_vars,
        groups_master = groups_master,
        weight = weight,
        output = output,
        pct_mult = pct_mult
      )
    }
    
    out[[ind]] <- list(simple = simple_list)
  }
  
  result <- list(
    results = out,
    meta = list(
      indicators = indicators,
      group_vars = group_vars,
      group_labels = group_labels,
      strata = NULL,
      cluster = NULL,
      weight = weight,
      division = division,
      div_weight = NULL,
      output = output,
      target = target,
      valid_values = valid_values,
      pct_mult = pct_mult,
      na_rm = na_rm,
      strict = strict
    )
  )
  
  class(result) <- c("svySE_simple_result", "list")
  result
}


# ==============================================================================
# Nombres de metricas simples
# ============================================================================== 

#' @keywords internal
svySE_simple_metric_names <- function(
    output = c("unweighted", "weighted", "both")
) {
  
  output <- match.arg(output)
  
  unweighted_cols <- c(
    "freq_0",
    "pct_0",
    "freq_1",
    "pct_1",
    "freq_total",
    "pct_total"
  )
  
  weighted_cols <- c(
    "exp_0",
    "exp_1",
    "exp_total"
  )
  
  switch(
    output,
    unweighted = unweighted_cols,
    weighted = weighted_cols,
    both = c(unweighted_cols, weighted_cols)
  )
}


# ==============================================================================
# Construccion de metricas simples
# ============================================================================== 

#' @keywords internal
svySE_simple_metrics <- function(
    data,
    weight = NULL,
    output = c("unweighted", "weighted", "both"),
    pct_mult = 100
) {
  
  output <- match.arg(output)
  
  n_total <- nrow(data)
  n_target <- sum(data$.__svySE_cat__ == 1, na.rm = TRUE)
  n_other <- sum(data$.__svySE_cat__ == 0, na.rm = TRUE)
  
  unweighted_metrics <- data.frame(
    freq_0 = n_other,
    pct_0 = if (n_total > 0) n_other / n_total * pct_mult else NA_real_,
    freq_1 = n_target,
    pct_1 = if (n_total > 0) n_target / n_total * pct_mult else NA_real_,
    freq_total = n_total,
    pct_total = if (n_total > 0) pct_mult else NA_real_
  )
  
  weighted_metrics <- NULL
  
  if (output %in% c("weighted", "both")) {
    
    if (is.null(weight)) {
      svySE_abort(
        title = paste(
          "Peso requerido para frecuencias expandidas /",
          "Weight required for weighted frequencies."
        )
      )
    }
    
    valid_weight <- !is.na(data[[weight]])
    
    weighted_metrics <- data.frame(
      exp_0 = sum(
        data[[weight]][
          data$.__svySE_cat__ == 0 & valid_weight
        ],
        na.rm = TRUE
      ),
      exp_1 = sum(
        data[[weight]][
          data$.__svySE_cat__ == 1 & valid_weight
        ],
        na.rm = TRUE
      ),
      exp_total = sum(
        data[[weight]][valid_weight],
        na.rm = TRUE
      )
    )
  }
  
  switch(
    output,
    unweighted = unweighted_metrics,
    weighted = weighted_metrics,
    both = cbind(unweighted_metrics, weighted_metrics)
  )
}


# ==============================================================================
# Calculo de una tabla simple
# ============================================================================== 

#' @keywords internal
svySE_simple_one <- function(
    data,
    group_vars,
    groups_master,
    weight = NULL,
    output = c("unweighted", "weighted", "both"),
    pct_mult = 100
) {
  
  output <- match.arg(output)
  metric_names <- svySE_simple_metric_names(output)
  
  out <- data.frame()
  groups_in_data <- sort(unique(data$.__svySE_group_id__))
  
  for (g in groups_master) {
    
    if (!(g %in% groups_in_data)) {
      row_na <- svySE_na_row(
        group_id = g,
        group_vars = group_vars,
        metric_names = metric_names
      )
      
      out <- rbind(out, row_na)
      next
    }
    
    group_data <- data[
      data$.__svySE_group_id__ == g,
      ,
      drop = FALSE
    ]
    
    group_row <- group_data[1, group_vars, drop = FALSE]
    
    metrics <- svySE_simple_metrics(
      data = group_data,
      weight = weight,
      output = output,
      pct_mult = pct_mult
    )
    
    out <- rbind(out, cbind(group_row, metrics))
  }
  
  total_group <- as.data.frame(
    as.list(rep("NACIONAL", length(group_vars))),
    stringsAsFactors = FALSE
  )
  
  names(total_group) <- group_vars
  
  total_metrics <- svySE_simple_metrics(
    data = data,
    weight = weight,
    output = output,
    pct_mult = pct_mult
  )
  
  out <- rbind(
    cbind(total_group, total_metrics),
    out
  )
  
  rownames(out) <- NULL
  out
}


# ==============================================================================
# Impresion del resultado simple
# ============================================================================== 

#' Print an svySE simple result
#'
#' @param x Objeto de clase `"svySE_simple_result"`.
#' @param ... Argumentos adicionales.
#' @return Invisiblemente `x`.
#' @export
print.svySE_simple_result <- function(x, ...) {
  
  cat("svySE simple result\n")
  cat("--------------------------------------------------\n")
  cat("Indicators :", paste(x$meta$indicators, collapse = ", "), "\n")
  cat("Groups     :", paste(x$meta$group_vars, collapse = ", "), "\n")
  cat(
    "Division   :",
    if (is.null(x$meta$division)) "NULL" else x$meta$division,
    "\n"
  )
  cat(
    "Weight     :",
    if (is.null(x$meta$weight)) "NULL" else x$meta$weight,
    "\n"
  )
  cat("Output     :", x$meta$output, "\n")
  cat("Target     :", x$meta$target, "\n")
  cat("Remove NA  :", x$meta$na_rm, "\n")
  
  if (identical(x$meta$output, "unweighted")) {
    cat("Weighted   : No\n")
    cat("Warning    : Results describe the observed sample only.\n")
  } else if (identical(x$meta$output, "weighted")) {
    cat("Weighted   : Yes\n")
    cat("Warning    : Weighted frequencies use the specified weight.\n")
  } else {
    cat("Weighted   : Both weighted and unweighted results\n")
    cat(
      "Warning    : Unweighted columns describe the observed sample; ",
      "expanded columns use the specified weight.\n",
      sep = ""
    )
  }
  
  invisible(x)
}
