# ==============================================================================
# svySE: seleccion de columnas para exportacion
# svySE: column selection for export
# Archivo / File: R/columnas.R
# ==============================================================================

#' Available sampling error columns
#'
#' Devuelve todas las columnas metricas disponibles para la tabla de errores
#' muestrales.
#' Returns all available metric columns for the sampling error table.
#'
#' @return Vector de nombres de columnas / Character vector of column names.
#'
#' @export
svySE_cols_err_all <- function() {
  
  c(
    "est_abs",
    "est_pct",
    "se_abs",
    "se_pct",
    "ci_l_abs",
    "ci_l_pct",
    "ci_u_abs",
    "ci_u_pct",
    "cv",
    "deff",
    "n_unw"
  )
}


#' Available simple table columns
#'
#' Devuelve todas las columnas disponibles para la tabla simple.
#' Returns all available columns for the simple table.
#'
#' @return Vector de nombres de columnas / Character vector of column names.
#'
#' @export
svySE_cols_tab_all <- function() {
  
  c(
    "freq_0",
    "pct_0",
    "freq_1",
    "pct_1",
    "freq_total",
    "pct_total"
  )
}


#' Select sampling error columns
#'
#' Selecciona columnas para exportar la tabla de errores muestrales.
#' Selects columns to export the sampling error table.
#'
#' @param type Tipo de salida / Output type. Opciones:
#'   `"full"`, `"pct"`, `"abs"`, `"basic"`, `"quality"` o `"custom"`.
#' @param cols Vector de columnas cuando `type = "custom"`.
#'
#' @return Vector de columnas seleccionadas / Selected column vector.
#'
#' @examples
#' svySE_cols_err("full")
#' svySE_cols_err("pct")
#' svySE_cols_err("custom", cols = c("est_pct", "cv"))
#'
#' @export
svySE_cols_err <- function(
    type = "full",
    cols = NULL
) {
  
  # ---------------------------------------------------------------------------
  # Validar tipo de salida
  # Validate output type
  # ---------------------------------------------------------------------------
  
  type <- match.arg(
    type,
    choices = c("full", "pct", "abs", "basic", "quality", "custom")
  )
  
  all_cols <- svySE_cols_err_all()
  
  # ---------------------------------------------------------------------------
  # Seleccionar columnas segun perfil
  # Select columns according to profile
  # ---------------------------------------------------------------------------
  
  out <- switch(
    type,
    
    full = all_cols,
    
    pct = c(
      "est_pct",
      "se_pct",
      "ci_l_pct",
      "ci_u_pct",
      "cv"
    ),
    
    abs = c(
      "est_abs",
      "se_abs",
      "ci_l_abs",
      "ci_u_abs",
      "cv",
      "deff",
      "n_unw"
    ),
    
    basic = c(
      "est_abs",
      "est_pct",
      "se_abs",
      "se_pct",
      "cv"
    ),
    
    quality = c(
      "est_pct",
      "se_pct",
      "cv",
      "deff",
      "n_unw"
    ),
    
    custom = cols
  )
  
  # ---------------------------------------------------------------------------
  # Validar columnas seleccionadas
  # Validate selected columns
  # ---------------------------------------------------------------------------
  
  svySE_chk_cols(
    cols = out,
    all_cols = all_cols,
    arg = "cols"
  )
  
  out
}


#' Select simple table columns
#'
#' Selecciona columnas para exportar la tabla simple.
#' Selects columns to export the simple table.
#'
#' @param type Tipo de salida / Output type. Opciones:
#'   `"full"`, `"target"`, `"freq"`, `"pct"` o `"custom"`.
#' @param cols Vector de columnas cuando `type = "custom"`.
#'
#' @return Vector de columnas seleccionadas / Selected column vector.
#'
#' @examples
#' svySE_cols_tab("full")
#' svySE_cols_tab("target")
#' svySE_cols_tab("custom", cols = c("freq_1", "pct_1"))
#'
#' @export
svySE_cols_tab <- function(
    type = "full",
    cols = NULL
) {
  
  # ---------------------------------------------------------------------------
  # Validar tipo de salida
  # Validate output type
  # ---------------------------------------------------------------------------
  
  type <- match.arg(
    type,
    choices = c("full", "target", "freq", "pct", "custom")
  )
  
  all_cols <- svySE_cols_tab_all()
  
  # ---------------------------------------------------------------------------
  # Seleccionar columnas segun perfil
  # Select columns according to profile
  # ---------------------------------------------------------------------------
  
  out <- switch(
    type,
    
    full = all_cols,
    
    target = c(
      "freq_1",
      "pct_1",
      "freq_total",
      "pct_total"
    ),
    
    freq = c(
      "freq_0",
      "freq_1",
      "freq_total"
    ),
    
    pct = c(
      "pct_0",
      "pct_1",
      "pct_total"
    ),
    
    custom = cols
  )
  
  # ---------------------------------------------------------------------------
  # Validar columnas seleccionadas
  # Validate selected columns
  # ---------------------------------------------------------------------------
  
  svySE_chk_cols(
    cols = out,
    all_cols = all_cols,
    arg = "cols"
  )
  
  out
}


# ==============================================================================
# Funciones internas
# Internal functions
# ==============================================================================

#' Check selected columns
#'
#' Valida que las columnas seleccionadas existan dentro del conjunto permitido.
#' Checks whether selected columns exist in the allowed column set.
#'
#' @param cols Columnas seleccionadas / Selected columns.
#' @param all_cols Columnas disponibles / Available columns.
#' @param arg Nombre del argumento / Argument name.
#'
#' @return Invisiblemente `TRUE`.
#'
#' @keywords internal
svySE_chk_cols <- function(
    cols,
    all_cols,
    arg = "cols"
) {
  
  if (is.null(cols) || length(cols) == 0) {
    stop(
      paste0(
        "`", arg, "` debe contener al menos una columna / ",
        "must contain at least one column."
      ),
      call. = FALSE
    )
  }
  
  if (!is.character(cols)) {
    stop(
      paste0(
        "`", arg, "` debe ser un vector de texto / ",
        "must be a character vector."
      ),
      call. = FALSE
    )
  }
  
  bad <- setdiff(cols, all_cols)
  
  if (length(bad) > 0) {
    stop(
      paste0(
        "Columnas no validas / Invalid columns: ",
        paste(bad, collapse = ", "),
        ". Columnas disponibles / Available columns: ",
        paste(all_cols, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}
