# ==============================================================================
# svySE: configuracion general para errores muestrales
# svySE: general configuration for sampling errors
# Archivo / File: R/config.R
# ==============================================================================

#' Configure sampling error parameters
#'
#' Crea una configuracion general para el calculo de errores muestrales.
#' Creates a general configuration object for sampling error estimation.
#'
#' @param estimator Tipo de estimador. Opciones: `"prop"`, `"total"`,
#'   `"mean"` o `"ratio"`.
#' @param variance Metodo de estimacion de varianza. Actualmente solo `"taylor"`.
#' @param lonely_psu Tratamiento de estratos con una sola UPM. Opciones:
#'   `"adjust"`, `"average"`, `"certainty"`, `"remove"` o `"fail"`.
#' @param conf_level Nivel de confianza. Por defecto `0.95`.
#' @param target Valor objetivo del indicador. Por defecto `1`.
#' @param valid_values Valores validos del indicador. Por defecto `c(0, 1)`.
#' @param truncate_lower_ci Si es `TRUE`, trunca el limite inferior del IC en cero.
#' @param pct_mult Multiplicador para expresar proporciones como porcentajes.
#'   Por defecto `100`.
#' @param deff Si es `TRUE`, calcula efecto de diseno cuando sea posible.
#' @param cv Si es `TRUE`, calcula coeficiente de variacion.
#' @param na_rm Si es `TRUE`, remueve valores perdidos en operaciones auxiliares.
#'
#' @return Lista de clase `"svySE_cfg"`.
#'
#' @examples
#' cfg <- svySE_cfg(
#'   estimator = "prop",
#'   variance = "taylor",
#'   target = 1
#' )
#'
#' @export
svySE_cfg <- function(
    estimator = "prop",
    variance = "taylor",
    lonely_psu = "adjust",
    conf_level = 0.95,
    target = 1,
    valid_values = c(0, 1),
    truncate_lower_ci = TRUE,
    pct_mult = 100,
    deff = TRUE,
    cv = TRUE,
    na_rm = TRUE
) {
  
  # ---------------------------------------------------------------------------
  # Validar tipo de estimador
  # Validate estimator type
  # ---------------------------------------------------------------------------
  
  estimator <- match.arg(
    estimator,
    choices = c("prop", "total", "mean", "ratio")
  )
  
  # ---------------------------------------------------------------------------
  # Validar metodo de varianza
  # Validate variance method
  # ---------------------------------------------------------------------------
  
  variance <- match.arg(
    variance,
    choices = c("taylor")
  )
  
  # ---------------------------------------------------------------------------
  # Validar tratamiento de lonely PSU
  # Validate lonely PSU treatment
  # ---------------------------------------------------------------------------
  
  lonely_psu <- match.arg(
    lonely_psu,
    choices = c("adjust", "average", "certainty", "remove", "fail")
  )
  
  # ---------------------------------------------------------------------------
  # Validar nivel de confianza
  # Validate confidence level
  # ---------------------------------------------------------------------------
  
  if (!is.numeric(conf_level) || length(conf_level) != 1) {
    stop("`conf_level` debe ser un valor numerico unico / must be a single numeric value.",
         call. = FALSE)
  }
  
  if (is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` debe estar entre 0 y 1 / must be between 0 and 1.",
         call. = FALSE)
  }
  
  # ---------------------------------------------------------------------------
  # Validar valor objetivo
  # Validate target value
  # ---------------------------------------------------------------------------
  
  if (length(target) != 1) {
    stop("`target` debe contener un solo valor / must contain one single value.",
         call. = FALSE)
  }
  
  if (is.null(valid_values) || length(valid_values) == 0) {
    stop("`valid_values` debe contener al menos un valor / must contain at least one value.",
         call. = FALSE)
  }
  
  if (!(target %in% valid_values)) {
    stop("`target` debe estar incluido en `valid_values` / must be included in `valid_values`.",
         call. = FALSE)
  }
  
  # ---------------------------------------------------------------------------
  # Validar multiplicador porcentual
  # Validate percentage multiplier
  # ---------------------------------------------------------------------------
  
  if (!is.numeric(pct_mult) || length(pct_mult) != 1) {
    stop("`pct_mult` debe ser un valor numerico unico / must be a single numeric value.",
         call. = FALSE)
  }
  
  if (is.na(pct_mult) || pct_mult <= 0) {
    stop("`pct_mult` debe ser mayor que cero / must be greater than zero.",
         call. = FALSE)
  }
  
  # ---------------------------------------------------------------------------
  # Validar argumentos logicos
  # Validate logical arguments
  # ---------------------------------------------------------------------------
  
  svySE_chk_bool(truncate_lower_ci, "truncate_lower_ci")
  svySE_chk_bool(deff, "deff")
  svySE_chk_bool(cv, "cv")
  svySE_chk_bool(na_rm, "na_rm")
  
  # ---------------------------------------------------------------------------
  # Aplicar configuracion global del paquete survey
  # Apply global survey package option
  # ---------------------------------------------------------------------------
  
  options(survey.lonely.psu = lonely_psu)
  
  # ---------------------------------------------------------------------------
  # Crear objeto de configuracion
  # Create configuration object
  # ---------------------------------------------------------------------------
  
  out <- list(
    estimator = estimator,
    variance = variance,
    lonely_psu = lonely_psu,
    conf_level = conf_level,
    target = target,
    valid_values = valid_values,
    truncate_lower_ci = truncate_lower_ci,
    pct_mult = pct_mult,
    deff = deff,
    cv = cv,
    na_rm = na_rm
  )
  
  class(out) <- c("svySE_cfg", "list")
  
  out
}


# ==============================================================================
# Funciones internas
# Internal functions
# ==============================================================================

#' Check logical scalar
#'
#' Valida que un argumento sea logico de longitud uno.
#' Checks whether an argument is a single logical value.
#'
#' @param x Objeto a validar / Object to validate.
#' @param arg Nombre del argumento / Argument name.
#'
#' @return Invisiblemente `TRUE`.
#'
#' @keywords internal
svySE_chk_bool <- function(x, arg) {
  
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(
      paste0(
        "`", arg, "` debe ser TRUE o FALSE / must be TRUE or FALSE."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


#' Check svySE configuration object
#'
#' Verifica si un objeto pertenece a la clase `svySE_cfg`.
#' Checks whether an object belongs to class `svySE_cfg`.
#'
#' @param x Objeto a evaluar / Object to check.
#'
#' @return `TRUE` o `FALSE`.
#'
#' @keywords internal
svySE_is_cfg <- function(x) {
  inherits(x, "svySE_cfg")
}


#' Print svySE configuration
#'
#' Imprime la configuracion de `svySE`.
#' Prints the `svySE` configuration.
#'
#' @param x Objeto de clase `"svySE_cfg"`.
#' @param ... Argumentos adicionales.
#'
#' @return Invisiblemente el objeto `x`.
#'
#' @export
print.svySE_cfg <- function(x, ...) {
  
  cat("svySE configuration\n")
  cat("--------------------------------------------------\n")
  cat("Estimator          :", x$estimator, "\n")
  cat("Variance           :", x$variance, "\n")
  cat("Lonely PSU         :", x$lonely_psu, "\n")
  cat("Confidence level   :", x$conf_level, "\n")
  cat("Target value       :", x$target, "\n")
  cat("Valid values       :", paste(x$valid_values, collapse = ", "), "\n")
  cat("Truncate lower CI  :", x$truncate_lower_ci, "\n")
  cat("Percentage mult.   :", x$pct_mult, "\n")
  cat("Include DEFF       :", x$deff, "\n")
  cat("Include CV         :", x$cv, "\n")
  cat("Remove NA          :", x$na_rm, "\n")
  
  invisible(x)
}
