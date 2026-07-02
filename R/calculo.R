# ==============================================================================
# svySE: calculo de errores muestrales
# svySE: sampling error calculation
# Archivo / File: R/calculo.R
# ==============================================================================


#' Calculate sampling errors
#'
#' Calcula errores muestrales para uno o mas indicadores usando un data.frame
#' previamente cargado por el usuario.
#'
#' Calculates sampling errors for one or more indicators using a data.frame
#' previously loaded by the user.
#'
#' @param data Base de datos ya cargada en R / Data frame already loaded in R.
#' @param indicators Vector con los nombres de los indicadores.
#' @param group_vars Variables de agrupacion.
#' @param group_labels Etiquetas de las variables de agrupacion.
#' @param strata Variable o variables de estrato.
#' @param weight Variable de peso principal.
#' @param division Variable de division opcional.
#' @param div_weight Variable de peso opcional para las divisiones.
#' @param cfg Configuracion creada con `svySE_cfg()`.
#' @param verbose Si es `TRUE`, imprime avance del calculo.
#' @param strict Si es `TRUE`, detiene el calculo cuando encuentra valores
#'   fuera de `cfg$valid_values`; si es `FALSE`, los excluye con advertencia.
#'
#' @return Objeto de clase `"svySE_result"`.
#'
#' @importFrom stats update reformulate confint coef
#' @importFrom utils head
#' @export
svySE_calc <- function(
    data,
    indicators,
    group_vars,
    group_labels = group_vars,
    strata,
    weight,
    division = NULL,
    div_weight = NULL,
    cfg = svySE_cfg(),
    verbose = TRUE,
    strict = FALSE
) {
  
  # ---------------------------------------------------------------------------
  # Validaciones iniciales de configuracion
  # Initial configuration validations
  # ---------------------------------------------------------------------------
  
  svySE_chk_pkg("survey")
  
  if (!svySE_is_cfg(cfg)) {
    svySE_abort(
      title = "Configuracion invalida / Invalid configuration.",
      details = "`cfg` debe ser creado con `svySE_cfg()` / `cfg` must be created with `svySE_cfg()`.",
      hint = "Ejemplo / Example: cfg <- svySE_cfg(estimator = \"prop\", target = 1)"
    )
  }
  
  svySE_chk_bool(verbose, "verbose")
  svySE_chk_bool(strict, "strict")
  
  svySE_chk_estimator(
    cfg = cfg,
    indicators = indicators
  )
  
  # ---------------------------------------------------------------------------
  # Validaciones generales de datos y variables
  # General data and variable validations
  # ---------------------------------------------------------------------------
  
  svySE_chk_df(data)
  svySE_chk_chr(indicators, "indicators")
  svySE_chk_chr(group_vars, "group_vars")
  svySE_chk_chr(group_labels, "group_labels")
  svySE_chk_chr(strata, "strata")
  svySE_chk_chr(weight, "weight")
  
  if (length(weight) != 1) {
    svySE_abort(
      title = "Peso invalido / Invalid weight.",
      details = "`weight` debe contener una sola variable / `weight` must contain one variable.",
      vars = list(weight = weight)
    )
  }
  
  if (!is.null(division)) {
    svySE_chk_chr(division, "division")
    
    if (length(division) != 1) {
      svySE_abort(
        title = "Variable de division invalida / Invalid division variable.",
        details = "`division` debe contener una sola variable / `division` must contain one variable.",
        vars = list(division = division)
      )
    }
  }
  
  if (!is.null(div_weight)) {
    svySE_chk_chr(div_weight, "div_weight")
    
    if (length(div_weight) != 1) {
      svySE_abort(
        title = "Peso de division invalido / Invalid division weight.",
        details = "`div_weight` debe contener una sola variable / `div_weight` must contain one variable.",
        vars = list(div_weight = div_weight)
      )
    }
  }
  
  svySE_chk_vars(data, indicators, "indicators")
  svySE_chk_vars(data, group_vars, "group_vars")
  svySE_chk_vars(data, strata, "strata")
  svySE_chk_vars(data, weight, "weight")
  
  if (!is.null(division)) {
    svySE_chk_vars(data, division, "division")
  }
  
  if (!is.null(div_weight)) {
    svySE_chk_vars(data, div_weight, "div_weight")
  }
  
  if (length(group_labels) != length(group_vars)) {
    svySE_abort(
      title = "Etiquetas de grupo incompatibles / Incompatible group labels.",
      details = "`group_labels` debe tener la misma longitud que `group_vars` / `group_labels` must have the same length as `group_vars`.",
      vars = list(
        group_vars = group_vars,
        group_labels = group_labels
      ),
      hint = "Si solo agrupas por departamento, usa por ejemplo: group_vars = \"NOMBRECCDD\", group_labels = \"DEPARTAMENTO\"."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Copia interna para no modificar la base original
  # Internal copy to avoid modifying original data
  # ---------------------------------------------------------------------------
  
  data <- as.data.frame(data)
  
  data <- svySE_prepare_weight(
    data = data,
    weight = weight,
    arg = "weight"
  )
  
  if (!is.null(div_weight)) {
    data <- svySE_prepare_weight(
      data = data,
      weight = div_weight,
      arg = "div_weight"
    )
  }
  
  svySE_chk_required_no_na(
    data = data,
    vars = c(group_vars, strata),
    context = "variables de grupo y estrato / group and strata variables"
  )
  
  if (!is.null(division)) {
    svySE_warn_na_division(data, division)
  }
  
  # ---------------------------------------------------------------------------
  # Calculo por indicador
  # Calculation by indicator
  # ---------------------------------------------------------------------------
  
  out <- list()
  
  for (ind in indicators) {
    
    if (isTRUE(verbose)) {
      message("Procesando / Processing: ", ind)
    }
    
    res_ind <- tryCatch({
      
      svySE_chk_indicator_values(
        data = data,
        indicator = ind,
        cfg = cfg,
        strict = strict
      )
      
      data_ind <- svySE_prep_ind(
        data = data,
        indicator = ind,
        group_vars = group_vars,
        weight = weight,
        cfg = cfg
      )
      
      groups_master <- sort(unique(data_ind$.__svySE_group_id__))
      
      if (length(groups_master) == 0) {
        svySE_abort(
          title = "No se encontraron grupos validos / No valid groups found.",
          details = "Despues de filtrar valores validos del indicador y peso, no quedaron grupos para calcular.",
          vars = list(
            indicator = ind,
            group_vars = group_vars,
            weight = weight
          )
        )
      }
      
      filters <- svySE_filters(
        data = data_ind,
        division = division,
        weight = weight,
        div_weight = div_weight
      )
      
      err_list <- list()
      tab_list <- list()
      
      for (i in seq_along(filters)) {
        
        f <- filters[[i]]
        
        if (isTRUE(verbose)) {
          message("  Division / Division: ", f$name, " | Peso / Weight: ", f$weight)
        }
        
        res_div <- tryCatch({
          
          err_list[[f$name]] <- svySE_err_one(
            data = f$data,
            group_vars = group_vars,
            strata = strata,
            weight = f$weight,
            indicator = ind,
            groups_master = groups_master,
            cfg = cfg
          )
          
          tab_list[[f$name]] <- svySE_tab_one(
            data = f$data,
            group_vars = group_vars,
            indicator = ind,
            groups_master = groups_master,
            cfg = cfg
          )
          
          TRUE
          
        }, error = function(e) {
          
          svySE_abort(
            title = "Error al calcular una division / Error while calculating a division.",
            details = paste0(
              "El calculo fallo dentro del indicador `", ind,
              "` y la division `", f$name, "`."
            ),
            vars = list(
              indicator = ind,
              division = f$name,
              group_vars = group_vars,
              strata = strata,
              weight_used = f$weight,
              estimator = cfg$estimator,
              target = cfg$target,
              valid_values = cfg$valid_values
            ),
            hint = "Revisa que el peso no tenga valores negativos, que el estrato tenga registros validos y que el indicador tenga valores compatibles con `valid_values`.",
            parent = e
          )
        })
        
        invisible(res_div)
      }
      
      list(
        error = err_list,
        simple = tab_list
      )
      
    }, error = function(e) {
      
      svySE_abort(
        title = "Error al calcular un indicador / Error while calculating an indicator.",
        details = paste0("El calculo fallo para el indicador `", ind, "`."),
        vars = list(
          indicator = ind,
          group_vars = group_vars,
          strata = strata,
          weight = weight,
          division = if (is.null(division)) "NULL" else division,
          div_weight = if (is.null(div_weight)) "NULL" else div_weight,
          estimator = cfg$estimator,
          target = cfg$target,
          valid_values = cfg$valid_values
        ),
        hint = "Revisa el mensaje tecnico original al final. Usualmente el problema esta en valores invalidos, pesos no numericos, estratos sin variacion o uso incorrecto del estimador.",
        parent = e
      )
    })
    
    out[[ind]] <- res_ind
  }
  
  result <- list(
    results = out,
    meta = list(
      indicators = indicators,
      group_vars = group_vars,
      group_labels = group_labels,
      strata = strata,
      weight = weight,
      division = division,
      div_weight = div_weight,
      cfg = cfg,
      strict = strict
    )
  )
  
  class(result) <- c("svySE_result", "list")
  
  result
}


# ==============================================================================
# Calculo de tabla de error por indicador y division
# Error table calculation by indicator and division
# ==============================================================================

#' @keywords internal
svySE_err_one <- function(
    data,
    group_vars,
    strata,
    weight,
    indicator,
    groups_master,
    cfg
) {
  
  metric_names <- svySE_cols_err_all()
  
  out <- data.frame()
  
  groups_in_data <- sort(unique(data$.__svySE_group_id__))
  
  if (nrow(data) == 0) {
    return(
      svySE_empty_by_groups(
        groups_master = groups_master,
        group_vars = group_vars,
        metric_names = metric_names
      )
    )
  }
  
  design <- tryCatch(
    svySE_design(
      data = data,
      strata = strata,
      weight = weight
    ),
    error = function(e) {
      svySE_abort(
        title = "No se pudo construir el diseno muestral / Could not build survey design.",
        details = "El error ocurrio al ejecutar `survey::svydesign()`.",
        vars = list(
          strata = strata,
          weight = weight,
          n_rows = nrow(data)
        ),
        hint = "Verifica que el peso sea numerico, no negativo, no todo NA, y que la variable de estrato exista y no tenga valores perdidos.",
        parent = e
      )
    }
  )
  
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
    
    group_data <- data[data$.__svySE_group_id__ == g, , drop = FALSE]
    group_row <- group_data[1, group_vars, drop = FALSE]
    
    design_dom <- tryCatch(
      update(
        design,
        .__svySE_dom__ = ifelse(.__svySE_group_id__ == g, 1, 0)
      ),
      error = function(e) {
        svySE_abort(
          title = "No se pudo crear el dominio / Could not create domain.",
          details = paste0("Fallo la creacion del dominio para el grupo `", g, "`."),
          vars = list(
            group_id = g,
            group_vars = group_vars
          ),
          parent = e
        )
      }
    )
    
    metrics <- tryCatch(
      svySE_est_dom(
        design = design_dom,
        indicator = indicator,
        domain_var = ".__svySE_dom__",
        cfg = cfg,
        data_raw = data,
        group_id = g
      ),
      error = function(e) {
        svySE_abort(
          title = "No se pudo estimar el dominio / Could not estimate domain.",
          details = paste0("Fallo la estimacion para el grupo `", g, "`."),
          vars = list(
            indicator = indicator,
            group_id = g,
            estimator = cfg$estimator,
            target = cfg$target
          ),
          hint = "Si el indicador es binario o categorico, usa `estimator = \"prop\"`. Si el dominio tiene pocos casos, revisa el tamano muestral no ponderado.",
          parent = e
        )
      }
    )
    
    out <- rbind(out, cbind(group_row, metrics))
  }
  
  total_metrics <- tryCatch(
    svySE_est_total(
      design = design,
      indicator = indicator,
      cfg = cfg,
      data_raw = data
    ),
    error = function(e) {
      svySE_abort(
        title = "No se pudo calcular el total nacional / Could not calculate national total.",
        details = "Fallo la estimacion total del indicador.",
        vars = list(
          indicator = indicator,
          estimator = cfg$estimator,
          target = cfg$target
        ),
        parent = e
      )
    }
  )
  
  total_group <- as.data.frame(
    as.list(rep("NACIONAL", length(group_vars))),
    stringsAsFactors = FALSE
  )
  
  names(total_group) <- group_vars
  
  out <- rbind(cbind(total_group, total_metrics), out)
  
  rownames(out) <- NULL
  
  out
}


# ==============================================================================
# Estimacion por dominio
# Domain estimation
# ==============================================================================

#' @keywords internal
svySE_est_dom <- function(
    design,
    indicator,
    domain_var,
    cfg,
    data_raw,
    group_id
) {
  
  if (cfg$estimator == "prop") {
    
    total_obj <- survey::svytotal(
      stats::as.formula(
        paste0("~ I(.__svySE_cat__ * ", domain_var, ")")
      ),
      design
    )
    
    prop_obj <- survey::svyratio(
      numerator = stats::as.formula(
        paste0("~ I(.__svySE_cat__ * ", domain_var, ")")
      ),
      denominator = stats::as.formula(
        paste0("~ ", domain_var)
      ),
      design = design,
      deff = cfg$deff
    )
    
    ci_total <- stats::confint(total_obj, level = cfg$conf_level)
    ci_prop  <- stats::confint(prop_obj, level = cfg$conf_level)
    
    return(
      svySE_metrics(
        est_abs = as.numeric(stats::coef(total_obj)[1]),
        est_pct = as.numeric(stats::coef(prop_obj)[1]) * cfg$pct_mult,
        se_abs = as.numeric(survey::SE(total_obj)[1]),
        se_pct = as.numeric(survey::SE(prop_obj)[1]) * cfg$pct_mult,
        ci_l_abs = as.numeric(ci_total[1]),
        ci_l_pct = as.numeric(ci_prop[1]) * cfg$pct_mult,
        ci_u_abs = as.numeric(ci_total[2]),
        ci_u_pct = as.numeric(ci_prop[2]) * cfg$pct_mult,
        cv = svySE_safe_cv(prop_obj, cfg$pct_mult),
        deff = svySE_safe_deff(prop_obj),
        n_unw = sum(
          data_raw$.__svySE_group_id__ == group_id &
            data_raw$.__svySE_cat__ == 1,
          na.rm = TRUE
        ),
        cfg = cfg
      )
    )
  }
  
  if (cfg$estimator == "total") {
    
    total_obj <- survey::svytotal(
      stats::as.formula(
        paste0("~ I(.__svySE_cat__ * ", domain_var, ")")
      ),
      design,
      deff = cfg$deff
    )
    
    ci_total <- stats::confint(total_obj, level = cfg$conf_level)
    
    return(
      svySE_metrics(
        est_abs = as.numeric(stats::coef(total_obj)[1]),
        est_pct = NA_real_,
        se_abs = as.numeric(survey::SE(total_obj)[1]),
        se_pct = NA_real_,
        ci_l_abs = as.numeric(ci_total[1]),
        ci_l_pct = NA_real_,
        ci_u_abs = as.numeric(ci_total[2]),
        ci_u_pct = NA_real_,
        cv = svySE_safe_cv(total_obj, cfg$pct_mult),
        deff = svySE_safe_deff(total_obj),
        n_unw = sum(
          data_raw$.__svySE_group_id__ == group_id &
            data_raw$.__svySE_cat__ == 1,
          na.rm = TRUE
        ),
        cfg = cfg
      )
    )
  }
  
  if (cfg$estimator == "mean") {
    
    mean_obj <- survey::svyratio(
      numerator = stats::as.formula(
        paste0("~ I(", indicator, " * ", domain_var, ")")
      ),
      denominator = stats::as.formula(
        paste0("~ ", domain_var)
      ),
      design = design,
      deff = cfg$deff
    )
    
    ci_mean <- stats::confint(mean_obj, level = cfg$conf_level)
    
    return(
      svySE_metrics(
        est_abs = as.numeric(stats::coef(mean_obj)[1]),
        est_pct = NA_real_,
        se_abs = as.numeric(survey::SE(mean_obj)[1]),
        se_pct = NA_real_,
        ci_l_abs = as.numeric(ci_mean[1]),
        ci_l_pct = NA_real_,
        ci_u_abs = as.numeric(ci_mean[2]),
        ci_u_pct = NA_real_,
        cv = svySE_safe_cv(mean_obj, cfg$pct_mult),
        deff = svySE_safe_deff(mean_obj),
        n_unw = sum(data_raw$.__svySE_group_id__ == group_id, na.rm = TRUE),
        cfg = cfg
      )
    )
  }
  
  if (cfg$estimator == "ratio") {
    svySE_abort(
      title = "Estimador `ratio` no disponible en `svySE_calc()` / `ratio` estimator is not available in `svySE_calc()`.",
      details = "El estimador `ratio` requiere numerador y denominador explicitos.",
      hint = paste0(
        "Para indicadores binarios usa: cfg <- svySE_cfg(estimator = \"prop\", target = 1, valid_values = c(0, 1)). ",
        "La proporcion ya se calcula internamente como una razon entre el total ponderado del valor objetivo y el total ponderado del dominio."
      )
    )
  }
  
  svySE_abort(
    title = "Estimador no reconocido / Unknown estimator.",
    vars = list(estimator = cfg$estimator)
  )
}


# ==============================================================================
# Estimacion total nacional
# National total estimation
# ==============================================================================

#' @keywords internal
svySE_est_total <- function(
    design,
    indicator,
    cfg,
    data_raw
) {
  
  if (cfg$estimator == "prop") {
    
    total_obj <- survey::svytotal(
      ~ .__svySE_cat__,
      design
    )
    
    prop_obj <- survey::svymean(
      ~ .__svySE_cat__,
      design,
      deff = cfg$deff
    )
    
    ci_total <- stats::confint(total_obj, level = cfg$conf_level)
    ci_prop  <- stats::confint(prop_obj, level = cfg$conf_level)
    
    return(
      svySE_metrics(
        est_abs = as.numeric(stats::coef(total_obj)[1]),
        est_pct = as.numeric(stats::coef(prop_obj)[1]) * cfg$pct_mult,
        se_abs = as.numeric(survey::SE(total_obj)[1]),
        se_pct = as.numeric(survey::SE(prop_obj)[1]) * cfg$pct_mult,
        ci_l_abs = as.numeric(ci_total[1]),
        ci_l_pct = as.numeric(ci_prop[1]) * cfg$pct_mult,
        ci_u_abs = as.numeric(ci_total[2]),
        ci_u_pct = as.numeric(ci_prop[2]) * cfg$pct_mult,
        cv = svySE_safe_cv(prop_obj, cfg$pct_mult),
        deff = svySE_safe_deff(prop_obj),
        n_unw = sum(data_raw$.__svySE_cat__ == 1, na.rm = TRUE),
        cfg = cfg
      )
    )
  }
  
  if (cfg$estimator == "total") {
    
    total_obj <- survey::svytotal(
      ~ .__svySE_cat__,
      design,
      deff = cfg$deff
    )
    
    ci_total <- stats::confint(total_obj, level = cfg$conf_level)
    
    return(
      svySE_metrics(
        est_abs = as.numeric(stats::coef(total_obj)[1]),
        est_pct = NA_real_,
        se_abs = as.numeric(survey::SE(total_obj)[1]),
        se_pct = NA_real_,
        ci_l_abs = as.numeric(ci_total[1]),
        ci_l_pct = NA_real_,
        ci_u_abs = as.numeric(ci_total[2]),
        ci_u_pct = NA_real_,
        cv = svySE_safe_cv(total_obj, cfg$pct_mult),
        deff = svySE_safe_deff(total_obj),
        n_unw = sum(data_raw$.__svySE_cat__ == 1, na.rm = TRUE),
        cfg = cfg
      )
    )
  }
  
  if (cfg$estimator == "mean") {
    
    mean_obj <- survey::svymean(
      stats::as.formula(paste0("~", indicator)),
      design,
      deff = cfg$deff
    )
    
    ci_mean <- stats::confint(mean_obj, level = cfg$conf_level)
    
    return(
      svySE_metrics(
        est_abs = as.numeric(stats::coef(mean_obj)[1]),
        est_pct = NA_real_,
        se_abs = as.numeric(survey::SE(mean_obj)[1]),
        se_pct = NA_real_,
        ci_l_abs = as.numeric(ci_mean[1]),
        ci_l_pct = NA_real_,
        ci_u_abs = as.numeric(ci_mean[2]),
        ci_u_pct = NA_real_,
        cv = svySE_safe_cv(mean_obj, cfg$pct_mult),
        deff = svySE_safe_deff(mean_obj),
        n_unw = nrow(data_raw),
        cfg = cfg
      )
    )
  }
  
  svySE_abort(
    title = "Estimador no reconocido / Unknown estimator.",
    vars = list(estimator = cfg$estimator)
  )
}


# ==============================================================================
# Tabla simple
# Simple table
# ==============================================================================

#' @keywords internal
svySE_tab_one <- function(
    data,
    group_vars,
    indicator,
    groups_master,
    cfg
) {
  
  metric_names <- svySE_cols_tab_all()
  
  out <- data.frame()
  
  for (g in groups_master) {
    
    group_data <- data[data$.__svySE_group_id__ == g, , drop = FALSE]
    
    if (nrow(group_data) == 0) {
      
      row_na <- svySE_na_row(
        group_id = g,
        group_vars = group_vars,
        metric_names = metric_names
      )
      
      out <- rbind(out, row_na)
      next
    }
    
    group_row <- group_data[1, group_vars, drop = FALSE]
    
    m <- data.frame(
      freq_0 = sum(group_data[[indicator]] == 0, na.rm = TRUE),
      pct_0 = mean(group_data[[indicator]] == 0, na.rm = TRUE) * cfg$pct_mult,
      freq_1 = sum(group_data[[indicator]] == cfg$target, na.rm = TRUE),
      pct_1 = mean(group_data[[indicator]] == cfg$target, na.rm = TRUE) * cfg$pct_mult,
      freq_total = nrow(group_data),
      pct_total = cfg$pct_mult
    )
    
    out <- rbind(out, cbind(group_row, m))
  }
  
  total_group <- as.data.frame(
    as.list(rep("NACIONAL", length(group_vars))),
    stringsAsFactors = FALSE
  )
  
  names(total_group) <- group_vars
  
  total_m <- data.frame(
    freq_0 = sum(data[[indicator]] == 0, na.rm = TRUE),
    pct_0 = mean(data[[indicator]] == 0, na.rm = TRUE) * cfg$pct_mult,
    freq_1 = sum(data[[indicator]] == cfg$target, na.rm = TRUE),
    pct_1 = mean(data[[indicator]] == cfg$target, na.rm = TRUE) * cfg$pct_mult,
    freq_total = nrow(data),
    pct_total = cfg$pct_mult
  )
  
  out <- rbind(cbind(total_group, total_m), out)
  
  rownames(out) <- NULL
  
  out
}


# ==============================================================================
# Preparacion de datos
# Data preparation
# ==============================================================================

#' @keywords internal
svySE_prep_ind <- function(
    data,
    indicator,
    group_vars,
    weight,
    cfg
) {
  
  data <- data[
    data[[indicator]] %in% cfg$valid_values &
      !is.na(data[[weight]]),
    ,
    drop = FALSE
  ]
  
  if (nrow(data) == 0) {
    svySE_abort(
      title = "Indicador sin registros validos / Indicator without valid records.",
      details = paste0(
        "El indicador `", indicator,
        "` no tiene registros despues de filtrar `valid_values` y peso no perdido."
      ),
      vars = list(
        indicator = indicator,
        valid_values = cfg$valid_values,
        weight = weight
      ),
      hint = "Revisa si el indicador esta codificado como 0/1, TRUE/FALSE, texto, o si el peso tiene valores perdidos."
    )
  }
  
  data$.__svySE_cat__ <- ifelse(data[[indicator]] == cfg$target, 1, 0)
  
  data$.__svySE_group_id__ <- do.call(
    paste,
    c(data[group_vars], sep = " | ")
  )
  
  data
}


#' @keywords internal
svySE_filters <- function(
    data,
    division = NULL,
    weight,
    div_weight = NULL
) {
  
  if (is.null(division)) {
    return(
      list(
        TOTAL = list(
          name = "TOTAL",
          data = data,
          weight = weight
        )
      )
    )
  }
  
  cats <- sort(unique(data[[division]]))
  cats <- cats[!is.na(cats)]
  
  if (length(cats) == 0) {
    svySE_abort(
      title = "Variable de division sin categorias validas / Division variable without valid categories.",
      details = paste0("La variable `", division, "` no tiene categorias no perdidas."),
      vars = list(division = division)
    )
  }
  
  filters <- list(
    TOTAL = list(
      name = "TOTAL",
      data = data,
      weight = weight
    )
  )
  
  for (cat in cats) {
    
    data_cat <- data[data[[division]] == cat, , drop = FALSE]
    
    this_weight <- if (!is.null(div_weight)) div_weight else weight
    
    if (!is.null(div_weight)) {
      data_cat <- data_cat[!is.na(data_cat[[div_weight]]), , drop = FALSE]
    }
    
    filters[[as.character(cat)]] <- list(
      name = as.character(cat),
      data = data_cat,
      weight = this_weight
    )
  }
  
  filters
}


#' @keywords internal
svySE_design <- function(
    data,
    strata,
    weight
) {
  
  data <- data[!is.na(data[[weight]]), , drop = FALSE]
  
  if (nrow(data) == 0) {
    svySE_abort(
      title = "No hay registros con peso valido / No records with valid weight.",
      vars = list(weight = weight)
    )
  }
  
  svySE_chk_weight_values(
    data = data,
    weight = weight
  )
  
  survey::svydesign(
    ids = ~1,
    strata = stats::reformulate(strata),
    weights = stats::reformulate(weight),
    data = data
  )
}


# ==============================================================================
# Metricas
# Metrics
# ==============================================================================

#' @keywords internal
svySE_metrics <- function(
    est_abs,
    est_pct,
    se_abs,
    se_pct,
    ci_l_abs,
    ci_l_pct,
    ci_u_abs,
    ci_u_pct,
    cv,
    deff,
    n_unw,
    cfg
) {
  
  ci_l_abs <- as.numeric(ci_l_abs)
  ci_l_pct <- as.numeric(ci_l_pct)
  
  if (cfg$truncate_lower_ci) {
    
    if (is.finite(ci_l_abs)) {
      ci_l_abs <- max(ci_l_abs, 0)
    }
    
    if (is.finite(ci_l_pct)) {
      ci_l_pct <- max(ci_l_pct, 0)
    }
  }
  
  if (!cfg$cv) {
    cv <- NA_real_
  }
  
  if (!cfg$deff) {
    deff <- NA_real_
  }
  
  data.frame(
    est_abs = as.numeric(est_abs),
    est_pct = as.numeric(est_pct),
    se_abs = as.numeric(se_abs),
    se_pct = as.numeric(se_pct),
    ci_l_abs = as.numeric(ci_l_abs),
    ci_l_pct = as.numeric(ci_l_pct),
    ci_u_abs = as.numeric(ci_u_abs),
    ci_u_pct = as.numeric(ci_u_pct),
    cv = as.numeric(cv),
    deff = as.numeric(deff),
    n_unw = as.numeric(n_unw)
  )
}


#' @keywords internal
svySE_safe_cv <- function(
    obj,
    pct_mult = 100
) {
  
  out <- tryCatch(
    as.numeric(survey::cv(obj)[1]) * pct_mult,
    error = function(e) NA_real_
  )
  
  if (!is.finite(out)) {
    out <- NA_real_
  }
  
  out
}


#' @keywords internal
svySE_safe_deff <- function(obj) {
  
  out <- tryCatch(
    as.numeric(survey::deff(obj)),
    error = function(e) NA_real_
  )
  
  if (!is.finite(out)) {
    out <- NA_real_
  }
  
  out
}


# ==============================================================================
# Validaciones de usuario
# User validations
# ==============================================================================

#' @keywords internal
svySE_chk_pkg <- function(pkg) {
  
  if (!requireNamespace(pkg, quietly = TRUE)) {
    svySE_abort(
      title = "Paquete requerido no instalado / Required package not installed.",
      details = paste0("El paquete `", pkg, "` es requerido para ejecutar esta funcion."),
      hint = paste0("Instala el paquete con: install.packages(\"", pkg, "\")")
    )
  }
  
  invisible(TRUE)
}


#' @keywords internal
svySE_chk_df <- function(data) {
  
  if (!is.data.frame(data)) {
    svySE_abort(
      title = "`data` no es un data.frame / `data` is not a data.frame.",
      details = paste0("Clase recibida / received class: ", paste(class(data), collapse = ", ")),
      hint = "Carga o convierte tu base antes de usar svySE: data <- as.data.frame(data)."
    )
  }
  
  if (nrow(data) == 0) {
    svySE_abort(
      title = "`data` esta vacio / `data` is empty.",
      details = "La base no contiene filas."
    )
  }
  
  invisible(TRUE)
}


#' @keywords internal
svySE_chk_chr <- function(
    x,
    arg
) {
  
  if (is.null(x) || length(x) == 0) {
    svySE_abort(
      title = "Argumento vacio / Empty argument.",
      details = paste0("`", arg, "` debe contener al menos un valor."),
      vars = list(argument = arg)
    )
  }
  
  if (!is.character(x)) {
    svySE_abort(
      title = "Tipo de argumento invalido / Invalid argument type.",
      details = paste0("`", arg, "` debe ser un vector de texto / must be a character vector."),
      vars = list(
        argument = arg,
        received_class = class(x)
      )
    )
  }
  
  if (any(is.na(x)) || any(trimws(x) == "")) {
    svySE_abort(
      title = "Nombres de variables invalidos / Invalid variable names.",
      details = paste0("`", arg, "` contiene NA o textos vacios."),
      vars = list(argument = arg, values = x)
    )
  }
  
  invisible(TRUE)
}


#' @keywords internal
svySE_chk_vars <- function(
    data,
    vars,
    arg
) {
  
  missing_vars <- setdiff(vars, names(data))
  
  if (length(missing_vars) > 0) {
    svySE_abort(
      title = "Variables no encontradas / Variables not found.",
      details = paste0("Algunas variables indicadas en `", arg, "` no existen en `data`."),
      vars = list(
        argument = arg,
        missing_vars = missing_vars,
        available_example = head(names(data), 30)
      ),
      hint = "Usa `names(data)` para revisar los nombres exactos. Recuerda que R distingue mayusculas y minusculas."
    )
  }
  
  invisible(TRUE)
}


#' @keywords internal
svySE_chk_estimator <- function(
    cfg,
    indicators
) {
  
  if (cfg$estimator == "ratio") {
    svySE_abort(
      title = "Configuracion no compatible / Incompatible configuration.",
      details = paste0(
        "Has usado `estimator = \"ratio\"` junto con `indicators`. ",
        "Esta funcion esta disenada para indicadores directos, especialmente binarios o categoricos."
      ),
      vars = list(
        estimator = cfg$estimator,
        indicators = indicators,
        target = cfg$target,
        valid_values = cfg$valid_values
      ),
      hint = paste0(
        "Para indicadores binarios como Indicador_1, Indicador_2, etc., usa: ",
        "cfg <- svySE_cfg(estimator = \"prop\", target = 1, valid_values = c(0, 1)). ",
        "Tecnicamente, `prop` ya calcula internamente una razon: total ponderado del valor objetivo dividido entre total ponderado del dominio."
      )
    )
  }
  
  invisible(TRUE)
}


#' @keywords internal
svySE_chk_indicator_values <- function(
    data,
    indicator,
    cfg,
    strict = FALSE
) {
  
  x <- data[[indicator]]
  
  n_total <- length(x)
  n_na <- sum(is.na(x))
  n_valid <- sum(x %in% cfg$valid_values, na.rm = TRUE)
  n_target <- sum(x == cfg$target, na.rm = TRUE)
  
  invalid_values <- setdiff(unique(x[!is.na(x)]), cfg$valid_values)
  
  if (n_valid == 0) {
    svySE_abort(
      title = "Indicador sin valores validos / Indicator without valid values.",
      details = paste0(
        "El indicador `", indicator,
        "` no contiene ningun valor dentro de `valid_values`."
      ),
      vars = list(
        indicator = indicator,
        n_total = n_total,
        n_na = n_na,
        valid_values = cfg$valid_values,
        observed_values = svySE_observed_values(x)
      ),
      hint = "Revisa si el indicador esta codificado como 1/2, TRUE/FALSE, texto, o si necesitas cambiar `valid_values`."
    )
  }
  
  if (n_target == 0 && cfg$estimator %in% c("prop", "total")) {
    warning(
      paste0(
        "El indicador `", indicator, "` no tiene casos con target = ",
        cfg$target, ". Las estimaciones pueden ser cero o no estimables."
      ),
      call. = FALSE
    )
  }
  
  if (length(invalid_values) > 0) {
    
    msg <- paste0(
      "El indicador `", indicator,
      "` contiene valores fuera de `valid_values`: ",
      paste(invalid_values, collapse = ", "),
      ". Estos registros seran excluidos del calculo."
    )
    
    if (isTRUE(strict)) {
      svySE_abort(
        title = "Indicador con valores no permitidos / Indicator with invalid values.",
        details = msg,
        vars = list(
          indicator = indicator,
          valid_values = cfg$valid_values,
          invalid_values = invalid_values,
          observed_values = svySE_observed_values(x)
        ),
        hint = "Corrige la codificacion del indicador o usa `strict = FALSE` para excluir esos registros."
      )
    } else {
      warning(msg, call. = FALSE)
    }
  }
  
  invisible(TRUE)
}


#' @keywords internal
svySE_prepare_weight <- function(
    data,
    weight,
    arg = "weight"
) {
  
  original <- data[[weight]]
  
  converted <- suppressWarnings(as.numeric(original))
  
  new_na <- sum(is.na(converted) & !is.na(original))
  
  if (new_na > 0) {
    warning(
      paste0(
        "La variable `", weight, "` indicada en `", arg,
        "` tiene ", new_na,
        " valores no numericos que fueron convertidos a NA."
      ),
      call. = FALSE
    )
  }
  
  data[[weight]] <- converted
  
  svySE_chk_weight_values(data, weight)
  
  data
}


#' @keywords internal
svySE_chk_weight_values <- function(
    data,
    weight
) {
  
  w <- data[[weight]]
  
  if (all(is.na(w))) {
    svySE_abort(
      title = "Peso completamente perdido / Weight is completely missing.",
      details = paste0("La variable de peso `", weight, "` solo contiene NA."),
      vars = list(weight = weight),
      hint = "Revisa si el nombre del peso es correcto o si fue leido como texto no convertible a numero."
    )
  }
  
  if (any(w < 0, na.rm = TRUE)) {
    svySE_abort(
      title = "Peso con valores negativos / Weight has negative values.",
      details = paste0("La variable `", weight, "` contiene pesos negativos."),
      vars = list(
        weight = weight,
        min_weight = min(w, na.rm = TRUE)
      ),
      hint = "Los pesos muestrales deben ser no negativos. Revisa la variable de factor de expansion."
    )
  }
  
  if (all(w == 0 | is.na(w))) {
    svySE_abort(
      title = "Peso sin valores positivos / Weight has no positive values.",
      details = paste0("La variable `", weight, "` no tiene valores positivos."),
      vars = list(weight = weight),
      hint = "Verifica que estes usando el factor de expansion correcto."
    )
  }
  
  if (any(w == 0, na.rm = TRUE)) {
    warning(
      paste0(
        "La variable de peso `", weight,
        "` contiene valores cero. Estos registros no aportaran al estimador ponderado."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


#' @keywords internal
svySE_chk_required_no_na <- function(
    data,
    vars,
    context
) {
  
  for (v in vars) {
    
    n_na <- sum(is.na(data[[v]]))
    
    if (n_na > 0) {
      svySE_abort(
        title = "Variable requerida con valores perdidos / Required variable has missing values.",
        details = paste0(
          "La variable `", v, "` tiene ", n_na,
          " valores perdidos dentro de ", context, "."
        ),
        vars = list(
          variable = v,
          context = context,
          n_missing = n_na
        ),
        hint = "Depura la base antes del calculo o filtra registros con estrato/grupo perdido."
      )
    }
  }
  
  invisible(TRUE)
}


#' @keywords internal
svySE_warn_na_division <- function(
    data,
    division
) {
  
  n_na <- sum(is.na(data[[division]]))
  
  if (n_na > 0) {
    warning(
      paste0(
        "La variable de division `", division,
        "` tiene ", n_na,
        " valores perdidos. Esos registros se mantienen en TOTAL, ",
        "pero no forman una categoria de division."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# ==============================================================================
# Filas auxiliares
# Auxiliary rows
# ==============================================================================

#' @keywords internal
svySE_na_row <- function(
    group_id,
    group_vars,
    metric_names
) {
  
  group_values <- strsplit(group_id, " \\| ")[[1]]
  
  if (length(group_values) < length(group_vars)) {
    group_values <- c(
      group_values,
      rep("", length(group_vars) - length(group_values))
    )
  }
  
  group_values <- group_values[seq_along(group_vars)]
  
  group_df <- as.data.frame(
    as.list(group_values),
    stringsAsFactors = FALSE
  )
  
  names(group_df) <- group_vars
  
  metric_df <- as.data.frame(
    as.list(rep(NA_real_, length(metric_names))),
    stringsAsFactors = FALSE
  )
  
  names(metric_df) <- metric_names
  
  cbind(group_df, metric_df)
}


#' @keywords internal
svySE_empty_by_groups <- function(
    groups_master,
    group_vars,
    metric_names
) {
  
  out <- data.frame()
  
  for (g in groups_master) {
    out <- rbind(
      out,
      svySE_na_row(
        group_id = g,
        group_vars = group_vars,
        metric_names = metric_names
      )
    )
  }
  
  rownames(out) <- NULL
  
  out
}


# ==============================================================================
# Mensajes de error y diagnostico
# Error messages and diagnostics
# ==============================================================================

#' @keywords internal
svySE_abort <- function(
    title,
    details = NULL,
    vars = NULL,
    hint = NULL,
    parent = NULL
) {
  
  msg <- c(
    "",
    title
  )
  
  if (!is.null(details)) {
    msg <- c(
      msg,
      "",
      "Detalle / Detail:",
      details
    )
  }
  
  if (!is.null(vars)) {
    msg <- c(
      msg,
      "",
      "Variables involucradas / Involved variables:",
      svySE_format_vars(vars)
    )
  }
  
  if (!is.null(hint)) {
    msg <- c(
      msg,
      "",
      "Sugerencia / Hint:",
      hint
    )
  }
  
  if (!is.null(parent)) {
    msg <- c(
      msg,
      "",
      "Error tecnico original / Original technical error:",
      conditionMessage(parent)
    )
  }
  
  stop(
    structure(
      list(
        message = paste(msg, collapse = "\n"),
        call = NULL
      ),
      class = c("svySE_error", "error", "condition")
    )
  )
}


#' @keywords internal
svySE_format_vars <- function(vars) {
  
  out <- character(0)
  
  nms <- names(vars)
  
  if (is.null(nms)) {
    nms <- paste0("var_", seq_along(vars))
  }
  
  for (i in seq_along(vars)) {
    
    value <- vars[[i]]
    
    if (length(value) > 20) {
      value <- c(value[1:20], "...")
    }
    
    value <- paste(value, collapse = ", ")
    
    out <- c(
      out,
      paste0("- ", nms[i], ": ", value)
    )
  }
  
  out
}


#' @keywords internal
svySE_observed_values <- function(x) {
  
  vals <- unique(x[!is.na(x)])
  
  if (length(vals) > 20) {
    vals <- c(vals[1:20], "...")
  }
  
  vals
}


# ==============================================================================
# Impresion del resultado
# Print result
# ==============================================================================

#' Print svySE result
#'
#' Imprime un resumen del resultado de `svySE_calc()`.
#'
#' Prints a summary of the result from `svySE_calc()`.
#'
#' @param x Objeto de clase `"svySE_result"`.
#' @param ... Argumentos adicionales.
#'
#' @return Invisiblemente `x`.
#'
#' @export
print.svySE_result <- function(x, ...) {
  
  cat("svySE result\n")
  cat("--------------------------------------------------\n")
  cat("Indicators :", paste(x$meta$indicators, collapse = ", "), "\n")
  cat("Groups     :", paste(x$meta$group_vars, collapse = ", "), "\n")
  cat("Strata     :", paste(x$meta$strata, collapse = ", "), "\n")
  cat("Weight     :", x$meta$weight, "\n")
  cat("Division   :", ifelse(is.null(x$meta$division), "NULL", x$meta$division), "\n")
  cat("Estimator  :", x$meta$cfg$estimator, "\n")
  cat("Target     :", x$meta$cfg$target, "\n")
  cat("Strict     :", x$meta$strict, "\n")
  
  invisible(x)
}
