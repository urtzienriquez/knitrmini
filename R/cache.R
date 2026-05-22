new_cache <- function() {
  cache_path <- function(hash) {
    dir <- dirname(hash)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    hash
  }

  cache_save <- function(objects, output, hash, level = 1,
                          obj_list = NULL, global_list = NULL,
                          packages = NULL, plot_digests = NULL) {
    path <- cache_path(hash)

    saveRDS(output, paste0(path, "_output.rds"))

    if (level >= 1) {
      obj_dir <- paste0(path, "_objects")
      unlink(obj_dir, recursive = TRUE)
      dir.create(obj_dir, showWarnings = FALSE)
      env <- knit_global()
      obj_names <- if (is.list(objects) || is.environment(objects)) {
        names(objects)
      } else {
        as.character(objects)
      }
      for (name in obj_names) {
        val <- if (is.list(objects) || is.environment(objects)) {
          objects[[name]]
        } else if (exists(name, envir = env, inherits = FALSE)) {
          get(name, envir = env, inherits = FALSE)
        } else {
          NULL
        }
        if (!is.null(val)) {
          saveRDS(val, file.path(obj_dir, paste0(name, ".rds")))
        }
      }
    }

    meta <- list(
      level = level,
      objects = obj_list %n% objects,
      globals = global_list,
      packages = packages,
      plot_digests = plot_digests
    )
    saveRDS(meta, paste0(path, "_meta.rds"))
  }

  cache_load <- function(hash) {
    path <- cache_path(hash)
    out_path <- paste0(path, "_output.rds")
    meta_path <- paste0(path, "_meta.rds")

    if (!file.exists(out_path)) {
      return(NULL)
    }

    if (file.exists(meta_path)) {
      meta <- readRDS(meta_path)
    } else {
      meta <- list(level = 1, objects = NULL, globals = NULL, packages = NULL, plot_digests = NULL)
    }

    if (!is.null(meta$packages)) {
      for (pkg in meta$packages) {
        suppressPackageStartupMessages(
          require(pkg, character.only = TRUE, quietly = TRUE)
        )
      }
    }

    obj_dir <- paste0(path, "_objects")
    if (dir.exists(obj_dir)) {
      env <- knit_global()
      obj_files <- list.files(obj_dir, pattern = "\\.rds$")
      for (f in obj_files) {
        name <- sub("\\.rds$", "", f)
        load_path <- file.path(obj_dir, f)
        delayedAssign(name, readRDS(load_path), assign.env = env)
      }
      objs_path <- paste0(path, "_objects.rds")
      if (file.exists(objs_path)) {
        obj_data <- readRDS(objs_path)
        if (is.list(obj_data)) {
          for (nme in names(obj_data)) {
            assign(nme, obj_data[[nme]], envir = env)
          }
        }
      }
    }

    list(
      output = readRDS(out_path),
      level = meta$level,
      objects = meta$objects,
      globals = meta$globals,
      plot_digests = meta$plot_digests,
      packages = meta$packages
    )
  }

  cache_exists <- function(hash) {
    path <- cache_path(hash)
    file.exists(paste0(path, "_output.rds"))
  }

  cache_purge <- function(hash) {
    for (h in hash) {
      path <- cache_path(h)
      unlink(paste0(path, "_output.rds"))
      unlink(paste0(path, "_meta.rds"))
      unlink(paste0(path, "_objects"), recursive = TRUE)
      unlink(paste0(path, "_objects.rds"))
    }
  }

  cache_output <- function(hash) {
    path <- cache_path(hash)
    out_path <- paste0(path, "_output.rds")
    if (file.exists(out_path)) readRDS(out_path) else NULL
  }

  list(
    save = cache_save, load = cache_load,
    exists = cache_exists, purge = cache_purge,
    output = cache_output
  )
}

cache <- new_cache()

#' Compute a digest hash
#'
#' Compute an MD5 (or SHA1 if \pkg{digest} is not available) hash of an R object.
#' Used to generate cache keys.
#'
#' @param x Any R object to hash.
#' @return A character string of the hash.
#' @keywords internal
digest <- function(x) {
  if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(x)
  } else {
    serial <- serialize(x, connection = NULL)
    tools::md5sum(serial)
  }
}

cache_content <- function(params) {
  content <- list(
    code = params[["code"]],
    eval = params[["eval"]],
    warning = params[["warning"]],
    message = params[["message"]],
    error = params[["error"]],
    cache = params[["cache"]],
    fig.keep = params[["fig.keep"]],
    fig.path = params[["fig.path"]],
    fig.ext = params[["fig.ext"]],
    dev = params[["dev"]],
    dpi = params[["dpi"]],
    fig.width = params[["fig.width"]],
    fig.height = params[["fig.height"]]
  )
  if (isFALSE(params$cache.comments) && params$engine == "R") {
    content$code <- parse_only(content$code)
  }
  content
}

#' Generate a cache name
#'
#' Generate a unique cache identifier from chunk parameters (label + hash of
#' code and options).
#'
#' @param params A list of chunk options (must include \code{label}, \code{cache.path}, and \code{code}).
#' @return A cache filename string.
#' @keywords internal
cache_name <- function(params) {
  paste0(valid_path(params$cache.path, params$label), "_", digest(cache_content(params)))
}

block_cache_save <- function(options, output, objects) {
  hash <- options$hash
  purge_cache(options)
  level <- options[["cache"]] %n% 1L
  if (isTRUE(level)) level <- 1L
  if (is.character(level)) level <- as.integer(level)
  level <- as.integer(level)
  if (is.na(level) || level < 1L) level <- 1L

  obj_list <- options$.objects
  global_list <- options$.globals

  env <- knit_global()
  pkgs <- loadedNamespaces()
  options$.packages <- pkgs

  plot_digests <- NULL
  if (level >= 2L) {
    fig_paths <- Sys.glob(paste0(
      valid_path(options$fig.path, options$label), "*", options$fig.ext
    ))
    if (length(fig_paths)) {
      plot_digests <- vapply(fig_paths, function(f) {
        if (file.exists(f)) tools::md5sum(f) else ""
      }, character(1), USE.NAMES = FALSE)
    }
  }

  cache$save(objects, output, hash,
    level = level,
    obj_list = obj_list, global_list = global_list,
    packages = pkgs, plot_digests = plot_digests
  )
}

block_cache_load <- function(options) {
  hash <- options$hash
  data <- cache$load(hash)
  if (is.null(data)) return(NULL)

  if (!is.null(data$plot_digests) && length(data$plot_digests)) {
    fig_paths <- Sys.glob(paste0(
      valid_path(options$fig.path, options$label), "*", options$fig.ext
    ))
    if (length(fig_paths) != length(data$plot_digests)) return(NULL)
    for (i in seq_along(fig_paths)) {
      if (!file.exists(fig_paths[i]) ||
          tools::md5sum(fig_paths[i]) != data$plot_digests[i]) return(NULL)
    }
  }

  level <- options[["cache"]] %n% 1L
  if (isTRUE(level)) level <- 1L
  if (is.character(level)) level <- as.integer(level)
  level <- as.integer(level)
  if (is.na(level) || level < 1L) level <- 1L
  options$cache.level <- level

  data$output
}

purge_cache <- function(options) {
  deps <- dep_list$get(options$label)
  hashes <- paste0(valid_path(options$cache.path, c(options$label, deps)), "_*")
  for (h in hashes) {
    files <- Sys.glob(paste0(h, "_output.rds"))
    files <- c(files, Sys.glob(paste0(h, "_meta.rds")))
    files <- c(files, Sys.glob(paste0(h, "_objects.rds")))
    unlink(files)
  }
  for (h in hashes) {
    obj_dir <- paste0(h, "_objects")
    if (dir.exists(obj_dir)) unlink(obj_dir, recursive = TRUE)
  }
}

#' Find symbols in an expression
#'
#' Recursively extract all symbol names from a parsed R expression.
#'
#' @param expr An R expression (character, expression, call, or symbol).
#' @return Character vector of symbol names.
#' @keywords internal
find_symbols <- function(expr) {
  if (!is.character(expr) && !is.expression(expr) && !is.call(expr) && !is.symbol(expr)) {
    return(character())
  }
  if (is.character(expr)) {
    expr <- tryCatch(parse(text = expr), error = function(e) NULL)
    if (is.null(expr)) return(character())
  }
  if (is.expression(expr)) {
    return(unique(unlist(lapply(expr, find_symbols))))
  }
  if (is.symbol(expr)) {
    s <- as.character(expr)
    if (nzchar(s) && s != "...") return(s) else return(character())
  }
  if (is.call(expr) || is.recursive(expr)) {
    children <- as.list(expr)
    unique(unlist(lapply(children, find_symbols)))
  } else {
    character()
  }
}

dep_auto <- function(options, variables, values) {
  if (is.null(options$hash) || length(options$hash) != 1) return(invisible())
  current_label <- options$label
  current_globals <- variables$globals

  if (is.null(current_globals) || length(current_globals) == 0) return(invisible())

  labels <- all_labels_internal()
  for (lab in labels) {
    if (lab == current_label) next
    chunk_meta <- options[[paste0(".chunk_meta.", lab)]]
    if (is.null(chunk_meta)) {
      chunk_objects <- NULL
      ch_path <- valid_path(options$cache.path, lab)
      meta_file <- paste0(dirname(options$hash), "/", basename(ch_path), "_meta.rds")
      if (file.exists(meta_file)) {
        meta <- tryCatch(readRDS(meta_file), error = function(e) NULL)
        if (!is.null(meta)) chunk_objects <- meta$objects
      }
    } else {
      chunk_objects <- chunk_meta$objects
    }
    if (is.null(chunk_objects)) next
    shared <- intersect(current_globals, chunk_objects)
    if (length(shared)) {
      existing <- dep_list$get(lab) %n% character()
      if (!(current_label %in% existing)) {
        dep_list$set(setNames(list(c(existing, current_label)), lab))
      }
    }
  }
}

dep_auto_init <- function(recent_chunks) {
  for (i in seq_len(nrow(recent_chunks))) {
    row <- recent_chunks[i, , drop = FALSE]
    vars <- list(
      objects = unique(unlist(strsplit(row$objects %n% "", ";"))),
      globals = unique(unlist(strsplit(row$globals %n% "", ";")))
    )
    opts <- list(
      label = row$label,
      hash = row$hash,
      cache.path = row$cache.path,
      fig.path = row$fig.path,
      fig.ext = row$fig.ext
    )
    dep_auto(opts, vars, NULL)
  }
}

all_labels_internal <- function() {
  .knitEnv$labels %n% character()
}

#' Get the knit evaluation environment
#'
#' Returns the environment in which code chunks are evaluated. Set by [knit()]
#' via `knit_global_set()`.
#'
#' @return An environment object.
#' @keywords internal
knit_global <- function() {
  if (exists(".knitGlobal", envir = .knitEnv)) {
    get(".knitGlobal", envir = .knitEnv)
  } else {
    globalenv()
  }
}
