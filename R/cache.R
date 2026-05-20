new_cache <- function() {
  cache_path <- function(hash) {
    dir <- dirname(hash)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    hash
  }

  cache_save <- function(objects, output, hash, lazy = TRUE) {
    path <- cache_path(hash)
    saveRDS(objects, paste0(path, "_objects.rds"))
    saveRDS(output, paste0(path, "_output.rds"))
  }

  cache_load <- function(hash, lazy = TRUE) {
    path <- cache_path(hash)
    objs_path <- paste0(path, "_objects.rds")
    out_path <- paste0(path, "_output.rds")
    if (!file.exists(objs_path)) {
      return(NULL)
    }
    list(
      objects = readRDS(objs_path),
      output = readRDS(out_path)
    )
  }

  cache_exists <- function(hash, lazy = TRUE) {
    path <- cache_path(hash)
    file.exists(paste0(path, "_objects.rds")) &&
      file.exists(paste0(path, "_output.rds"))
  }

  cache_purge <- function(hash) {
    for (h in hash) {
      path <- cache_path(h)
      unlink(paste0(path, "_objects.rds"))
      unlink(paste0(path, "_output.rds"))
    }
  }

  cache_output <- function(hash, mode = "character") {
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

cache_name <- function(params) {
  paste0(valid_path(params$cache.path, params$label), "_", digest(cache_content(params)))
}

block_cache_save <- function(options, output, objects) {
  hash <- options$hash
  purge_cache(options)
  cache$save(objects, output, hash)
}

block_cache_load <- function(options) {
  hash <- options$hash
  data <- cache$load(hash)
  if (is.null(data)) {
    return(NULL)
  }
  data$output
}

purge_cache <- function(options) {
  deps <- dep_list$get(options$label)
  hashes <- paste0(valid_path(options$cache.path, c(options$label, deps)), "_*")
  for (h in hashes) {
    files <- Sys.glob(paste0(h, "_objects.rds"))
    files <- c(files, Sys.glob(paste0(h, "_output.rds")))
    unlink(files)
  }
}

cache_output_name <- function(hash) {
  paste0(".", gsub("/", "_", hash))
}

cache_meta_name <- function(hash) {
  paste0(".", gsub("/", "_", hash), "_meta")
}

knit_global <- function() {
  if (exists(".knitGlobal", envir = .knitEnv)) {
    get(".knitGlobal", envir = .knitEnv)
  } else {
    globalenv()
  }
}
