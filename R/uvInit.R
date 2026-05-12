#' uvInit
#'
#' Verify that a uv-managed Python environment exists at the given path.
#'
#' Unlike \code{condaInit}, no shell setup commands are needed to activate a uv-managed
#' environment at runtime: the environment's \code{bin/} directory is prepended to PATH
#' by \code{uvRun}. This function only validates that the environment exists and
#' returns an empty \code{setup}/\code{teardown} list for interface compatibility with
#' \code{condaRun}.
#'
#' @param path Path to the Python environment
#' @param log Path to log file or NULL to use stdout
#' @param verbose Verbosity level (0 for silent, higher for more verbose)
#' @return List containing empty \code{setup} and \code{teardown} commands.
#' @export
uvInit <- function(path, log = NULL, verbose = 0) {
  log <- if (!is.null(log)) normalizePath(log, mustWork = FALSE) else stdout()
  pythonBin <- file.path(
    normalizePath(path, mustWork = FALSE),
    if (.Platform$OS.type == "unix") "bin/python" else "Scripts/python.exe"
  )
  if (!file.exists(pythonBin)) {
    stop(paste0(
      "Python environment not found at '", path, "'.\n\n",
      "To use an existing shared environment, update cfg$pythonPath to point to it.\n\n",
      "To create a new environment at this path:\n",
      "  # Install uv once per user:\n",
      "  curl -LsSf https://astral.sh/uv/install.sh | sh\n",
      "  # Create and populate the environment:\n",
      "  uv venv ", path, " --python 3.11\n",
      "  uv pip install -r config/py_requirements.txt"
    ))
  }
  if (verbose > 0) {
    cat(date(), " uvInit -- Python found at '", pythonBin, "'\n", file = log, append = TRUE, sep = "")
  }
  list(setup = "", teardown = "")
}
