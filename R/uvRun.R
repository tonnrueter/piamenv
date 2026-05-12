#' uvRun
#'
#' Run a command in a specified uv-managed Python environment with given environment variables.
#'
#' The environment's \code{bin/} directory is prepended to PATH so that \code{python} in
#' \code{cmd} resolves to the environment's interpreter without requiring uv at runtime.
#' \code{VIRTUAL_ENV} is also set so Python's \code{sys.prefix} and package resolution
#' behave as if the environment were activated.
#'
#' @param cmd Command to run in the Python environment
#' @param path Path to the Python environment
#' @param env Named vector of environment variables to set, i.e., c("VAR0" = "value0", "VAR1" = "value1")
#' @param init List containing \code{setup} and \code{teardown} commands. Unused for uv
#'   environments; kept for interface compatibility with \code{condaRun}.
#' @param log Path to log file or NULL to use stdout
#' @param verbose Verbosity level (0 for silent, higher for more verbose).
#' @return Invisible NULL
#' @export
uvRun <- function(cmd, path, env = c(), init = list(setup = "", teardown = ""), log = NULL, verbose = 0) {
  log <- if (!is.null(log)) normalizePath(log, mustWork = FALSE) else stdout()
  path <- normalizePath(path, mustWork = TRUE)
  # &>> (bash) appends both stdout and stderr to the log, fully absorbing climate-assessment
  # output without leaking it to the parent process (i.e. the Slurm job's log.txt).
  # *>> is the PowerShell equivalent for Windows.
  redirectAndApppend <- if (.Platform$OS.type == "unix") "&>>" else "*>>"
  # Build the run command, optionally redirecting stdout+stderr to the log file
  runCmd <- paste(
    Filter(nzchar, c(
      cmd,
      if (log != stdout()) paste(redirectAndApppend, log) else ""
    )),
    collapse = " "
  )
  # Prepend the venv's bin directory to PATH so 'python' in cmd resolves to the venv's interpreter.
  # uv does not need to be present on the compute node at runtime.
  binDir <- file.path(path, if (.Platform$OS.type == "unix") "bin" else "Scripts")
  newPath <- paste(binDir, Sys.getenv("PATH"), sep = .Platform$path.sep)
  if (length(env) > 0 && verbose > 0) {
    cat(
      date(), " uvRun -- Environment variables:\n",
      paste0("   ", names(env), "=", env, " (sys: '", Sys.getenv(names(env)), "')", collapse = "\n"), "\n",
      file = log, append = TRUE, sep = ""
    )
  }
  if (verbose > 0) {
    cat(paste0(date(), " uvRun -- About to run\n", runCmd, "\n"), file = log, append = TRUE)
  }
  # Caller-supplied env vars are set first; VIRTUAL_ENV and PATH are appended so they
  # cannot be accidentally overridden by the caller.
  withr::with_envvar(
    c(env, "VIRTUAL_ENV" = path, "PATH" = newPath),
    {
      system(runCmd)
    },
    action = "replace"
  )
  invisible(NULL)
}
