#!/usr/bin/env Rscript
# connect_tagbio_worker.R
#
# Warm plugin worker for the FC engine: one long-lived R process that loads the SDK and its common libraries once,
# then runs each plugin job in a fresh forked child (connect_tagbio.R, unchanged), so a run skips R startup and
# library loading but stays isolated.
#
# The engine starts it with `Rscript connect_tagbio_worker.R --engine_pid <pid>`. The worker listens on a localhost
# port and prints `TAGBIO_WORKER_READY <port> <token>`. Per job the engine connects, sends one JSON line
# {"token", "args", "cwd", "stdout", "stderr"} (args = the usual connect_tagbio.R arguments) and reads
# `STARTED <pid>` then `EXIT <code>`; a connection that closes without EXIT means the child died. The worker exits
# when the engine process is gone. Mirrors tagbiopy/worker.py.

# Attached: exactly what connect_tagbio.R attaches, so plugins see the same search path as a cold run. Loaded only
# (not attached): what Rmd renders use, so a render skips loading it.
suppressPackageStartupMessages({
  library("optparse")
  library("rjson")
  library("tidyverse")
  library("tagbio")
  library("yaml")
  for (name in c("rmarkdown", "knitr")) requireNamespace(name, quietly = TRUE)
})

worker_args <- commandArgs(trailingOnly = TRUE)
engine_pid <- if ("--engine_pid" %in% worker_args) as.integer(worker_args[which(worker_args == "--engine_pid") + 1]) else NA
script_file <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])
runner <- file.path(dirname(normalizePath(script_file)), "connect_tagbio.R")

# random bytes from the OS, not R's generator (children must not inherit a used .Random.seed)
random_bytes <- function(n) {
  con <- file("/dev/urandom", "rb")
  on.exit(close(con))
  readBin(con, "raw", n)
}
random_hex <- function(n) paste(as.character(random_bytes(n)), collapse = "")
random_int <- function(low, high) low + sum(as.integer(random_bytes(4)) * 256^(0:3)) %% (high - low + 1)

token <- random_hex(16)

server <- NULL
for (attempt in 1:50) {
  port <- random_int(20000, 60999)
  server <- tryCatch(serverSocket(port), error = function(e) NULL)
  if (!is.null(server)) break
}
if (is.null(server)) stop("connect_tagbio_worker.R: could not open a localhost port")

engine_alive <- function() is.na(engine_pid) || isTRUE(tools::pskill(engine_pid, 0L))

# Each child works in its own folder inside the worker's tempdir() and removes it when done; what a crashed child
# leaves behind is swept by age (a cold run's leftovers go with its process).
TEMP_MAX_AGE_HOURS <- 24
last_sweep <- Sys.time()
sweep_tempdir <- function() {
  entries <- list.files(tempdir(), full.names = TRUE, all.files = TRUE, no.. = TRUE)
  old <- entries[difftime(Sys.time(), file.mtime(entries), units = "hours") > TEMP_MAX_AGE_HOURS]
  if (length(old) > 0) unlink(old, recursive = TRUE, force = TRUE)
}

# q() / quit() in a child would run R's session cleanup, which deletes the tempdir() every child shares; in a child
# they end the job with the given status instead (the runner's q(status = 1) on a failed render, or a plugin's)
child_quit <- function(save = "default", status = 0, runLast = TRUE) {
  stop(structure(class = c("child_quit", "condition"), list(status = as.integer(status), message = "quit", call = NULL)))
}

# In the forked child: send output to the job's files, run connect_tagbio.R with the job's arguments, report the code.
# A q(status = n) (connect_tagbio.R's on a failed Rmd render, or a plugin's) reports n; a child that dies outright
# ends without EXIT, which the engine reports as a failure.
run_child <- function(con, job) {
  writeLines(sprintf("STARTED %d", Sys.getpid()), con)
  flush(con)
  out <- file(job$stdout, open = "at")
  err <- file(job$stderr, open = "at")
  sink(out, type = "output")
  sink(err, type = "message")
  # every child would otherwise continue the parent's random state: R's (if any) and the C generator behind
  # tempfile() names, which simultaneous children would otherwise draw identically
  if (exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv())
  invisible(replicate(Sys.getpid() %% 997L + 1L, tempfile()))
  # A tempdir() of its own: packages clean up by pattern in tempdir() (rmarkdown deletes every rmarkdown-str*.html
  # there after a render), which in a shared folder deletes the files of renders still running in other children.
  job_dir <- file.path(tempdir(), sprintf("job_%d", Sys.getpid()))
  dir.create(job_dir, showWarnings = FALSE, recursive = TRUE)
  Sys.setenv(TMPDIR = job_dir)
  base_overrides <- list(
    tempdir = function(check = FALSE) {
      if (check) dir.create(job_dir, showWarnings = FALSE, recursive = TRUE)
      job_dir
    },
    q = child_quit,
    quit = child_quit)
  for (name in names(base_overrides)) {
    unlockBinding(name, baseenv())
    assign(name, base_overrides[[name]], envir = baseenv())
  }
  setwd(job$cwd)
  assign("JOB_ARGS", unlist(job$args), envir = globalenv())
  code <- tryCatch({
    # in the global environment, exactly as Rscript runs it cold: Rmd chunks then evaluate there, and code that
    # looks objects up in the global environment works (survminer::ggsurvplot re-evaluates fit$call$data there).
    # The child is a forked copy, so its global environment is its own.
    source(runner, local = globalenv())
    0L
  }, child_quit = function(e) e$status, error = function(e) {
    # the same lines Rscript prints for an uncaught error in a cold run
    call <- conditionCall(e)
    message(if (is.null(call)) "Error: " else paste0("Error in ", paste(deparse(call), collapse = " "), " : "),
            conditionMessage(e))
    message("Execution halted")
    1L
  })
  sink(type = "message")
  sink(type = "output")
  close(out)
  close(err)
  unlink(job_dir, recursive = TRUE, force = TRUE)
  writeLines(sprintf("EXIT %d", code), con)
  flush(con)
  close(con)
  invisible(NULL)
}

cat(sprintf("TAGBIO_WORKER_READY %d %s\n", port, token))
flush(stdout())

while (engine_alive()) {
  if (difftime(Sys.time(), last_sweep, units = "mins") > 10) {
    try(sweep_tempdir(), silent = TRUE)
    last_sweep <- Sys.time()
  }
  con <- tryCatch(socketAccept(server, blocking = TRUE, open = "r+", timeout = 1), error = function(e) NULL)
  if (is.null(con)) next
  job <- tryCatch(rjson::fromJSON(readLines(con, n = 1, warn = FALSE)), error = function(e) NULL)
  if (is.null(job) || !identical(job$token, token)) {
    close(con)
    next
  }
  parallel::mcparallel(run_child(con, job), detached = TRUE)
  close(con)
}
close(server)
