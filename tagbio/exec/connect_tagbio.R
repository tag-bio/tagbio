#!/usr/bin/env Rscript
# connect_tagbio.R
#
# author: j@tag.bio
# version: 0.9.3
# last update: 2024.11.18
#

suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("rjson"))
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("tagbio"))
suppressPackageStartupMessages(library("yaml"))


## simple Rmd reader
rmd_reader <- function(rmd_file) {
  yaml_content <- c()
  in_yaml <- FALSE
  other_content <- c()
  con <- file(rmd_file, "r")
  while (TRUE) {
    line <- readLines(con, n=1)

    if (length(line) == 0) {
      break
    }

    if (grepl("^---", line)) {
      in_yaml <- !in_yaml
    } else {
      if (in_yaml) {
        yaml_content <- c(yaml_content, line)
      } else {
        other_content <- c(other_content, line)
      }
    }
  }
  return(list(yaml_content = yaml_content, other_content = other_content))
}

## updates rmd files with required fields
rmd_updater <- function(rmd_file, email, analysis_url) {

  rmd <- rmd_reader(rmd_file)

  # check the yaml section
  yaml <- read_yaml(text = paste(rmd$yaml_content, collapse="\n"))
  yaml_fields <- names(yaml) # top level

  # email
  if (!("email" %in% yaml_fields) | is.na(yaml["email"])) {
    yaml["email"] <- email
  }

  # analysis_url
  if (!("analysis_url" %in% yaml_fields) | is.na(yaml["analysis_url"])) {
    if (!is.null(analysis_url)) {
      analysis_url <- gsub("https://", "", analysis_url) # stops the auto formatting
    }
    yaml["analysis_url"] <- analysis_url
  }

  # date
  if (!("date" %in% yaml_fields) | is.na(yaml["date"])) {
    yaml["date"] <- "`r Sys.Date()`"
  }

  # tag data and results - add these fields no matter what
  if  (!("params" %in% yaml_fields) | is.na(yaml["params"])) {
    yaml$params <- list()
  }
  yaml$params$tag_data <- ""
  yaml$params$tag_result <- ""

  # save new rmd to temp file
  # - save to same directory as original so that other content is accessible
  rmd_out_file <- tempfile(pattern = "_tmp_",
                           tmpdir = dirname(rmd_file),
                           fileext = ".Rmd")

  con <- file(rmd_out_file, "w")
  write("---", con)
  write_yaml(yaml, con)
  write("---\n\n", con)
  write(paste(rmd$other_content, collapse = "\n"), con)
  write("\n", con)
  close(con)

  return(rmd_out_file)
}

si <- sessionInfo()
print(paste("Tagbio SDK Version:", si$otherPkgs$tagbio$Version))

## Command line options
option_list = list(
  make_option(c("-d", "--fc_data"), action="store", default=NA, type='character',
              help="JSON file specifying FC and protocol."),
  make_option(c("-f", "--user_function"), action="store", default=NA, type='character',
              help="User function passed in a R file."),
  make_option(c("-m", "--message_file"), action="store", default=NA, type='character',
              help="Save messages to a file at this path."),
  make_option(c("-o", "--output_file"), action="store", default=NA, type='character',
              help="Save result file to this path.  DEPRECATED."),
  make_option(c("-O", "--output_files"), action="store", default=NA, type='character',
              help="Result files given as comma delimited key:path pairs."),
  make_option(c("-t", "--output_type"), action="store", default=NA, type='character',
              help="File type of result.")
)
args = parse_args(OptionParser(option_list=option_list))

# test for required options
if (is.na(args$fc_data)) {
  stop("fc_data is required.  See script usage (--help)")
}

if (is.na(args$user_function)) {
  stop("user_function is required.  See script usage (--help)")
}

if (is.na(args$output_file)) {
  stop("output_file is required.  See script usage (--help)")
}

if (!is.na(args$output_files)) {
  args$output_files <- unlist(strsplit(args$output_files,
                                       ","))
}

if (is.na(args$output_type)) {
  stop("output_type is required.  See script usage (--help)")
}

if (!(args$output_type %in% c("csv", "html", "json", "pdf", "png", "tar"))) {
  stop("output_type must be one of: csv, html, json, pdf, png, tar.  See script usage (--help)")
}

## Read in the fc params and create FC and protocol instances
fc_data <- fromJSON(file = args$fc_data)
fc_params <- fc_data$fc
fc_request <- fc_data$request
fc_name <- fc_params$name

# load params
# This is the full URL to the FC
fc_url <- NULL
if (!is.null(fc_params$`fc-url`)) {
  fc_url <- fc_params$`fc-url`
}
# This URL us used for adding links to reports
fc_protocol_url <- NULL
if (!is.null(fc_params$`protocol-url`)) {
  fc_protocol_url <- fc_params$`protocol-url`
}
# This URL is just local host
url <- NULL
if (!is.null(fc_params$`url`)) {
  url <- fc_params$`url`
}
fc_token <- NULL
if (!is.null(fc_request$auth)) {
  # Removes "Bearer "
  fc_token <- gsub(".* ", "", fc_request$auth)
}

fc_user_email <- Sys.info()['user'] # default to local user
if (!is.null(fc_request$email)) {
  fc_user_email <- fc_request$email
}
# we swap out the @ so that render does not change email into a link
fc_user_email <- gsub("@", "<span>&#64;</span>", fc_user_email)
fc_blob_id <- NULL
if (!is.null(fc_request$uuid)) {
  fc_blob_id <- fc_request$uuid
}

# make connection
# - we are now always running locally
#if (is.null(fc_url) | is.null(fc_token)) {
print("Using localhost to communicate with API.")
tag_con <- tagConnect()
fc <- tagFC(tag_con)
#} else {
#  print("Using token-based authentication to communicate with API.")
#  tag_con <- tagConnect(url = fc_url, token = fc_token)
#  fc <- tagFC(tag_con, fc_name)
#}

## Look for protocol instance or script
tag_data <- NULL
if (!is.null(fc_data[['protocol_instance']])) {
    prot_inst <- fc_data[['protocol_instance']]
    tag_data <- run_protocol(fc, prot_inst)
} else {
  if (!is.null(fc_data[['script']])) {
    script <- fc_data[['script']]
    tag_data <- run_script(fc, script)
  } else {
    # create an empty tag_data object
    tag_data <- tagData(results = tibble::tibble())
  }
}

## Set up a tag result object with file paths
tag_result <- tagResult(output_path = args$output_file,
                        result_type = args$output_type)
if (!is.null(args$message_file)) {
    tag_result$message_path <- args$message_file
}

# apply pass through arguments if exist
tag_params <- NULL
if (!is.null(tag_data)) {
  ## Add to this the pass through attributes
  tag_data$parameters <- fc_data$passthrough_arguments

  tag_params <- get_parameters(tag_data)
}

# add in additional parameters
tag_data$parameters$token <- fc_token
tag_data$parameters$fc_user_email <- fc_user_email
tag_data$parameters$fc_blob_id <- fc_blob_id
tag_data$parameters$fc_url <- fc_url

# A bit fragile, this assumes we can get the host URL by looking for the
# "fc-svc" element in the FC url and trimming it away.  This host URL is
# then made available for call backs to other FCs
tag_data$parameters$host_url <- gsub("/fc-svc/.*", "", fc_url)

output_set <- FALSE
if (!is.null(tag_params) & !is.null(tag_params$output_file) & !is.null(tag_params$output_type)) {
    tag_result <- add_result_file(tag_result, tag_params$output_file[1], tag_params$output_type[1])
    output_set <- TRUE
}

## render a notebook or execute function
if (grepl(".Rmd", args$user_function)) {
  print("Knitting Rmd Now")

  rmd_tmp_file <- rmd_updater(args$user_function, fc_user_email, fc_protocol_url)

  tryCatch(
    rmarkdown::render(rmd_tmp_file,
                      #intermediates_dir=protocol_dir,
                      #knit_root_dir=protocol_dir,
                      params = list(tag_data = tag_data, tag_result = tag_result),
                      output_file = args$output_file),
    error=function(cond) {
      message("Rmarkdown render has failed.")
      message("Here's the error message:")
      message(cond)

      # quit script with error code
      q(save = "no", status = 1)
    })

    # remove the temp file
    file.remove(rmd_tmp_file)

} else {

  print("Executing function")
  ## Read in user function
  user_function <- dget(args$user_function)

  ## Run the user function.  Should return a tag result back
  tag_result <- user_function(tag_data, tag_result)
}
print("Finished R process")
