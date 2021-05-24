#!/usr/bin/env Rscript
# connect_tagbio.R
#
# author: j@tag.bio
# version: 0.4
# last update: 2021.05.05
#

## Command line options
suppressPackageStartupMessages(library("argparse"))
suppressPackageStartupMessages(library("rjson"))
suppressPackageStartupMessages(library("tagbio"))
parser <- ArgumentParser()

parser$add_argument("-d", "--fc_data", required = TRUE,
    help="JSON file specifying FC and protocol.")
parser$add_argument("-f", "--user_function", required = TRUE,
    help="User function passed in a R file.")
parser$add_argument("-m", "--message_file", required = FALSE,
    help="Save messages to a file at this path.")
parser$add_argument("-o", "--output_file", required = TRUE,
    help="Save result file to this path.  DEPRECATED.")
parser$add_argument("-O", "--output_files", required = FALSE, nargs='+',
    help="Result files given as key:path pairs.")
parser$add_argument("-t", "--output_type", required = TRUE,
    help="File type of result.", choices = c("csv", "html", "json", "pdf", "png"))
parser$add_argument("-u", "--username", required = FALSE, default = NULL,
                    help="Username for authentication.")
parser$add_argument("-p", "--password", required = FALSE, default = NULL,
                    help="Password for authentication.")

args <- parser$parse_args()




## Read in the fc params and create FC and protocol instances
fc_data <- fromJSON(file = args$fc_data)
fc <- FC(name = fc_data$fc$name, url = fc_data$fc$url)

## Look for protocol instance or script
prot_inst <- NULL
if (!is.null(fc_data[['protocol_instance']])) {
    prot_inst <- fc_data[['protocol_instance']]
}

script <- NULL
if (!is.null(fc_data[['script']])) {
    script <- fc_data[['script']]
}

## Load the data into a TagbioData object
tag_data <- getTagData(fc, prot_inst, script,
                       username = args$username, password = args$password)

## Add to this the pass through attributes
tag_data@parameters <- fc_data$passthrough_arguments

## Set up a tag result object with file paths
tag_result <- TagbioResult(output_path = args$output_file, result_type = args$output_type)
if (!is.null(args$message_file)) {
    tag_result@message_path <- args$message_file
}

# apply pass through arguments if exist
tag_params <- tag_data@parameters
output_set <- FALSE
if (!is.null(tag_params) & !is.null(tag_params$output_file) & !is.null(tag_params$output_type)) {
    tag_result <- add_result_file(tag_result, tag_params$output_file[1], tag_params$output_type[1])
    output_set <- TRUE
}

## render a notebook or execute function
if (grepl(".Rmd", args$user_function)) {
  print("Knitting Rmd")
  rmarkdown::render(args$user_function,
                    params = list(tag_data = tag_data, tag_result = tag_result),
                    output_file = args$output_file)
#                    params = list(tag_data = tag_data, tag_result = tag_result))
  print("Done")
} else {
  print("Executing function")
  ## Read in user function
  user_function <- dget(args$user_function)

  ## Run the user function.  Should return a tag result back
  tag_result <- user_function(tag_data, tag_result)
}
print("Finished R process")
