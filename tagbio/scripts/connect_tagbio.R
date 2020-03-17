#!/usr/bin/env Rscript
# connect_tagbio.R
#
# author: j@tag.bio
# version: 0.3
# last update: 2020.02.18
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
parser$add_argument("-o", "--output_file", required = FALSE, 
    help="Save result file to this path.")
parser$add_argument("-t", "--output_type", required = FALSE, 
    help="File type of result.", choices = c("jpg", "html", "pdf", "png", "svg"))

args <- parser$parse_args()

print("Starting R process")

## method for adding file paths to tag result
add_result_file <- function(tag_res, file_path, file_type) {
    if (file_type == 'pdf') {
        tag_res@pdf <- file_path
    }
    if (file_type == 'png') {
        tag_res@png <- file_path
    }
    if (file_type == 'jpeg') {
        tag_res@jpeg <- file_path
    }
    if (file_type == 'svg') {
        tag_res@svg <- file_path
    }
    if (file_type == 'html') {
        tag_res@html <- file_path
    }
    return(tag_res)
}


## Read in user function
user_function <- dget(args$user_function)

## Read in the fc params and create FC and protocol instances
fc_data <- fromJSON(file = args$fc_data)
fc <- FC(name = fc_data$fc$name, url = fc_data$fc$url)
prot_inst <- ProtocolInstance(name = fc_data$protocol_instance$name, arguments = fc_data$protocol_instance$arguments)

## Load the data into a TagbioData object
tag_data <- getTagData(fc, prot_inst)

## Add to this the pass through attributes
tag_data@parameters <- fc_data$passthrough_arguments

## Set up a tag result object with file paths
tag_result <- TagbioResult()

# apply pass through arguments if exist
params <- tag_data@parameters
output_set <- FALSE
if (!is.null(params) & !is.null(params$output_file) & !is.null(params$output_type)) {
    print("HERE")
    tag_result <- add_result_file(tag_result, params$output_file[1], params$output_type[1])
    output_set <- TRUE
}

# apply commandline arguments if set
if (!is.null(args$output_file) & !is.null(args$output_type)) {
    tag_result <- add_result_file(tag_result, args$output_file, args$output_type)
    output_set <- TRUE
}

if (!output_set) {
    print("ERROR:  No output file or output type specified.")
    quit(save = "no", status = 1)
}

## Run the user function.  Should return a tag result back
tag_result <- user_function(tag_data, tag_result)

print("Finished R process")