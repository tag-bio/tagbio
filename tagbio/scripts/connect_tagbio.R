#!/usr/bin/env Rscript
# connect_tagbio.R
# 
# author: j@tag.bio
# version: 0.2
# last update: 2020.01.07
# 
# Example execution:
# ./tag.R -u https://fc-skcm.fc.tag.bio/q -j payload_example.json -f user_function_example.R -p params_example.json -o output.csv
#

print("Starting R process")

## Command line options
suppressPackageStartupMessages(library("argparse"))
suppressPackageStartupMessages(library("rjson"))
suppressPackageStartupMessages(library("tagbio"))
parser <- ArgumentParser()

parser$add_argument("-d", "--fc_data", required = TRUE,
    help="JSON file specifying FC and protocol.")
parser$add_argument("-f", "--user_function", required = TRUE,
    help="User function passed in a R file.")
parser$add_argument("-P", "--pdf_file", required = FALSE, default = "",
    help="Save result PDF file to this path.")
parser$add_argument("-N", "--png_file", required = FALSE, default = "",
    help="Save result PNG file to this path.")
parser$add_argument("-J", "--jpeg_file", required = FALSE, default = "",
    help="Save result JPEG file to this path.")
parser$add_argument("-S", "--svg_file", required = FALSE, default = "",
    help="Save result SVG file to this path.")
parser$add_argument("-D", "--dataframe_file", required = FALSE, 
    help="Save result data frame file to this path.")

args <- parser$parse_args()

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
tag_result <- TagbioResult(jpeg = args$jpeg_file, pdf = args$pdf_file, 
    png = args$png_file, svg = args$svg_file) 

## Run the user function.  Should return a tag result back
tag_result <- user_function(tag_data, tag_result)

print("Finished R process")