#!/usr/local/bin/Rscript
# connect_tagbio.R
# 
# author: j@tag.bio
# version: 0.2
# last update: 2020.01.07
# 
# Example execution:
# ./tag.R -u https://fc-skcm.fc.tag.bio/q -j payload_example.json -f user_function_example.R -p params_example.json -o output.csv
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

## Run the user function.  Should return a tag result back
tag_result <- user_function(tag_data)
