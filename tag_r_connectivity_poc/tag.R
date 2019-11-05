#!/usr/bin/env Rscript
# tag.R
# 
# author: j@tag.bio
# version: 0.1
# last update: 2019.11.05
# 
# Example execution:
# ./tag.R -u https://fc-skcm.fc.tag.bio/q -j payload_example.json -f user_function_example.R -p params_example.json -o output.csv
#

## Command line options
suppressPackageStartupMessages(library("argparse"))
suppressPackageStartupMessages(library("httr"))
parser <- ArgumentParser()

parser$add_argument("-u", "--fc_url", required = TRUE,
    help="FC callback URL")
parser$add_argument("-j", "--json_payload", required = TRUE,
    help="JSON payload to be submitted in FC callback")
parser$add_argument("-f", "--user_function", required = TRUE,
    help="User function passed in a R file.")
parser$add_argument("-p", "--params", 
    help="JSON file of parameters needed for user function.")
parser$add_argument("-o", "--output", required = TRUE,
    help="Save user function output to the file path.")
args <- parser$parse_args()

## Read in user function
user_function <- dget(args$user_function)

## Get data from FC
r <- POST(args$fc_url, body = upload_file(args$json_payload, type = "text/json"), encode = "json")
tag_data <- content(r, as = "parsed", type = "text/csv", encoding = "UTF-8")

## Run the user function
user_function(tag_data, args$output, args$params)
