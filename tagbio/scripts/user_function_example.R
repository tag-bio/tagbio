
suppressPackageStartupMessages(library(tagbio))
suppressPackageStartupMessages(library(tidyverse))

function(tag_data) {
    # user function returns top n-lines of data
    n_lines <- tag_data@parameters$n_lines
    short_tag_data <- tag_data@data.frame %>% top_n(n_lines)
    print(short_tag_data)
    tag_result <- TagbioResult()
    tag_result@data.frame <- short_tag_data
    return(tag_result)
}