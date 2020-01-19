
suppressPackageStartupMessages(library(tagbio))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gridExtra))

function(tag_data, tag_result) {
    # user function returns top n-lines of data
    n_lines <- tag_data@parameters$n_lines
    short_tag_data <- tag_data@data.frame %>% top_n(n_lines)
    
    # open graphics device driver and add pdf to result
    pdf(tag_result, height=11, width=8.5)
    grid.table(short_tag_data)
    dev.off()

    return(tag_result)
}