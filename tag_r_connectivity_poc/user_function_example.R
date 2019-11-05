
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(tidyverse))

function(tag_data, output, params_json) {
    # just dump a few rows to file as an example
    params <- fromJSON(file = params_json)

    short_tag_data <- tag_data %>% top_n(params$number_lines)
    write.table(short_tag_data, output)

}