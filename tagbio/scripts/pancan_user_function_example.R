
suppressPackageStartupMessages(library(tagbio))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gplots))
#suppressPackageStartupMessages(library(devtools))

#Load latest version of heatmap.3 function
#source_url("https://raw.githubusercontent.com/obigriffith/biostar-tutorials/master/Heatmaps/heatmap.3.R")


function(tag_data, tag_result) {
    # user function returns top n-lines of data
    print("Starting user function")

    exp_data <- getDataFrame(tag_data, data_type = "Expression", 
                         row_name ="Sample ID")

    smp_filter <- apply(is.na(exp_data), 1, sum) / ncol(exp_data) < 0.1 
    exp_matrix <- as.matrix(exp_data[smp_filter,])
    rownames(exp_matrix) <- rownames(exp_data)[smp_filter]
    
    # open graphics device driver and add pdf to result
    pdf(tag_result, height=11, width=8.5)
    heatmap.2(t(exp_matrix), scale="column", trace="none", 
           key = FALSE, margin=c(6,8))
    dev.off()
    print("Finished user function")
    return(tag_result)
}