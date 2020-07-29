#!/usr/bin/env Rscript
#
# Install a package from BioConductor, return nonzero if failure
#
check_install <- function(lb, pkg_type = "default") {

    if (pkg_type == "default") {
        install.packages(lb, dependencies=TRUE, repos='https://cran.rstudio.com/')
    } else {
        BiocManager::install(lb)
    }

    if ( !library(lb, character.only=TRUE, logical.return=TRUE) ) {
        quit(status=1, save='no')
    }
}


args = commandArgs(trailingOnly=TRUE)
print("Installing BioConductor package:");
print(args)

if (!requireNamespace("BiocManager", quietly = TRUE))
    check_install("BiocManager")

check_install(args[1], pkg_type = "bioconductor")

