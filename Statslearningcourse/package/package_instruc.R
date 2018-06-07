
library("roxygen2")
library("devtools")

package.name = "isofor-master"
path.to.folder.WIN = "C:/gitlab"
path.to.folder.MAC = "~/Documents/github/"



if(.Platform$OS.type == 'windows'){
        setwd(path.to.folder.WIN)
} else{
        setwd(path.to.folder.MAC)
}

create(package.name)


#edit the description file in the new cats folder
# in description file
# add 'Imports:
#         dplyr,
#         magrittr'
#     
#  etc for packages that need to be attached

# copy and paste your functions into a new .R file.
# save this to the 'R' folder in the new directory
# copy and paste the 'documentation' to the front of the start function file:

#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()


# process the documentation
if(.Platform$OS.type == 'windows'){
        
        setwd(paste(path.to.folder.WIN, package.name, sep = "/"))

} else{
        setwd(paste("~/Documents/github/", package.name, sep =""))

}

devtools::document()

#add readme file
use_readme_rmd()

#install the package
if(.Platform$OS.type == 'windows'){
        setwd(path.to.folder.WIN)

} else{
        setwd("~/Documents/github/")
}

withr::with_libpaths(new = 'C:/Program Files/R/R-3.4.2/library', devtools::install(package.name))
devtools::install(package.name)
library(hwater)

?cat_function




