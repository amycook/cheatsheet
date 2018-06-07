library(babynames)
library('magrittr')
library('dplyr')
library('ggplot2')

#tutorial
str1= "A scratch? Your arm's off."
str2= "I've had worse."

#same
substr(str1,3,9)
str1 %>% substr(3,9)

?strsplit
str1 %>% strsplit('?', fixed=TRUE)

#chain pipes
#same
toupper(paste(str1,str2))
str1 %>% paste(str2) %>% toupper()

#function
x<- -5:5
vmax= function(x) apply(cbind(x,0),1,max)
vmax(-5:5)

vmax<- function(x) x %>% cbind(0) %>% apply(1,max)

#another example
Nitem  <- 100
ctmean <- 1
ctsd   <- .5

pantheria <-
        "http://esapubs.org/archive/ecol/E090/184/PanTHERIA_1-0_WR05_Aug2008.txt"
download.file(pantheria, destfile = "mammals.txt")

mammals <- read.table("mammals.txt", sep = "\t", header = TRUE, 
                      stringsAsFactors = FALSE)
names(mammals) <- sub("X[0-9._]+", "", names(mammals))
names(mammals) <- sub("MSW05_", "", names(mammals))
mammals <- dplyr::select(mammals, Order, Binomial, AdultBodyMass_g, 
                         AdultHeadBodyLen_mm, HomeRange_km2, LitterSize)
names(mammals) <- gsub("([A-Z])", "_\\L\\1", names(mammals), perl = TRUE)
names(mammals) <- gsub("^_", "", names(mammals), perl = TRUE)
mammals[mammals == -999] <- NA
names(mammals)[names(mammals) == "binomial"] <- "species"
mammals <- dplyr::tbl_df(mammals) 
glimpse(mammals)
str(mammals)
select(mammals, species)
#select and filter are awesome - select and filter dataframes by columns and rows respectively
filter(mammals, order == "Carnivora" & adult_body_mass_g < 200)
select(mammals, ends_with("g"))
select(mammals, contains("body"))
select(mammals, adult_head_body_len_mm, litter_size)


#piping
mammals %>% arrange(adult_body_mass_g)

#find the species with the highest body mass to length ratio
glimpse(mammals)

mammals %>%
        mutate(ratio=adult_body_mass_g/adult_head_body_len_mm) %>%
        arrange(-ratio) %>%
        select(species, ratio)

# find what taxonomic orders have a median litter size greater than 3

mammals %>% 
        group_by(order) %>%
        summarise(median= median(litter_size, na.rm=TRUE)) %>%
        arrange(median) %>%
        filter(median>3) %>%
        select(order, median)




