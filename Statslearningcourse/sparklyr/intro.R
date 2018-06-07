# tutorial can be found at spark.rstudio.com


library(sparklyr)
spark_install(version = "1.6.2")
sc <- spark_connect(master = "local")

library(dplyr)
iris_tbl <- copy_to(sc, iris)
