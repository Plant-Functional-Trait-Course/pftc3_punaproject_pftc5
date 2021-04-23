# TNRS and The Plant List corrections



genus_tnrs <- read.table(header = TRUE,
                                  stringsAsFactors = FALSE,
                                  text = "genus genus_new
                                  Agrostis Polypogon
                                  Cyrtochilum Cyrtochilum
                                  Lucilia Belloa"
                                  )

species_tnrs <- read.table(header = TRUE,
                                  stringsAsFactors = FALSE,
                                  text = "species species_new
                                  haenkeana exasperatus
                                  mystacinum aureum
                                  kunthiana kunthiana"
)
