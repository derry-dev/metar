library(data.table)
library(metar)

raw_metar <- fread(file = "examples/EHAM_Nov18_Nov19.txt")

processed_metar <- parse_METAR(raw_metar$metar)

fwrite(cbind(raw_metar, processed_metar), file = "examples/EHAM_Nov18_Nov19_processed.csv")
