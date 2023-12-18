

imdbTSVfiles <- function(fileName){
  url <- paste0("https://datasets.imdbws.com/",fileName,".tsv.gz")
  tmp <- tempfile()
  download.file(url, tmp)
  
  assign(fileName,
         readr::read_tsv(
           file = gzfile(tmp),
           col_names = TRUE,
           quote = "",
           na = "\\N"),
         envir = .GlobalEnv)
}

imdbTSVfiles("title.basics")
imdbTSVfiles("title.ratings")


