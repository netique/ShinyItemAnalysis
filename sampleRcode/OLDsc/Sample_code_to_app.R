# reading sample R code as string
txt <- readChar(
  "sampleRcode/Sample_Regression.R",
  file.info("sampleRcode/Sample_Regression.R")$size
)

# cutting string by headers
txt <- unlist(strsplit(
  txt,
  "# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\r\n"
))

# removing headers
txt <- txt[-1][seq(2, length(txt), 2)]
# styling
txt <- sapply(txt, function(x) gsub(" ", "&nbsp;", gsub("\r\n", "<br>", x)))
# removing the first and the last line break
txt <- sapply(
  txt, function(x)
    substr(x, start = 5, stop = nchar(x)))
txt <- sapply(
  txt, function(x)
    substr(x, start = 1, stop = nchar(x) - 4))
names(txt) <- NULL

txt
