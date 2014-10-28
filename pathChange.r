setwd("~/Documents/towerProperty")

## text files needing path changes
filenames <- c( "data.r", "msTest.r" ) # msTest.r is a dummy file place holder

## change path from PC "\\" to Mac "/"
for( f in filenames ){
  path.x <- readLines(f)
  path.y <- gsub( "\\\\\\\\", "/", path.x )
  cat(path.y, file=f, sep="\n")
}

## Review output
for( f in filenames ){ 
  print(head(readLines(f)))
}
