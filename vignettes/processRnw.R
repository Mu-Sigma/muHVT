library(tools)
library(stringr)
library(rmarkdown)

rnwToRmd <- function(rnw) {
  
  lines <- readLines(rnw)
  
  ## replace knitr code chunks with Rmarkdown, keeping the arguments
  lines <- gsub(pattern = "^<<([[:print:]]+)>>=", replacement = "```{r \\1}", x = lines)
  lines <- gsub(pattern = "^@$", replacement = "```", x = lines)
  
  ## our new code blocks will get mangled by pandoc, so we put 'verbatim' around them to keep the formatting
  ## in the intermediatory steps
  codeBlocks <- matrix(grep(pattern = "^```", x = lines), ncol = 2, byrow = TRUE)
  lines[codeBlocks[,1]] <- paste0("\\begin{verbatim}\n", lines[codeBlocks[,1]] )
  lines[codeBlocks[,2]] <- paste0(lines[codeBlocks[,2]] , "\n\\end{verbatim}")
  
  ## pandoc doesn't seem to treat ciations correctly when there are multiple entries in a single \cite{}
  ## so we'll give it a helping hand.  We replace \cite{} with the markdown syntax [@]
  ## this isn't perfect and has to be processed again later
  ## it also only handles upto 3 references in a single tag, there should be a slick way of making this generic
  lines <- gsub("\\\\cite\\{([a-zA-Z0-9:.]+)\\}", replacement = "[@\\1]", x = lines)
  lines <- gsub("\\\\cite\\{([a-zA-Z0-9:.]+),([a-zA-Z0-9:.]+)\\}", replacement = "[@\\1; @\\2]", x = lines)
  lines <- gsub("\\\\cite\\{([a-zA-Z0-9:.]+),([a-zA-Z0-9:.]+),([a-zA-Z0-9:.]+)\\}", replacement = "[@\\1; @\\2; @\\3]", x = lines)
  
  ## write this hybrid format to file 
  tmp_file1 <- tempfile()
  writeLines(lines, con = tmp_file1)
  
  ## convert using pandoc from 'LaTeX' to markdown
  tmp_file2 <- tempfile()
  rmarkdown::pandoc_convert(input = tmp_file1, output = tmp_file2,
                            to = "markdown", from = "latex",
                            options = c("-s"))
  
  ## read the new markdown file
  ## code chunks now have 4 spaces in front of them, as they were in verbatim tags before conversion.
  ## We find them and remove the first instance of 4 spaces on each code line
  lines <- readLines(tmp_file2)
  codeBlocks <- matrix(grep(pattern = "^[[:space:]]+```", x = lines), ncol = 2, byrow = TRUE)
  codeLines <- unlist(apply(codeBlocks, 1, function(x) x[1]:x[2]))
  lines[codeLines] <- sub(pattern = " {4}", replacement = "", x = lines[codeLines])
  
  ## the pandoc conversion started escaping all our markdown citations
  ## we find them and remove the escape characters
  ## this is down in two opperations as the citations can fall over line breaks
  lines <- str_replace_all(string = lines, pattern = "\\\\(\\[@[[a-zA-Z0-9.:]]+)", replacement = "\\1")
  lines <- str_replace_all(string = lines, pattern = "(@[[a-zA-Z0-9.:]]+)\\\\]", replacement = paste0("\\1", "]"))
  
  ## write our process Rmarkdown document 
  writeLines(lines, con = paste0(tools::file_path_sans_ext(rnw), ".Rmd"))
}
