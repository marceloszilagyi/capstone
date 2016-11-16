# wondeful function to read samples
fsample <-
  function(fname, n, seed, header=FALSE, ..., reader = read.csv)
  {
    set.seed(seed)
    con <- file(fname, open="r")
    hdr <- if (header) {
      readLines(con, 1L)
    } else character()
    
    buf <- readLines(con, n)
    n_tot <- length(buf)
    
    repeat {
      txt <- readLines(con, n)
      if ((n_txt <- length(txt)) == 0L)
        break
      
      n_tot <- n_tot + n_txt
      n_keep <- rbinom(1, n_txt, n_txt / n_tot)
      if (n_keep == 0L)
        next
      
      keep <- sample(n_txt, n_keep)
      drop <- sample(n, n_keep)
      buf[drop] <- txt[keep]
    }
    
    reader(textConnection(c(hdr, buf)), header=header, ...)
  }