# fetch_rds() removes partial downloads on failure

    Code
      fetch_rds("oz_books", dir = dir)
    Condition
      Error in `download_file()`:
      ! HTTP status was '504 Gateway Timeout'

