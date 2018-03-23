module Good.Architecture.Scraper where

import Good.Architecture.Output

class Scraper s where
    scrape :: s -> Outputting o m a  
