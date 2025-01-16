\title{Scrape Economic News from Radar Jawa Pos Website}
\name{scraperadar}
\alias{scraperadar}
\usage{
scraperadar(wilayahradar, x)
}
\arguments{
  \item{wilayahradar}{A character string indicating the specific Radar Jawa Pos region (e.g., "kediri").}
  \item{x}{An integer specifying the number of pages to scrape.}
}
\description{
  This function scrapes economic news articles from the Radar Jawa Pos website.
  It extracts article titles, publication dates, content, and links, and then processes
  the data to clean dates and categorize keywords.
}

\details{
  This function scrapes economic news articles from the Radar Jawa Pos website.
  It processes the data to clean the publication dates, categorize keywords, and extract relevant details from the articles.
}
\value{
  Returns a data frame containing the following columns:
  \item{judul}{The title of the news article.}
  \item{tglberita}{The publication date of the article.}
  \item{isiberita}{The content of the news article.}
  \item{link_judul}{The link to the full news article.}
  \item{tanggal}{The cleaned publication date in YYYY-MM-DD format.}
  \item{sentimen}{Sentiment classification based on keywords in the title.}
  \item{lokasi}{The location mentioned in the content (e.g., "Kediri").}
  \item{katakunci}{The extracted keywords from the article content.}
  \item{dptisi}{The function for extracting the article content.}
}

\author{Joko Ade Nursiyono}
\seealso{Other related functions, if any}
\references{Relevant references or documentation for the website or scraping process}
