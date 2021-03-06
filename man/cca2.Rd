% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ccaR2.R
\name{cca2}
\alias{cca2}
\title{Calculates the total Corrected Covered Area (CCA) Index}
\usage{
cca2(cm)
}
\arguments{
\item{cm}{A dataframe for the citation matrix}
}
\value{
res
}
\description{
This package calculates the Corrected Covered Area (CCA) index. The measure for assessing the overall degree of overlap in an OOSR. It is taking as input the citation matrix.
}
\examples{
DATASET<-readxl::read_excel(system.file('extdata','cca.xlsx', package = 'ccaR'))

cca_table<-cca(DATASET)



}
