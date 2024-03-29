% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/excel_to_plan.R
\name{excel_to_plan}
\alias{excel_to_plan}
\title{Create a drake plan using tabular species data}
\usage{
excel_to_plan(file)
}
\arguments{
\item{file}{Character. File path to an xlsx file containing required
parameters for creating a species plan. See Details and \code{\link[=species_plan]{species_plan()}}
for further information.}
}
\value{
A drake plan with a combined workflow for all species.
}
\description{
Import parameters from an Excel xlsx file and create a drake plan to
estimate establishment likelihoods.
}
\details{
To simplify reproducibility, \code{edmaps} provides an \emph{Excel} interface
for specifying species parameters relevant to estimating establishment
likelihood. An example spreadsheet is bundled with the package, available
at the path given by \code{system.file('extdata/parameters.xlsx', package='edmaps')}. The spreadsheet has two sheets, the first specifying
"global" parameters that will apply to all species (e.g. file paths to
rasters that will), be used regardless of species identity and the second
specifying parameters that can vary by species. In the second sheet, each
row corresponds to a separate species. Tooltips and data validation guide
the user with respect to expected/allowable data.
}
