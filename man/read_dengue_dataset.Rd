% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_dengue_dataset.R
\name{read_dengue_dataset}
\alias{read_dengue_dataset}
\title{read_dengue_dataset}
\usage{
read_dengue_dataset(
  path,
  spatial_resolution,
  status_caso,
  des_edo_res = NULL,
  des_mpo_res = NULL
)
}
\arguments{
\item{path}{is the path of dengue dataset.}

\item{spatial_resolution}{is the spatial resolution or the administrative level. The values are country, state and municipality.}

\item{status_caso}{1 probable, 2 confirmado, & 3 descartado.}

\item{des_edo_res}{is a string for define the state in uppercase.}

\item{des_mpo_res}{is a string for define the municipality in uppercase.}
}
\value{
a data.table object.
}
\description{
Read the dengue dataset of [SINAVE](https://www.sinave.gob.mx/)
}
\examples{
read_dengue_dataset(path = "/Users/felipedzul/Dropbox/dataset/dge/denv/2025/DENGUE2_.txt", spatial_resolution = "state", des_edo_res = "NUEVO LEON", status_caso = c(1, 2))
}
\author{
Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
}
