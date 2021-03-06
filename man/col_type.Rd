% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{col_type<-}
\alias{col_type<-}
\alias{col_type}
\title{Set or retrieve a column type}
\usage{
col_type(x, cols = NULL) <- value

col_type(x, cols = NULL)
}
\arguments{
\item{x}{a colorful data frame}

\item{cols}{column names to set or retrieve}

\item{value}{character vector with column types}
}
\description{
Set or retrieve a column type of a colorful data frame
}
\details{
Rather than directly assigning a style to a column (which is possible
using the \code{col.styles} element) it is preferable to change a style
associated with a column type. Several such types are defined in the
default styles:
\itemize{
\item character
\item numeric
\item integer
\item factor
\item identifier
\item pval
\item match
\item hidden
\item default
}

Of course, new column types may be defined and their formatting defined
in a theme or a particular data frame style.
}
\examples{
mc <- colorDF(mtcars)
col_type(mc, "gear") <- "factor"
col_type(mc, "gear")
col_type(mc) <- list(gear="factor", cyl="integer")
## Note: the *class* of the columns did not change!
## Chaning column type merely changes the way it is displayed
class(mc[["gear"]])

## Hide some columns
col_type(mc, c("disp", "hp")) <- "hidden"

## Create a new type and style
col_type(mc, "carb") <- "carbstyle"
df_style(mc)$type.styles$carbstyle <- list(fg="red", decoration="bold")
}
