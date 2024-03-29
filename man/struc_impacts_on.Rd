% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impact.R
\name{struc_impacts_on}
\alias{struc_impacts_on}
\title{Construct impact equations for a given stock (level)}
\usage{
struc_impacts_on(stk, inputs, simplify = TRUE)
}
\arguments{
\item{stk}{A string that indicates the stock's name}

\item{inputs}{A list of three elements (flows, pathways, velocities). One can
obtain this list automatically from the function \code{sd_impact_inputs} from
the package \code{readsdr}.}

\item{simplify}{A boolean that indicates whether the analytical expression is
algebraically simplified}
}
\value{
A data frame.
}
\description{
Construct impact equations for a given stock (level)
}
\examples{
 flows <- data.frame(
   stock    = c("x", "y", "y", "z", "z"),
   flow     = c("f1", "f1", "f2", "f2", "f3"),
   sign     = c("-", "+", "-", "+", "-"),
   equation = c("R*x*z", "R*x*z", "a*y", "a*y", "y"))

 pathways <- data.frame(
 from    = c("z", "x", "z", "x", "y", "y", "z"),
 to      = c("x", "x", "y", "y", "y", "z", "z"),
 through = c("f1", "f1", "f1", "f1", "f2", "f2", "f3"))

 velocities <- data.frame(
   stock    = c("x", "y", "z"),
   equation = c(
     "-R * x * z",
     " R * x * z - a * y",
     "a * y - z"))

 inputs <- list(flows      = flows,
                pathways   = pathways,
                velocities = velocities)

 struc_impacts_on("x", inputs)
}
