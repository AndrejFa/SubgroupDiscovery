% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rsubgroup-results.R
\name{rsubgroupAnalysis}
\alias{rsubgroupAnalysis}
\title{Rsubgroup analysis}
\usage{
rsubgroupAnalysis(data_config, method, ...)
}
\arguments{
\item{data_config}{list with 2 elements: data frame with database and character vector of target attribute name and desired target value}

\item{method}{character method name}

\item{...}{arguments for \code{\link[rsubgroup]{DiscoverSubgroups}} configuration \code{\link[rsubgroup]{SDTaskConfig}}.
Method is added automatically so must be left out. It can be passed in each individually or packed in list object}
}
\value{
data frame with subgroups and summary statistic or list of data frames with subgroups and summary
statistic for each target attribute value
}
\description{
Do subgroup discovery with rsubgroup package
}
\details{
Function that extracts subgroups from data with interface to Vikamine platform for subgroup discovery.
Do basic analysis as we do it with rsubgroup package. Function use \code{\link[rsubgroup]{DiscoverSubgroups}}
functions from rsubgroup package for subgroup discovery analysis.
Argument passed insted of ... for \code{\link[rsubgroup]{DiscoverSubgroups}} configuration are prof checked for their names
and not for their types. If some Java error occur is due to incorrect argument type in configuration
see \code{\link[rsubgroup]{SDTaskConfig}} for details arguments type and description of configuration.
Function calculates analysis for all target attribute values if desired target value is not specified.
}
\seealso{
\code{\link[rsubgroup]{SDTaskConfig, DiscoverSubgroups}}
}
