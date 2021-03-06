% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss_get_single_item_shipping_info.R
\name{ss_get_single_item_shipping_info}
\alias{ss_get_single_item_shipping_info}
\title{Estimate product dimensions based on shipping LxWxH for recent single-item orders}
\usage{
ss_get_single_item_shipping_info(
  start_date = Sys.Date() - 7,
  end_date = Sys.Date() - 1
)
}
\arguments{
\item{start_date}{first day of data (Date/scalar)}

\item{end_date}{last day of data (Date/scalar)}
}
\description{
Estimate product dimensions based on shipping LxWxH for recent single-item orders
}
