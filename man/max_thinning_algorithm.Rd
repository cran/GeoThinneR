% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/max_thinning_algorithm.R
\name{max_thinning_algorithm}
\alias{max_thinning_algorithm}
\title{Thinning Algorithm for Spatial Data}
\usage{
max_thinning_algorithm(neighbor_indices, n, trials, all_trials = FALSE)
}
\arguments{
\item{neighbor_indices}{A list of integer vectors where each element contains the indices
of the neighboring points for each point in the dataset.}

\item{n}{The number of points in the dataset.}

\item{trials}{The number of thinning trials to run.}

\item{all_trials}{If TRUE, returns the results of all trials; if FALSE, returns the best
trial with the most points retained (default: FALSE).}
}
\value{
A list of logical vectors indicating which points are kept in each trial if
        all_trials is TRUE; otherwise, a single logical vector indicating the points kept
        in the best trial.
}
\description{
This function performs the core thinning algorithm used to reduce the density of points
in spatial data while maintaining spatial representation. It works by iteratively removing
points with the most neighbors until no points with neighbors remain. The algorithm
supports multiple trials to find the optimal thinning solution.
}
\examples{
# Example usage within a larger thinning function
neighbor_indices <- list(c(2, 3), c(1, 3), c(1, 2))
n <- 3
trials <- 5
all_trials <- FALSE
keep_points <- max_thinning_algorithm(neighbor_indices, n, trials, all_trials)
print(keep_points)
}
