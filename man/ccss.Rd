\name{ccss}
\alias{ccss}
\title{
Cluster Circular Systematic Sampling
}
\description{ 
Draws Systematic samples from a population that follows linear trend. The function returns a matrix comprising of the required samples as its column vectors. The samples produced are highly efficient and the inter sampling variance is minimum. The scheme will be useful in various field like Bioinformatics where the samples are expensive and must be precise in reflecting the population by possessing least \href{https://methods.sagepub.com/reference/encyclopedia-of-survey-research-methods/n515.xml}{sampling variance}.
}
\usage{
ccss(x, size, number)
}
\arguments{
  \item{x}{Population vector}
  \item{size}{Size of the required sample}
  \item{number}{Number of samples required}
}
\value{
The scheme arrange the population in a matrix of dimension and samples the column in a circular systematical fasion. The function returns a matrix having column as the required sample. The number of column is the number of sample required.
}
\author{Abhibhav Sharma}
\examples{ccss(6:60, 7, 10)}
