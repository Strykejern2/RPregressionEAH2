\name{RPregression}
\alias{RPregression}
\title{Perform Regression Analysis and Plot Results}
\description{
This function allows you to run a regression analysis, generate a regression table,
create a scatter plot, and download the results, all without the need for additional
packages such as 'stargazer', 'ggplot2', or 'ggthemes'. With just two lines of code, you can
perform a regression analysis, visualize the results, and save the output.
}
\usage{
RPregression(x, y, table = "text", plot = FALSE, xlab = "", ylab = "", 
             title = "", subtitle = "", caption = "", plottheme = "theme_grey()", 
             download = FALSE, color_points = "black", color_line = "red", 
             ci = TRUE, sd = FALSE)
}
\arguments{
  \item{x}{The independent variable.}
  \item{y}{The dependent variable.}
  \item{table}{The format for the regression table ("text", "html", or "latex").}
  \item{plot}{Logical, whether to generate a plot.}
  \item{xlab}{Label for x-axis in the plot.}
  \item{ylab}{Label for y-axis in the plot.}
  \item{title}{The title for the regression table and plot.}
  \item{subtitle}{Subtitle for the plot (optional).}
  \item{caption}{Caption for the plot (optional).}
  \item{plottheme}{Theme for the plot (default is "theme_grey()").}
  \item{download}{Logical, whether to save the regression table as a text file.}
  \item{color_points}{Color for points in the plot (default is "black").}
  \item{color_line}{Color for the regression line (default is "red").}
  \item{ci}{Logical, whether to include confidence interval in the plot (default is TRUE).}
  \item{sd}{Logical, whether to include standard deviation bars in the plot (default is FALSE).}
}
\value{
If plot is TRUE, a plot is generated. Otherwise, a regression table is printed.
}
\examples{
RPregression(mtcars$mpg, mtcars$wt, table = "text", title = "Regression Analysis",
plot = TRUE, xlab = "mpg", ylab = "wt", plottheme = "theme_grey()", download = FALSE)
}
\author{
Raghav Puri
Maintainer: Raghav Puri <itsraghavpuri@gmail.com>
}
