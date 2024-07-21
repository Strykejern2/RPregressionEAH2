#' R package for regression analysis
#'
#' This package allows you to run a regression analysis, generate a regression table,
#' create a scatter plot, and download the results. It uses 'stargazer' for generating
#' regression tables and 'ggplot2' for creating plots. With just two lines of code, you can
#' perform a regression analysis, visualize the results, and save the output.
#'
#' @param x The independent variable.
#' @param y The dependent variable.
#' @param table The format for the regression table ("text", "html", or "latex").
#' @param plot Logical, whether to generate a plot.
#' @param xlab Label for x-axis in the plot.
#' @param ylab Label for y-axis in the plot.
#' @param title The title for the regression table and plot.
#' @param subtitle Subtitle for the plot (optional).
#' @param caption Caption for the plot (optional).
#' @param plottheme Theme for the plot (default is "theme_grey()").
#' @param download Logical, whether to save the regression table as a text file.
#' @param color_points Color for points in the plot (default is "black").
#' @param color_line Color for the regression line (default is "red").
#' @param ci Logical, whether to include confidence interval in the plot (default is TRUE).
#' @param sd Logical, whether to include standard deviation bars in the plot (default is FALSE).
#' 
#' @return If plot is TRUE, a plot is generated. Otherwise, a regression table is printed.
#' @examples
#' RPregressionEAH2_eah1(mtcars$mpg, mtcars$wt, table = "text", title = "Regression Analysis",
#' plot = TRUE, xlab = "mpg", ylab = "wt", plottheme = "theme_grey()", download = FALSE)
#'
#' @export
RPregressionEAH_eah1 <- function (x, y, table = "text", plot = FALSE, xlab = "", ylab = "", 
                                            title = "", subtitle = "", caption = "", plottheme = "theme_classic()", 
                                            download = FALSE, color_points = "black", color_line = "red", 
                                            ci = TRUE, sd = FALSE) 
{
    if (!requireNamespace("stargazer", quietly = TRUE)) {
        stop("The 'stargazer' package is required but not installed.")
    }
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("The 'ggplot2' package is required but not installed.")
    }
    x_name <- deparse(substitute(x))
    y_name <- deparse(substitute(y))
    formula <- as.formula(paste(y_name, "~", x_name))
    model <- lm(formula, data = data.frame(x, y))
    adj_r2 <- summary(model)$adj.r.squared
    p_value <- summary(model)$coefficients[2, 4]
    p_value_text <- ifelse(p_value < 0.001, "p-value < 0.001", paste("p-value = ", round(p_value, 3)))
    
    if (table == "text") {
        regression <- stargazer::stargazer(model, type = "text", 
                                           title = title)
    }
    else if (table == "html") {
        regression <- stargazer::stargazer(model, type = "html", 
                                           title = title)
    }
    else if (table == "latex") {
        regression <- stargazer::stargazer(model, type = "latex", 
                                           title = title)
    }
    else {
        stop("Please select either 'text', 'html', or 'latex' for the table format.")
    }
    if (plot) {
        pacman::p_load(ggplot2)
        p <- ggplot2::ggplot(data = data.frame(x, y), ggplot2::aes(x, y)) + 
            ggplot2::geom_point(color = color_points) + 
            ggplot2::geom_smooth(method = "lm", se = ci, color = color_line) + 
            ggplot2::labs(x = xlab, y = ylab, title = title, 
                          subtitle = subtitle, caption = caption) +
            ggplot2::annotate("text", x = min(x), y = max(y), 
                              label = paste(" Adj R2 = ", round(adj_r2, 3), "\n", p_value_text), 
                              hjust = 0, vjust = 1, size = 5, color = "black") + 
            ggplot2::theme_classic() + 
            ggplot2::theme(text = element_text(family = "Arial", size = 12), 
                           plot.title = element_text(size = 14, face = "bold"),
                           plot.subtitle = element_text(size = 12),
                           plot.caption = element_text(size = 11),
                           panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
                           axis.text = element_text(size = 12, face = "bold"),
                           axis.title = element_text(size = 14,face = "bold")) + 
            ggplot2::theme(panel.grid.major = element_line(size = 0.25, color = "pink"), panel.grid.minor = element_blank())
        if (sd) {
            p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = predict(model) - 
                                                             sd(residuals(model)), ymax = predict(model) + 
                                                             sd(residuals(model))), width = 0.2, color = "gray")
        }
        if (download) {
            sink(paste0(title, ".txt"))
            print(stargazer::stargazer(model, type = "text", 
                                       title = title))
            sink()
        }
        return(list(model_summary = summary(model), plot = p, 
                    regression_table = regression))
    }
    else {
        if (download) {
            sink(paste0(title, ".txt"))
            print(stargazer::stargazer(model, type = "text", 
                                       title = title))
            sink()
        }
        return(list(model_summary = summary(model), regression_table = regression))
    }
}
