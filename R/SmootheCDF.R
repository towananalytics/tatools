#' Create a smoothed (theoretical) Cumulative Distribution Function plot
#'
#' This function produces a smoothed (theoretical) eCDF plot.
#'
#' @param data a dataframe containing the data.
#' @param y column reference to determine exceedance entered as an object (without quotes).
#' @param adj adjustment value for smoothing. Select values between 0 and 1.
#' @param color_var \emph{optional} a grouping variable entered as an object reference.
#' @param title \emph{optional} title for resulting plot.
#' @param subtitle \emph{optional} a subtitle for the resulting plot if required.
#' @param compare \emph{optional} logical value to output the true Empirical Cumulative Distribution Function with the smoothed function, providing a visual comparison.
#' @param coord_flip \emph{optional} to flip the X and Y axis for visual preference.
#' @param plot_only \emph{optional} when \code{TRUE} a ggtable object is returned without appending the data.
#' @param show_percent \emph{optional} when \code{TRUE} percentage labels are shown on the y axis instead of probability.
#' @import dplyr
#' @import ggplot2
#' @importFrom graphics abline plot points text
#' @importFrom stats quantile density
#' @return Depending on the argument passed to \code{plot_only} either a list object is returned when \code{plot_only == FALSE} containing a grob object \code{[["plot"]]} for plotting and a dataframe \code{[["data"]]} for further analysis
#' . A single plot is returned when \code{plot_only == TRUE}.
#' @examples
#' data(waves)
#' 
#'  plt_a <- smooth_ecd(data = waves,
#'  y = height_meters)
#'
#'  plot(plt_a)
#'
#'  plt_a <- smooth_ecd(data = waves,
#'  y = height_meters,
#'  plot_only = FALSE)
#'
#'  plot(plt_a[["plot"]])
#'
#'  # Add a grouping variable
#'  library(dplyr)
#'
#'  plt_b <- smooth_ecd(data = waves %>%
#'  mutate(hour = format(date_time, "%H")),
#'  y = height_meters,
#'  color_var = hour,
#'  adj = 1,
#'  title = "Wave Height Cumulative Distribution",
#'  subtitle = "By Hour",
#'  plot_only = FALSE)
#'
#'  # Access the list objects
#'  plot(plt_b[["plot"]])
#'  
#'  dat <- plt_b[["data"]]
#'
#' @export

smooth_ecd = function(data, y, adj = 1, color_var = NULL, title = NULL,
                      subtitle = NULL, compare = FALSE, ylab = NULL,
                      coord_flip = FALSE,
                      plot_only = TRUE,
                      show_percent = FALSE) {

  dat <- as.data.frame(data) # Ensure data is in df format

  y.lab <- deparse(substitute(y))
  y <- enquo(y)

  if(is.null(ylab)){
    ylab <- y.lab
  } else {

  }

  if(!missing(color_var)){

    lab.legend <- deparse(substitute(color_var))
    color_var <- enquo(color_var)

    count_distinct <- dat %>%
      select(!!color_var) %>%
      distinct(!!color_var)

    temp.df <- NULL

    for (i in 1:nrow(count_distinct)){

      temp.dat <- dat %>% filter(!!color_var == count_distinct[i, 1])
      dens <- density(temp.dat[, y.lab], adjust=adj, from=min(temp.dat[, y.lab]), to=max(temp.dat[, y.lab]))
      dens <-  data.frame(x=dens$x, y=dens$y, z = count_distinct[i, 1], cum.sum = cumsum(dens$y)/sum(dens$y))
      temp.df <- rbind(temp.df, dens)
    }

    names(temp.df) <- c("x", "y", lab.legend, "cum.sum")
    dens <- temp.df

    p <- ggplot(data = dat, aes(x = !!y)) +
      geom_line(data=dens, aes(x=x, y=cum.sum, colour = factor(!!color_var))) +
      labs(y = "Cumulative Distribution",
           colour = lab.legend,
           x = ylab,
           title = title,
           subtitle = subtitle)

  } else { # Colour var not passed so no grouping to occur

    dens <- density(dat[, y.lab], adjust=adj, from=min(dat[, y.lab]), to=max(dat[, y.lab]))
    dens <-  data.frame(x=dens$x, y=dens$y, cum.sum = cumsum(dens$y)/sum(dens$y))

    names(dens) <- c("x", "y", "cum.sum")

    p <- ggplot(data = dat, aes(x = !!y)) +
      geom_line(data=dens, aes(x=x, y=cum.sum), colour = "steelblue") +
      labs(y = "Cumulative Distribution",
           x = ylab,
           title = title,
           subtitle = subtitle)

    if(compare == TRUE){ # Add a comparison ecdf showing steps
      p <- p + stat_ecdf(colour="red", size=0.6, alpha=0.6)
    }

  }
    if(coord_flip == TRUE){
      
      p <- p + coord_flip()
      
    }
  
  if(show_percent == TRUE){ # Show percentage values on y axis
    
    p <- p +
      # ylim(0, 100) + # Change Exceednace scale for percentages
      scale_y_continuous(labels = scales::percent)
    
  }
  
  p <- p +
    # ylim(0, 100) + # Change Exceednace scale for percentages
    xlim(min(dens$x), max(dens$x)) +
    theme_bw()


  q <- ggplot_build(p)
  #q$data[[1]]$y <- (1-q$data[[1]]$y) * 100 # 1-y to reverse the chart then * 100 to show percentages

  if(compare == TRUE) { # Plot the stat_ecdf line
   # q$data[[2]]$y <- (1-q$data[[2]]$y) * 100
  }

  plt_obj <- ggplot_gtable(q) # reassemble the plot

  # Create a list object containing the data and plot
  lst.dat <- list(plot = plt_obj,
                  data = dat)
  
  if(plot_only == TRUE){
    
    plot(plt_obj)
    return(plt_obj)
    
  } else { # Return list object containing data and plot 
    
    return(lst.dat)
    
  }

}

