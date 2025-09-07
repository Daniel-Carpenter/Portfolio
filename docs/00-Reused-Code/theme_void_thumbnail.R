# theme_void_thumbnail()

library(ggplot2)

theme_void_thumbnail <- function() {

  list(

    # No axis
    theme_void(),

    # No labels
    labs(
      x = '',
      y = '',
      title = '',
      subtitle = '',
      caption = '',
    ),

    # No legend and small margin
    theme(
      legend.position = 'none',
      plot.margin = grid::unit(c(0, 5, 0, 5), "pt")  # top, right, bottom, left
    )
  )
}


ggsaveToPNGThumbnail <- function(plot_void, filename, scaled_by = 2) {
  ggsaveToPNG(
    plot_void,
    filename,
    outWidth = 750,
    outHeight = 500,
    SCALED_BY = scaled_by
  )
}
