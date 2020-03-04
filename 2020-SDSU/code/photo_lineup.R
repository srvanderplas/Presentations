photo_lineup_prep <- function(imgs) {
  imgs %>%
    shuffle() %>%
    purrr::map(imager::resize, size_x = 256, size_y = 256) %>%
    purrr::map_df(as.data.frame, wide = "c", .id = ".id") %>%
    mutate(.id = as.numeric(.id)) %>%
    mutate(rgb.val = rgb(c.1, c.2, c.3))
}

image_lineup <- function(df, nrow = 4) {
  ggplot(df, aes(x = x, y = y, fill = rgb.val)) + 
    scale_fill_identity() + 
    scale_y_continuous(trans = scales::reverse_trans()) + 
    geom_raster() + 
    facet_wrap(~.id, nrow = nrow) + 
    coord_equal() + 
    theme_grey() + 
    theme(panel.background = element_rect(fill = "white", color = "grey30"),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()
    )
}

shuffle <- function(x) {
  idx <- sample(1:length(x), size = length(x), replace = F)
  x[idx]
}
