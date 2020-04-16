library(tidyverse)
library(EBImage)
library(ggpcp)

fix_names <- function(x) {
  if (str_detect(x, "[rgb][A-Z]{3}")) {

    pat <- str_extract(x, "[rgb][A-Z]{3}")
    csp <- str_extract(pat, "[A-Z]{3}")
    o1 <- str_extract(pat, "^[rgb]") %>% factor(levels = c("r", "g", "b"), ordered = T) %>% as.numeric()
    o2 <- str_extract_all(pat, "[A-Z]", simplify = T) %>% str_to_lower() %>% magrittr::extract(o1)
    str_replace(x, pat, paste0(o2, csp))
  } else {
    x
  }
}

features <- read_csv("data/ved-data-preprocessed/features.csv") %>%
  set_names(map_chr(names(.), fix_names)) %>%
  mutate(flavor_orig = str_remove(type, "_\\d{1,}") %>% str_remove(" Case")) %>%
  mutate(flavor = str_replace(flavor_orig, "Jewel (.*)", "\\1 (Jewel)") %>%
           str_replace(., "Sunkist\\(R\\) (.*)", "\\1 (Sunkist)") %>%
           str_replace(., "A&W\\(R\\) (.*)", "\\1 (A&W)")) %>%
  mutate(flavor = factor(flavor)) %>%
  group_by(flavor) %>%
  mutate(rgb = rgb(mean(r_mean), mean(g_mean), mean(b_mean), maxColorValue = 255))
#
# head(features)
#
# features <- features %>% mutate(rgb = rgb(r_mean, g_mean, b_mean, maxColorValue = 255))
#
#
# features %>%
#   ggplot(aes(color = rgb)) + geom_pcp(aes(vars = vars(r_mean, r_std, r_ratio, matches("cluster_\\d_r")))) +
#   scale_color_identity(guide = F) +
#   theme_bw() +
#   facet_wrap(~flavor)
#
# features %>%
#   ggplot(aes(color = rgb)) + geom_pcp(aes(vars = vars(g_mean, g_std, g_ratio, matches("cluster_\\d_g")))) +
#   scale_color_identity(guide = F) +
#   theme_bw() +
#   facet_wrap(~flavor)
#
# features %>%
#   ggplot(aes(color = rgb)) + geom_pcp(aes(vars = vars(b_mean, b_std, b_ratio, matches("cluster_\\d_b")))) +
#   scale_color_identity(guide = F) +
#   theme_bw() +
#   facet_wrap(~flavor)
#
# features %>%
#   ggplot(aes(color = rgb)) +
#   geom_pcp(aes(vars = vars(r_mean, r_std, r_ratio, matches("cluster_\\d_r"),
#                            g_mean, g_std, g_ratio, matches("cluster_\\d_g"),
#                            b_mean, b_std, b_ratio, matches("cluster_\\d_b"),
#                            bg_ratio, br_ratio, gr_ratio, bgsd_ratio, brsd_ratio, grsd_ratio))) +
#   scale_color_identity(guide = F) +
#   theme_bw() +
#   facet_wrap(~flavor)
#
# features %>%
#   ggplot(aes(color = rgb)) +
#   geom_pcp(aes(vars = vars(matches("_mean")))) +
#   scale_color_identity(guide = F) +
#   theme_bw() +
#   facet_wrap(~flavor)
#
# features %>%
#   ggplot(aes(color = rgb,vars = vars(flavor, r_std, g_std, b_std,
#                                      flavor, h_std, s_std, v_std,
#                                      flavor, y_std, u_std, v_std1, flavor))) +
#   # geom_pcp_box(boxwidth = 0.1, fill="grey70") +
#   geom_pcp(alpha = .2) +
#   scale_color_identity(guide = F) +
#   theme_bw()
#
# features %>%
#   ggplot(aes(color = rgb)) + geom_pcp(aes(vars = vars(h_mean, h_std, h_ratio, matches("cluster_\\d_h")))) +
#   scale_color_identity(guide = F) +
#   theme_bw() +
#   facet_wrap(~flavor)
#



get_img_features <- function(path) {
  im <- EBImage::readImage(path)

  black_areas <- apply(im == 0, 1:2, function(x) max(x)) %>% EBImage::as.Image()
  white_areas <- apply(im == 1, 1:2, function(x) min(x)) %>% EBImage::as.Image()
  useful <- which((black_areas + white_areas) == 0)
  useful_px <- as.character(as.raster(im))[useful]

  rgb <- dplyr::bind_cols(tibble::tibble(hex = useful_px), col2rgb(useful_px) %>% t() %>% tibble::as_tibble())

  stdize <- function(x) mean(x)/sd(x)
  px_features <- rgb %>%
    dplyr::rename(r = red, b = blue, g = green) %>%
    dplyr::summarize_at(dplyr::vars(r, g, b), .funs = dplyr::funs(oa=mean(.), osd = sd(.), oz = stdize(.))) %>%
    dplyr::mutate(
      oa_rg = r_oa/g_oa,
      oa_rb = r_oa/b_oa,
      oa_gb = g_oa/b_oa,
      osd_rg = r_osd/g_osd,
      osd_rb = r_osd/b_osd,
      osd_gb = g_osd/b_osd
    )
  k <- 6
  km <- stats::kmeans(rgb[,2:4], k, nstart = 4)
  km_ct <- table(km$cluster)
  km_ct <- km_ct[order(km_ct, decreasing = T)]/sum(km_ct)
  km_pct <- km_ct %>% as.numeric() %>% t() %>% as.data.frame()  %>% magrittr::set_colnames(paste0("km_pct", 1:k))
  km_features <- km$centers[order(table(km$cluster), decreasing = T),] %>%
    tibble::as_tibble() %>% magrittr::set_names(c("r", "g", "b")) %>%
    dplyr::mutate(cl = paste0("kmcenter", 1:k)) %>%
    tidyr::pivot_longer(r:b) %>%
    dplyr::mutate(name = paste(cl, name, sep = "_")) %>%
    dplyr::bind_rows(tibble::tibble(cl = NA, name = paste0("kmwithinss", 1:k),
                                    value = km$withinss/km$tot.withinss)) %>%
    dplyr::select(-cl) %>%
    tidyr::pivot_wider(names_from = "name") %>%
    dplyr::mutate(kmbwss = km$betweenss/km$totss) %>%
    dplyr::bind_cols(km_pct)

  tibble::tibble(path = path) %>%
    dplyr::mutate(
      flavor = basename(path) %>%
        stringr::str_extract("^[A-z\\(\\)\\.]{1,}") %>%
        stringr::str_remove(" Case") %>%
        stringr::str_remove("\\(R\\)"),
      idnum = basename(path) %>% stringr::str_extract("\\d{1,}") %>%
        as.integer(),
      rgb = list(rgb),
      overall_features = list(px_features),
      km_features = list(km_features))
}


library(randomForest)

rf_test_accuracy <- function(rf_model, test_set) {
  pred <- predict(rf_model, select(test_set, -one_of("flavor", "idx")))
  act <- test_set$flavor
  100*(mean(pred == act))
}

plot_conf_matrix <- function(cmat) {
  idx <- 1:nrow(cmat)
  dendro_dist <- hclust(as.dist(1 - cmat[,idx]/rowSums(cmat[,idx])), method = "ward.D2")

  f_ord <- dendro_dist$labels[dendro_dist$order]

  confusion_long <- cmat %>% as_tibble() %>%
    select(idx) %>%
    mutate(n = rowSums(.)) %>%
    mutate_each(~./n) %>%
    mutate(actual = names(.)[idx]) %>%
    select(actual, everything()) %>%
    tidyr::pivot_longer(cols = all_of(idx + 1), names_to = "predicted") %>%
    mutate(actual = factor(actual, levels = f_ord, ordered = T),
           predicted = factor(predicted, levels = rev(f_ord), ordered = T))

  ggplot(confusion_long, aes(x = predicted, y = actual, fill = value)) +
    geom_tile() +
    coord_equal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

}
imlist <- list.files("data/ved-data-preprocessed/Split_Jellybeans/", pattern = "*.png", full.names = T)

# system.time((imdata <- imlist[1:100] %>% purrr::map_df(get_img_features)))
#
# future::plan("multicore")
# imdata <- furrr::future_map_dfr(imlist, get_img_features)


imdata <- parallel::mclapply(imlist, get_img_features, mc.cores = 14)

# save(imdata, file = "data/imdata.rda")
load("data/imdata.rda")

imdata <- do.call(bind_rows, imdata)
imdata <- imdata %>%
  mutate(flavor = basename(path) %>%
           str_remove("(\\(R\\))?_\\d{1,}\\.png") %>%
           str_remove("\\((R|TM)\\)") %>%
           str_remove(" Case") %>%
           str_replace("S_mores", "Smores") %>%
           str_replace("_", "&") %>%
           str_remove("A&W ") %>% str_remove("Sunkist "))
imflav <- unique(imdata$flavor)
flavor_list <- list.files("data/all_flavors/") %>%
  str_replace_all("_", " ") %>%
  str_remove("( Case)?.png") %>%
  str_remove("A&W ") %>%
  str_replace("Cappuchino", "Cappuccino") %>%
  str_replace("Daquiri", "Daiquiri") %>%
  str_replace("Tutti Fruitti", "Tutti-Fruitti")

model_data <- filter(imdata, flavor %in% flavor_list) %>%
  ungroup() %>%
  mutate(idx = 1:n()) %>%
  mutate(flavor = factor(flavor)) %>%
  group_by(flavor)

overall_data <- model_data %>%
  select(idx, flavor, overall_features) %>%
  unnest(c(overall_features)) %>%
  ungroup()

km_data <- model_data %>%
  select(idx, flavor, km_features) %>%
  unnest(c(km_features)) %>%
  ungroup()

combined_data <- model_data %>%
  select(idx, flavor, overall_features, km_features) %>%
  unnest(c(overall_features, km_features)) %>%
  ungroup()

my_rf_model <- function(training, test) {
  training <- training %>% select(-idx)
  test <- test %>% select(-idx)
  randomForest(x = training[,-1],
               y = training$flavor,
               xtest = test[,-1],
               ytest = test$flavor,
               strata = training$flavor, keep.forest = T)

}

fit_rf_models <- function(split_fcn = sample_frac, ...) {
  md <- model_data %>% dplyr::select(idx, flavor)
  test_set <- split_fcn(md, ...) %>% ungroup() %>% select(-flavor)
  training_set <- anti_join(md, test_set, by = "idx") %>% ungroup() %>% select(-flavor)

  overall_model <- my_rf_model(training = semi_join(overall_data, training_set, by = c("idx")),
                               test = semi_join(overall_data, test_set, by = c("idx")))

  km_model <- my_rf_model(training = semi_join(km_data, training_set, by = c("idx")),
                          test = semi_join(km_data, test_set, by = c("idx")))

  combined_model <- my_rf_model(training = semi_join(combined_data, training_set, by = c("idx")),
                                test = semi_join(combined_data, test_set, by = c("idx")))

  tibble(test_set_rows = list(test_set$idx),
         overall_test_accuracy = rf_test_accuracy(overall_model, semi_join(overall_data, test_set, by = c("idx"))),
         km_test_accuracy = rf_test_accuracy(km_model, semi_join(km_data, test_set, by = c("idx"))),
         combined_test_accuracy = rf_test_accuracy(combined_model, semi_join(combined_data, test_set, by = c("idx"))),
         overall_model = list(overall_model),
         km_model = list(km_model),
         combined_model = list(combined_model))

}

szs <- tibble(fcn_name = c("frac", "n", "frac", "n"),
              split_fcn = c(sample_frac, sample_n, sample_frac, sample_n),
              sz = c("small", "small", "med", "med"), size = c(.1, 2, .2, 4))

long_res <- crossing(iter = 1:100, fcn_name = c("frac"), sz = c("small")) %>%
  left_join(szs) %>%
  select(-fcn_name, -sz) %>%
  mutate(res = pmap(., fit_rf_models)) %>%
  unnest(res)

# qplot(long_res$overall_test_accuracy, fill = factor(long_res$size), geom = "density", alpha = I(0.5))

# long_res$km_model[[1]]$confusion

long_res <- long_res %>%
  mutate(km_confusion = purrr::map(km_model, pluck, "confusion"),
         overall_confusion = purrr::map(overall_model, pluck, "confusion"),
         combined_confusion = purrr::map(combined_model, pluck, "confusion"))

average_km_confusion <- long_res$km_confusion %>% abind::abind(along = 3) %>%
  apply(., 1:2, mean)
average_overall_confusion <- long_res$overall_confusion %>% abind::abind(along = 3) %>%
  apply(., 1:2, mean)
average_combined_confusion <- long_res$combined_confusion %>% abind::abind(along = 3) %>%
  apply(., 1:2, mean)

tmp <- randomForest(data = combined_data[,-1], flavor ~ ., strata = combined_data$flavor, keep.forest = T)

pal <- c("#FFFFFF", RColorBrewer::brewer.pal(9, "Greens"), "#000000")

save(model_data, combined_data, km_data, overall_data, file = "data/rf_model_data.rda")
save(average_km_confusion, average_overall_confusion, average_combined_confusion, tmp, file = "data/rf_res.rda")

plot_conf_matrix(average_km_confusion) +
  scale_fill_gradientn("Accuracy", colours = pal,
                       values = c(0, .01, .025, .05, .1, .2, .4, .6, .8, .9, 1),
                       breaks = c(.025, .1, .3, .5, .7, .9)) +
  geom_tile(color = "grey") +
  ggtitle(sprintf("Average over 200 K-Means based models (Test Set Accuracy: %.1f%%)",
                  mean(long_res$km_test_accuracy)))

plot_conf_matrix(average_overall_confusion) +
  scale_fill_gradientn("Accuracy", colours = pal,
                       values = c(0, .01, .025, .05, .1, .2, .4, .6, .8, .9, 1),
                       breaks = c(.025, .1, .3, .5, .7, .9)) +
  geom_tile(color = "grey") +
  ggtitle(sprintf("Overall model: Average test set confusion matrix (200 training/test splits) \n(Test Set Accuracy: %.1f%%)",
                  mean(long_res$overall_test_accuracy)))


plot_conf_matrix(average_combined_confusion) +
  scale_fill_gradientn("Accuracy", colours = pal,
                       values = c(0, .01, .025, .05, .1, .2, .4, .6, .8, .9, 1),
                       breaks = c(.025, .1, .3, .5, .7, .9)) +
  geom_tile(color = "grey") +
  ggtitle(sprintf("Average over 200 Combined (K-Means + Overall) features model\n(Test Set Accuracy: %.1f%%)",
                  mean(long_res$combined_test_accuracy)))


simplified_conf_matrix <- function(cmat) {
  idx <- 1:nrow(cmat)
  dendro_dist <- hclust(as.dist(1 - cmat[,idx]/rowSums(cmat[,idx])), method = "ward.D2")

  f_ord <- dendro_dist$labels[dendro_dist$order]

  confusion_long <- cmat %>% as_tibble() %>%
    select(idx) %>%
    mutate(n = rowSums(.)) %>%
    mutate_each(~./n) %>%
    mutate(actual = names(.)[idx]) %>%
    select(actual, everything()) %>%
    tidyr::pivot_longer(cols = all_of(idx + 1), names_to = "predicted") %>%
    mutate(actual = factor(actual, levels = f_ord, ordered = T),
           predicted = factor(predicted, levels = rev(f_ord), ordered = T))

  perfect_class <- filter(confusion_long, as.character(actual) == as.character(predicted)) %>% filter(value == 1)

  ggplot(confusion_long, aes(x = predicted, y = actual, fill = value)) +
    geom_tile() +
    coord_equal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

}
