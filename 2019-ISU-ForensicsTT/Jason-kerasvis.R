library(reticulate)
library(keras)
use_condaenv("keras", required = T)
# reticulate::conda_install("keras", "keras-vis", pip = T)
# reticulate::conda_install("keras", "tensorflow")

#Changing reticulate to use python 3 
reticulate::use_python(python = '~/anaconda3/envs/keras/bin/python3', required = T)

kerasvis <- import("vis.visualization")
im <- import("vis.input_modifiers")
visutils <- import("vis.utils")
plt <- import("matplotlib.pyplot")
collections <- import("collections")



visualize_filter <- function(model, selected_filters, layer_names){
  jitter <- list(im$Jitter(.05))
  
  #Using python's deque to store the filter images 
  old_images = collections$deque()
  python_index <- 0
  r_index <- 1
  
  #Generating the visualization for all the selected filters in the selected layers 
  for(layer_name in layer_names){
    img = collections$deque()
    old_images$append(img)
    layer_idx <- visutils$utils$find_layer_idx(model, layer_names[[r_index]])
    for(filter_num in selected_filters[[r_index]]){
      old_filter <- kerasvis$visualize_activation(model, layer_idx, 
                                                  filter_indices = filter_num, 
                                                  tv_weight = 0L, 
                                                  input_modifiers = jitter, 
                                                  max_iter = 1L)
      filter_name <- paste(layer_names[r_index], "pre_Filter", filter_num, sep="_")
      plt$axis('off')
      plt$title(filter_name)
      plt$imshow(old_filter)
      plt$savefig(filter_name ,bbox_inches='tight')
      img$append(old_filter)
      
      
    }
    python_index = python_index + 1
    r_index = r_index + 1
  }
  
  #Generating a better visualization for all the selected filters in the selected layers
  # through increased iterations, regularization, and using the old visualization as a starting point. 
  python_index <- 0
  r_index <- 1
  jitter2 <- list(im$Jitter(.05))
  new_images = collections$deque()
  for(layer_name in layer_names){
    temp_layer = old_images$popleft()
    img = collections$deque()
    new_images$append(img)
    layer_idx <- visutils$utils$find_layer_idx(model, layer_names[[r_index]])
    for(filter_num in selected_filters[[r_index]]){
      new_filter <- kerasvis$visualize_activation(model, layer_idx, filter_indices=filter_num, seed_input=temp_layer$popleft() , input_modifiers=jitter2, max_iter=1L)
      filter_name <- paste(layer_names[r_index], "post_Filter", filter_num, sep="_")
      plt$axis('off')
      plt$title(filter_name)
      plt$imshow(new_filter)
      plt$savefig(filter_name ,bbox_inches='tight')
      img$append(new_filter)
      
      
    }
    
    #Generating a image pallete with 4 columns for all the layers 
    stitched <- visutils$utils$stitch_images(new_images[python_index], cols=4L)
    plt$figure(figsize=c(20,30))
    name <- paste(layer_names[r_index], "",  sep="")
    plt$axis('off')
    plt$title(name)
    plt$imshow(stitched)
    plt$savefig(name ,bbox_inches='tight')
    plt$show()
    
    python_index = python_index + 1
    r_index = r_index + 1
    
  }
}


source("~/models/shoe_nn/Generate_Model_Images.R")
model_path <- "~/models/shoe_nn/TrainedModels/"
newest_model <- get_newest(dir = model_path, pattern = "weights.h5")
newest_data_file <- file.path("~/models/shoe_nn/RProcessedImages/", newest_model$process_dir, "cropped_photos.Rdata")
model_wts_file <- file.path(newest_model$path, newest_model$base_file)
loaded_model <- set_weights(model_wts_file)



model <- application_vgg16(weights = 'imagenet', include_top = TRUE)
model <- application_vgg16(weights = 'imagenet', include_top = TRUE)

images = collections$deque()
selected_filters <- list(list(41L))#, 42L), list(5L,200L))
layer_names <- list("block5_conv3")#, "predictions")
visualize_filter(loaded_model, list(list(1L)), layer_names)

visutils$utils$find_layer_idx(loaded_model, "block5_conv3")
