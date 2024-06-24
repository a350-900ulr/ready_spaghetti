check_route_hits <- \(from, to, hits_what='O', min_resolution=100) {
	#' Given a start and end point, check if the route encounters the block_character.
	#' The resolution determines how many points are drawn between the start & end points,
	#' which means that a returned negative could be a false negative.
	#' This function relies on "track" being in the environment.
	
	# calculate the resolution of the route by
	resolution <- abs(from - to) %>%
		# adding the x & y distances together
		prod() %>%
		# multiplying randomly between 1.5 & 2
		`*`(runif(1, 1.5, 2)) %>%
		# round to integer
		round() %>%
		# make sure its still larger than max resolution
		max(min_resolution)
	
	# generate respective x & y ranges based on the resolution
	range_x <- seq(from[1], to[1], length.out=resolution)
	range_y <- seq(from[2], to[2], length.out=resolution)
	
	# get a logical vector containing any occurences of the block_character along the path
	route_logical <- map2_lgl(
		range_x,
		range_y,
		\(x, y) track[round(x), round(y)] == hits_what
	)
	
	# return if any true value is present in the vector
	return(any(route_logical))
	
}


map_matrix <- \(input_matrix, custom_function, type_function=as.logical) {
	#' maps a logical function to each cell's index in the dimensions of the track matrix
	#' @usage is_G <- map_matrix(\(x_ind, y_ind) track[x_ind, y_ind] == 'G')
	
	outputs <- map2(
		row(input_matrix),
		col(input_matrix),
		\(x_index, y_index) custom_function(x_index, y_index)
	) %>%
		type_function() %>%
		matrix(unlist(.), nrow = nrow(input_matrix), ncol = ncol(input_matrix))
}




plot_logical_matrix <- \(logical_matrix) {
	#' plot a logical matrix as a grid of tiles (heatmap)
	logical_matrix %>%
		as.data.frame() %>%
		# take all calls
		pivot_longer(cols=1:ncol(.), names_to='x', values_to='fill') %>%
		mutate(x=as.numeric(gsub("V", "", x))) %>%
		mutate(y =
			map(
				nrow(track):1,
				\(base_num) (rep(base_num, ncol(track)))
			) %>% flatten_int()
		) %>% ggplot(aes_all(names(.))) +
		geom_tile() +
		scale_fill_manual(values=c("white", "black")) +
		theme_minimal() +
		theme(
			axis.text = element_blank(),
			axis.ticks = element_blank(),
			axis.title = element_blank(),
			legend.title = element_blank()
		) +
		coord_fixed()
}


plot_track <- \(
	with_region = NULL,
	with_region2 = NULL,
	with_path = path_car,
	with_path2,
	title=''
) {
	#' plot a logical matrix onto the current track
	
	matrix_to_df <- \(input_matrix, value_name='value') {
		input_matrix %>%
			# format as table
			as.data.frame(stringsAsFactors=F) %>%
			# create row column in descending order
			mutate(row=nrow(.):1) %>%
			# merge all columns into 1
			pivot_longer(cols=-row, names_to="column", values_to=value_name) %>%
			# convert column row to numeric
			mutate(column=as.numeric(gsub("V", "", column)))
	}
	
	track_df <- track %>% matrix_to_df('type')
	
	if (!is.null(with_region)) {
		track_visible_df <- with_region %>% matrix_to_df('visible')
	}
	
	if (!is.null(with_region2)) {
		track_visible_df2 <- with_region2 %>% matrix_to_df('visible')
	}
	
	if (!is.null(with_path)) {
		# adjust the row values to match the coordinate system of the plot
		with_path$x <- nrow(track) - with_path$x + 1
	}

	if (!is.null(with_path2)) {
		with_path2$x <- nrow(track) - with_path2$x + 1
	}
	# not going straight
	
	track_plot <- ggplot() +
		geom_tile(data=track_df, mapping=aes(x=column, y=row, fill=type), color="black") +
		scale_fill_manual(values = c(
			"O" = "brown",
			"T" = "gray",
			"G" = "green",
			"S" = "yellow",
			"F" = "white"
		)) +
		theme_minimal() +
		theme(
			axis.text = element_blank(),
			axis.ticks = element_blank(),
			axis.title = element_blank(),
			legend.title = element_blank(),
			legend.position = "none",
		) +
		coord_fixed() +
		ggtitle(title)
		
	
	if (!is.null(with_region)) {
		track_plot %<>% `+`(geom_point(
			data = track_visible_df,
			mapping = aes(x=column, y=row, alpha=visible),
			color = '#FF00FF',
			size = 4,
			shape = 1
		))
	}
	
	if (!is.null(with_region2)) {
		track_plot %<>% `+`(geom_point(
			data = track_visible_df2,
			mapping = aes(x=column, y=row, alpha=visible),
			color = 'orange',
			size = 3,
			shape = 18
		))
	}
	
	if (!is.null(with_path)) {
		track_plot %<>% { . +
			# i dont know how x & y got switched up but they did
			geom_point(data=with_path, aes(x=y, y=x), color = "purple", size = 1) +
			geom_path(data=with_path, aes(x=y, y=x), color = "blue", size = 1)
		}
	}
	
	if (!is.null(with_path2)) {
		track_plot %<>% { . +
			geom_point(data=with_path2, aes(x=y, y=x), color = "purple", size = 1) +
			geom_path(data=with_path2, aes(x=y, y=x), color = "blue", size = 1, linetype='dashed')
		}
	}
	
	track_plot
}


get_subset <- \(input_matrix, x_coordinate, y_coordinate, pad=1) {
	#' return a square matrix 'window' with values surrounding a specific coordinate
	input_matrix[
		(x_coordinate-pad):(x_coordinate+pad), # x
		(y_coordinate-pad):(y_coordinate+pad)  # y
	]
}

has_neighbor <- \(
	x_center,
	y_center,
	has_what,
	invisible_from = matrix(F, nrow=nrow(track), ncol=ncol(track))
) {
	#' check if a cell in the matrix has a certain type of neighbor
	#' & is visible in the visibility matrix
	subset_track <- get_subset(track, x_center, y_center)
	subset_visibility <- get_subset(invisible_from, x_center, y_center)
	
	track_subset_elgible <- map_matrix(subset_track, \(x_subset, y_subset) {
		!subset_visibility[x_subset, y_subset] && subset_track[x_subset, y_subset] == has_what
	})
	
	any(track_subset_elgible)
}

# to calculate scores (distance from finish) for each possible move
euclidean_distance <- \(from, to) {
	sqrt((from[1] - to[1])^2 + (from[2] - to[2])^2)
}

save_plot <- \(with_region=NULL, with_region2=NULL, with_path=NULL, with_path2=NULL, tag='') {
	plot_track(
		with_region = with_region,
		with_region2 = with_region2,
		with_path = with_path,
		with_path2 = with_path2,
		title = paste(track_num, 'step', step_counter, ':', tag, '\n', format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
	) %>%
		ggsave(paste0(output_folder, 'step', step_counter, '.png'), ., create.dir = T)
}