check_route_hits <- \(from, to, block_character='O', min_resolution=50) {
	#' Given a start and end point, check if the route encounters the block_character.
	#' The resolution determines how many points are drawn between the start & end points,
	#' which means that a returned negative could be a false negative.
	#' This function relies on "track" being in the environment.
	
	# calculate the resolution of the route by
	resolution <- abs(from - to) %>%
		# adding the x & y distances together
		sum() %>%
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
		\(x, y) track[round(x), round(y)] == block_character
	)
	
	# return if any true value is present in the vector
	return(any(route_logical))
	
}


map_matrix <- \(func) {
	#' maps a logical function to each cell's index in the dimensions of the track matrix
	#' @usage is_G <- map_matrix(\(x_ind, y_ind) track[x_ind, y_ind] == 'G')
	
	map2(
		row(track),
		col(track),
		\(x_index, y_index) {
			func(x_index, y_index)
		}
	) %>%
		as.logical() %>%
		matrix(unlist(.), nrow = nrow(track), ncol = ncol(track)) -> test
}




plot_logical_matrix <- \(logical_matrix) {
	#' plot a logical matrix as a grid of tiles (heatmap)
	logical_matrix %>%
		as.data.frame() %>%
		# take all calls
		pivot_longer(cols=1:ncol(.), names_to='x', values_to="fill") %>%
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




