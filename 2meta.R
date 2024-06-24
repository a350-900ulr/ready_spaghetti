# Implement a metaheuristic-based approach
# • present June 24, 2024

#rm(list=ls())

track_name <- 'track_01.t'

pacman::p_load(
	magrittr, # pipe operators
	tibble,
	ggplot2,
	readr, # write_csv
	purrr, # maps
	dplyr, # mutate
	tidyr # pivot_longer
)

# load track in
track <- track_name %>%
	readLines() %>%
	strsplit('') %>%
	do.call(rbind, .)

# create a logical matrix that holds every position the car has viewed,
# to prevent it from considering those positions again
track_seen <- matrix(F, nrow=nrow(track), ncol=ncol(track))

# get positions
car_position <- which(track == 'S', arr.ind=T) %>% as.integer()
finish_positions <- which(track == 'F', arr.ind=T)

# mark starting position on track as traversed
track_seen[car_position[1], car_position[2]] <- T

## Create initial line segments

check_route_hits <- \(from, to, block_character='O', resolution=100) {
	#' given a start and end point, check if the route encounters the block_character
	# relies on "track" being in the global environment
	range_x <- seq(from[1], to[1], length.out=resolution)
	range_y <- seq(from[2], to[2], length.out=resolution)

	route_logical <- map2_lgl(
		range_x,
		range_y,
		\(x, y) track[round(x), round(y)] == block_character
	)

	return(any(route_logical))

}

map_matrix <- \(func) {
	#' maps a logical function to each cell's index in the dimensions of the track matrix
	map2(row(track), col(track), \(x_index, y_index) { func(x_index, y_index) }) %>%
		as.logical() %>%
		matrix(as.logical(unlist(.)), nrow = nrow(track), ncol = ncol(track))
}

visible_car <- map_matrix(\(x_ind, y_ind) {
	# acquire a logical scalar of each cell's visibility from the car position
	!check_route_hits(car_position, c(x_ind, y_ind), block_character='O')
})

visible_fin <- map_matrix(\(x_ind, y_ind) {
	# same thing as before, except for a list of finish positions
	map_lgl(nrow(finish_positions), \(finish_position_index) {
		!check_route_hits(finish_positions[finish_position_index,], c(x_ind, y_ind))
	})
})

plot_logical_matrix <- \(logical_matrix) {
	#' plot a logical matrix
	logical_matrix %>%
		as.data.frame() %>%
		pivot_longer(cols=1:ncol(.), names_to='x', values_to="fill") %>%
		mutate(x=as.numeric(gsub("V", "", x))) %>%
		mutate(y =
			map(
				(ncol(track)/2):1,
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

plot_logical_matrix(visible_car)

track_seen <- visible_car & visible_fin

if (!any(overlap <- visible_car & visible_fin)) {
	# when there is no overlap between all visible cells
	# in both from the car's current position & from the finish positions
	
	# calculate next 2 line segments coming from the car & the finish
	
	
	
} else {
	# repeat! (TODO: probably wrap in while loop)
	
	
}







### plotting

plot_region <- \(visible_matrix) {
	matrix_to_df <- \(matrix, value_name='value') {
		matrix %>%
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
	
	track_visible_df <- visible_matrix %>% matrix_to_df('visible')
	
	# Define the color mapping
	color_mapping <- c(
		"O" = "brown",
		"T" = "gray",
		"G" = "green",
		"S" = "yellow",
		"F" = "white"
	)
	
	ggplot() +
		geom_tile(data=track_df, mapping=aes(x=column, y=row, fill=type), color="black") +
		scale_fill_manual(values = color_mapping) +
		geom_point(
			data = track_visible_df,
			mapping = aes(x=column, y=row, color=ifelse(visible, "green", "red")),
			size = 3,
			alpha = .5
		) +
		theme_minimal() +
		theme(
			axis.text = element_blank(),
			axis.ticks = element_blank(),
			axis.title = element_blank(),
			legend.title = element_blank()
		) +
		coord_fixed()
}

plot_region(convergence)













