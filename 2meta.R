# Implement a metaheuristic-based approach
# • present June 24, 2024

track_name <- 'tracks/track_02.t'

source('custom_functions.R')
pacman::p_load(
	magrittr, # pipe operators
	tibble,
	ggplot2,
	readr, # write_csv
	purrr, # maps
	dplyr, # mutate
	tidyr # pivot_longer
)

# load track in as a character matrix
track <- track_name %>%
	readLines() %>%
	strsplit('') %>%
	do.call(rbind, .)

# create a logical matrix that holds every position that has been viewed,
# to prevent it from considering those positions again
visible_history <- matrix(F, nrow=nrow(track), ncol=ncol(track))

# get an integer vector of length 2 indicating the car's coordinates
car_position <- which(track == 'S', arr.ind=T) %>% as.integer()
# get a matrix of n x 2 indicating the finish positions,
# where n is the number of finish positions
finish_positions <- which(track == 'F', arr.ind=T)

# Create a matrix with the same dimensions as track indicating every cell currently visible
# from any finish position.
# When mapping over a list of each finish position & combine their resulting matrices
# using logical OR.
visible_fin <- map_matrix(\(x_ind, y_ind) {
	# same thing as before, except for a list of finish positions
	map_lgl(nrow(finish_positions), \(finish_position_index) {
		!check_route_hits(finish_positions[finish_position_index,], c(x_ind, y_ind))
	})
})




## Create initial line segments


# do the same,
# from the cars position
visible_car <- map_matrix(\(x_ind, y_ind) {
	# acquire a logical scalar of each cell's visibility from the car position
	!check_route_hits(car_position, c(x_ind, y_ind), block_character='O')
})





if (!any(overlap <- visible_car & visible_fin)) {
	# when there is no overlap between all visible cells
	# in both from the car's current position & from the finish positions
	
	visible_history %<>% `|`(visible_car) %>% `|`(visible_fin)
	
	
	# Using the visibility matrices, calculate the best candidate position within that
	# contiguous region.
	
	
	
	
	
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













