# Brian Boyle
# 2022-03-05
# Plot images using truchet tiles

# Source script containing function for creating and plotting truchet tiles
source('./truchet_tiles_function.R')

## Function arguments
# grid_max    number of tiles on each axis (e.g. 5 = 5x5 grid)
# seed        set seed for random tile selection
# all_tiles   T ==  straight and curved tiles, F == curved tiles only
# line_col    line colour
# line_size   line size
# line_alpha  line alpha value
# bg_col      background colour
# line_end    shape at end of each line (butt, square, round)


## Example plots using the function
# Plot 10x10 grid
plot.truchet(grid_max = 10, seed = 16)

# Curved tiles only
plot.truchet(grid_max = 20, seed = 16,
             all_tiles = T,
             line_end = 'round')

# Change colours
plot.truchet(grid_max = 15, seed = 16,
             all_tiles = F,
             line_size = 1,
             line_col = 'white',
             bg_col = 'skyblue4')

# All tile types
plot.truchet(grid_max = 15, seed = 16,
             all_tiles = T,
             line_size = 1,
             line_col = 'white',
             bg_col = 'skyblue4')
