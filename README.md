# Brightfield Image Cellularity

### Introduction
Cellularity is a measure of general cell population. This is measured as the percentage of a brightfield microscopy image that is covered by cells. This program loads brightfield images, segments them into cellular and acellular area and then calculates the percentage area covered by cells.

This program was primerily written for the use of a research group which use Leica microscopes which output `.lif` formats which contain a number of images per file. However, the program is also compatible with individual `.png`, `.tif`, `.tiff`, `.jpeg`, and `.jpg` file formats.

Images are normalised, before edge detection, and blurring. Then a cutoff is applied to remove the background, and all small objects (smaller than a cell) are removed. Blurring and artifacts of imaging can sometimes cause the program to overestimate cellular area. Therefore, a factor of the circumference of cellular area is subtracted from the final cellularity. 

When compared to humans on 99 brightfield images, the model had a mean square error of 6.97 on default settings

N.B. This Git repo is a 'clean' version of a private repo, so sometimes I may have referred to files which do not exist. Removed files pertain to images 
which I don't have permission to make public. This is also why there are no branches in the git history.

I will also be adding a code of conduct in the future

### How the cellularity algorithm it works
#### Normalisation
Due to the nature of the microscope, the background brighness can often change throughout an image creating a brightness gradient. Therefore, the first step is to perform 3D linear regression on the image to find the gradient and account for it. This is done multiple times since it doesn't always remove all of the gradient the first time.
Ways in which I would like to improve this step are firstly, by using non-linear regression here, and secondly, the gradient is currently accounted for by subtraction. I would like to change this to a multiplicative method.

#### Edge detection
The normalised image is then put through a sobel edge detection kernel to find the edges. Cellular areas have lots of edges in due to organelles and cell membranes etc., producing bright areas on the edge detected image. Acellular areas are generally smooth and hence produce dark areas on the edge detected image.
Another artifact of microscopy is the horizontal lines running accross the images. In order to stop the algorithm detecting these as cellular area so much, I weighted the horizontal edges twices as bright as the vertical edges, reducing this effect.
Sometimes some images are inherently smoother than others where it's more difficult to distinguish between cellular and acellular area. I mitigated against this by brightening edge detected images with a low mean birghtness. I would like to improve on this logic since this doesn't always work and it sometimes brightens images with low cellularity.

#### Cutoff
At this point, a cutoff is applied to round bright areas to 1 (cellular) and dark areas to 0 (acellular). This cutoff was chosen by running on a number of images, and trying to get the most accurate segmented image, along with altering blur and the brightness to which smooth images are increased. Gridsearch was useful to find minima. I attempted stochastic gradient descent but human alteration proved more useful since I could understand how the image will react. 

#### Shrink and calculating 
Due to blurring, the cellular areas extend slightly beyond where they are on the original image. Therefore, I applied a similar blur again to the segmented image, and only took the brightest areas, thereby removing a layer of cellular area along the circumferance of the cellular area.
Then cellularity is calculated by calculating the percentage cellular area out of the total image area, and outputted in the file "cellularities.csv" in the output file

#### By grid
Cellularity can also be calculated on a localised level by separating the image into a grid. Then cellularity is calculated for each grid segment. Then each image gets its own cellularity .csv file with all the cellularities in. The cellularities are placed in the .csv file as they were in the image (e.g. the cellularity for the top right grid segment will be fount in the top right cell).
