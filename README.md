# Brightfield Image Cellularity

### Introduction
Cellularity is a measure of general cell population. This is measured as the percentage of a brightfield microscopy image that is covered by cells. This program loads brightfield images, segments them into cellular and acellular area and then calculates the percentage area covered by cells.

This program was primerily written for the use of a research group which use Leica microscopes which output `.lif` formats which contain a number of images per file. However, the program is also compatible with individual `.png`, `.tif`, `.tiff`, `.jpeg`, and `.jpg` file formats.

Images are normalised, before edge detection, and blurring. Then a cutoff is applied to remove the background, and all small objects (smaller than a cell) are removed. Blurring and artifacts of imaging can sometimes cause the program to overestimate cellular area. Therefore, a factor of the circumference of cellular area is subtracted from the final cellularity. 

When compared to humans on 99 brightfield images, the model had an error of 
N.B. This Git repo is a 'clean' version of a private repo, so sometimes I may have referred to files which do not exist. Removed files pertain to images 
which I don't have permission to make public. This is also why there are no branches in the git history.

I will also be adding a code of conduct in the future
