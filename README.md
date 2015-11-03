## imagine
R package for image manipulation (resize, crop, conversion) and animation

##Requirements

You must install ImageMagick or GraphicsMagick for imagine to work

##Installation

library(devtools)

install_github("ampaho/imagine")

##Usage
There is only one function in the package and you can complete most tasks in one line

###Animation using an alias

	imagine("pic*.png", "pic.gif", list(operation="animation", delay=50))
	
###Animation with a vector files

	imagine(c("pic1.png", "pic2.png", "pic20.png"), "pic.gif", list(operation="animation", delay=50))

###Image format conversiom

	imagine("Rlogo.png", "Rlogo.jpg")

###Crop an image
	
	imagine("large_picture.bmp", "cropped_pic.jpg", list(operation="crop", crop.offset=c(50, 150), crop.gravity="NorthEast"))
	
###Resize an image

	imagine("large_picture.bmp", "resize_pic.png", list(operation="resize", with="200", ratio=0.8))
	#could have used height as well, but ratio will keep the image proportion
	
###Rotate an image

	imagine("Rlogo.png", "rotated_Rlogo.bmp", list(operation="flip", rotation=90))
	
##Todo

	* Draw an image using MVG
	* Convert a SVG to any other format (from a an SVG file or inline)

