source("http://bioconductor.org/biocLite.R")
biocLite()
biocLite("EBImage")

library(EBImage)

# Reading Image:The readImage function is used to read Image from file location or URLs

Image = readImage("E:/Unstructure Data - Batch 2B/Data Set/Pic.jpg")

# Displaying Image: Image can be displayed using display function and pixel intensities should range from 0 (black) to 1 (white)

display (Image)

# Image Properties: Images are stored as multi-dimensional arrays containing the pixel intensities, 
# so if we use the print function it will return the image properties. In the below output two 
# section is present i.e. summary and array of pixels.

print(Image)

# Adjusting Brightness

Image1 = Image + 0.2
Image2 = Image - 0.2
display (Image1); display(Image2)

# Adjusting Contrast

Image3 = Image * 0.5
Image4 = Image * 2
display(Image3); display(Image4)

# Cropping Image

Image5 = Image[1:800, 1:200,]

display(Image5)

# Spatial Transformation: Spatial image transformations can be performed with the functions 
# such as re-size, rotate, translate and the functions flip and flop to reflect images.

#Resizing the image
Image6 <- resize(Image, 500) 

display(Image6)

# FLip and Flop of image
Image7 <- flip(Image)

display(Image7)

Image8 = flop(Image)

display(Image8)

#Image Rotation
Image9 <- translate(rotate(Image, 90), c(50, 0))

display(Image9)

# Color Management

# The function colorMode can access and change the value of the slot colormode, 
# altering the rendering mode of an image. In this, the Color image (Image) with one 
# frame is changed into a Grayscale, corresponding to the red, green and blue channels.

#Greyscale Image

colorMode(Image) = Grayscale

display(Image)

# Image Filtering: Images can be linearly filtered using filter2. 
# Low-pass filtering is used to perform blur image, remove noise, etc. 
# High-pass filtering is used to perform detect edges, sharpen images etc., 
# various filter shapes can be generated using makeBrush.


#LOW-PASS Filtering image
flo = makeBrush(21, shape='disc', step=FALSE)^2
flo = flo/sum(flo)
imgflo = filter2(Image, flo)
display(imgflo)

#HIGH-PASS Filtering image
fhi = matrix(1, nc=3, nr=3)
fhi[2,2] = -8
imgfhi = filter2(Image, fhi)
display(imgfhi)















