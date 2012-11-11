#include "convertImage.h"
#include <wand/magick_wand.h>
#include <stdbool.h>
#include <limits.h>

size_t minH = 10;
size_t maxH = 50;
size_t minW = 10;
size_t maxW = 50;

void ThrowWandException(MagickWand *wand)
  {
    char *description;
    ExceptionType severity;
    description=MagickGetException(wand,&severity);
    (void) fprintf(stderr,"%s %s %lu %s\n",GetMagickModule(),description);
    description=(char *) MagickRelinquishMemory(description);
    exit(-1);
  }

/* converts a list of doubles to binary + flattens the matrix to a vector */
static binary_pattern_t *
mapToBinary(double** pattern, int width, int height){
  binary_pattern_t* binaryPattern = 
    (binary_pattern_t *) malloc(sizeof(binaryPattern));
  const int size = width * height;
  binaryPattern->size = size;
  binaryPattern->pattern = 
    (uint32_t *) malloc(sizeof(binaryPattern->pattern) * size);

  int i=0;

  for(int w = 0; w < width; w++)
    for(int h = 0; h < height; h++)
    {
      binaryPattern->pattern[i] = pattern[w][h] < 0.5 ? 0 : 1;
      i++;
    }

  return binaryPattern;
}

/* loads an image into a magic wand */
MagickWand*
load_into_wand(char* inputImg)
{
   /* load a picture in the "wand" */
  MagickWand *mw = NULL;

  MagickWandGenesis();

  /* Create a wand */
  mw = NewMagickWand();

  /* Read the input image */
 MagickBooleanType retVal = MagickReadImage(mw, inputImg);

   if (retVal == MagickFalse){
     ThrowWandException(mw);
     return NULL;
   }
    
  return mw;  
}

/* takes a magic wand object and resizes it */
static bool
resize(MagickWand* imgWand, size_t width, size_t height)
{
  return MagickResizeImage(imgWand, height, width, LanczosFilter, 0);
}

/* resizes a list of images to the minimum of the widths and 
   heights across the images. Furthermore, if the values are
   not in the range min, max, then we shall scale them so that
   they fit in the range as well */
void
resizeToMin(MagickWand** imgWands, int nr)
{
  size_t imgMinW = INT_MAX, imgMinH = INT_MAX; 

  /* calculate absolute min width and height for the images */
  for(int i = 0; i < nr; i++)
  {
    if (MagickGetImageWidth(imgWands[i]) < imgMinW)
      imgMinW = MagickGetImageWidth(imgWands[i]); 
    if (MagickGetImageHeight(imgWands[i]) < imgMinH)
      imgMinH = MagickGetImageWidth(imgWands[i]); 
  }

  /*if the min values don't fit in the range, trim down even more*/
  /* rescale width */ 
 if(imgMinW > maxW) 
    imgMinW = maxW;
  if(imgMinW < minW) 
    imgMinW = minW;

  /* rescale height */
  if(imgMinH > maxH) 
    imgMinH = maxH;
  if(imgMinH < minH) 
    imgMinH = minH;

  for(int i = 0; i < nr; i++)
  {
    resize(imgWands[i], imgMinW, imgMinH);
  }
}



/* converts a magic wand to a binary pattern. it first converts it to a
   list of doubles, and then used the helper function mapToBinary in 
   order to convert the doubles to binary */
binary_pattern_t*
mwToBinary(MagickWand* imgWand)
{
  PixelWand** pixels;
  PixelIterator* pixelIt = NewPixelIterator(imgWand);
 
  size_t width  = MagickGetImageWidth(imgWand); 
  size_t height = MagickGetImageHeight(imgWand); 
  double** outputPattern = (double**) malloc (sizeof(double*) * width);
  for(size_t i=0; i < width; i++)
  {
    outputPattern[i] = (double*) malloc(sizeof(double) * height);
  }

  long y;
  /* get pixel grayscale values */
  for (y=0; y < (long) height; y++)
  {
    pixels=PixelGetNextIteratorRow(pixelIt,&width);
    for (long x=0; x < (long) width; x++) {
      outputPattern[x][y] = (PixelGetRed(pixels[x]) +
        PixelGetGreen(pixels[x]) + PixelGetBlue(pixels[x]))/3;
    }
  }

  /* Tidy up */
  if(imgWand) 
    imgWand = DestroyMagickWand(imgWand);

  //MagickWandTerminus();

  return mapToBinary(outputPattern, width, height);

}


/* loads a list of images located at filepaths, and returns a list of 
   patterns that will be loaded in the Hopfield network */
binary_pattern_t**
image_to_binary (char** filepaths, int nr_images)
{
  MagickWand** wand_list = malloc (sizeof(MagickWand*) * nr_images);
  /* load images into wands  */
  for(int i = 0; i < nr_images; i++)
  {
    wand_list[i] = load_into_wand(filepaths[i]);
  }

  /* resize the images */
  resizeToMin(wand_list, nr_images); 
 
  binary_pattern_t** binary_patterns = 
    (binary_pattern_t**) malloc (sizeof(binary_pattern_t*) * nr_images);
  /* convert to patterns */
  for(int i = 0; i < nr_images; i++)
  {
    binary_patterns[i] = mwToBinary(wand_list[i]);
  }

  return binary_patterns; 

}


