#include "convertImage.h"
#include <assert.h>
#include <wand/magick_wand.h>

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
binary_pattern_t *
mapToBinary(double** pattern, int width, int height){
  binary_pattern_t* binaryPattern = (binary_pattern_t *) malloc(sizeof(*binaryPattern));
  const int size = width * height;
  binaryPattern->size = size;
  binaryPattern->pattern = (uint32_t *) malloc(sizeof(*binaryPattern->pattern) * size);

  int i=0;

  for(int w = 0; w < width; w++)
    for(int h = 0; h < height; h++)
    {
      binaryPattern->pattern[i] = pattern[w][h] < 0.5 ? 0 : 1;
      i++;
    }

  return binaryPattern;
}

/* loads a picture from the inputImg filepath, scales it to the specified 
   width and height, and then converts it to a binary pattern, usable by 
   the Hopfield Network */
binary_pattern_t *
load_picture(char* inputImg, size_t width, size_t height)
{
  /* load a picture in the "wand" */
  MagickWand *mw = NULL;

  MagickWandGenesis();

  /* Create a wand */
  mw = NewMagickWand();

  /* Read the input image */
 MagickBooleanType retVal = MagickReadImage(mw, inputImg);

   if (retVal == MagickFalse)
     ThrowWandException(mw);

  PixelWand** pixels;
  /* rescale the image */
  int resizeSuccess = MagickResizeImage(mw, height, width, LanczosFilter, 0);

  if (!resizeSuccess)
  {
    printf("resize failed\n");
    exit(1);
  }

  PixelIterator* pixelIt = NewPixelIterator(mw);
  double** outputPattern = (double**) malloc (sizeof(double*) * width);
  for(size_t i=0; i < width; i++)
  {
    outputPattern[i] = (double*) malloc(sizeof(double) * height);
  }

  long y;
  /* get pixel grayscale values */
  for (y=0; y < (long) height; y++)
  {
    size_t iter_width;
    pixels=PixelGetNextIteratorRow(pixelIt,&iter_width);
    assert (iter_width == width);
    for (long x=0; x < (long) iter_width; x++) {
      outputPattern[x][y] = (PixelGetRed(pixels[x]) +
        PixelGetGreen(pixels[x]) + PixelGetBlue(pixels[x]))/3;
    }
  }

  /* Tidy up */
  if(mw) mw = DestroyMagickWand(mw);

  MagickWandTerminus();
  
  /* since outputPattern is a list of doubles, convert it to a list of
     binary values */
  return mapToBinary(outputPattern, width, height);
}
