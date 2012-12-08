#include "convertImage.h"
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
  binary_pattern_t* binaryPattern = (binary_pattern_t *) malloc(sizeof(binaryPattern));
  const int size = width * height;
  binaryPattern->size = size;
  binaryPattern->pattern = (uint32_t *) malloc(sizeof(binaryPattern->pattern) * size);

  int i=0;

  for(int w = 0; w < width; w++)
    for(int h = 0; h < height; h++)
    {
      binaryPattern->pattern[i] = pattern[w][h] < 0.5 ? 0 : 1;
      i++;
    }

  return binaryPattern;
}

// TODO write function which takes all the traning images
// and gets min size for all
//  check how it will work with very different sizes

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
  PixelIterator* pixelIt = NewPixelIterator(mw);
  // size_t width = MagickGetImageWidth(mw);
  // size_t height = MagickGetImageHeight(mw);
  MagickResizeImage(mw, height, width, LanczosFilter, 0);
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

  return mapToBinary(outputPattern, width, height);

  /* print the vector */
  for (y=0; y < (long) height; y++){
    for (long x=0; x < (long) width; x++)
      printf("%f ", outputPattern[x][y]);

    printf("\n\n\n");
  }

  /* Tidy up */
  if(mw) mw = DestroyMagickWand(mw);

  MagickWandTerminus();
}
