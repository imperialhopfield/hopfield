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
binary_pattern_t
mapToBinary(double** pattern, int width, int height){
  const int size = width * height;
  int* binaryPattern =(int*) malloc(sizeof(int) * size);
  int i=0;

  for(int w = 0; w < width; w++)
    for(int h = 0; h < height; h++)
    {
      binaryPattern[i] = pattern[w][h] < 0.5 ? 0 : 1;
      i++;
    }

  binary_pattern_t res;
  res.pattern = binaryPattern;
  res.size = size;
  return res;
}


binary_pattern_t
load_picture(char* inputImg)
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
  size_t width = MagickGetImageWidth(mw);
  size_t height = MagickGetImageHeight(mw);
  long y;
  double** outputPattern = (double**) malloc (sizeof(double*) * width);
  for(size_t i=0; i < width; i++)
  {
    outputPattern[i] = (double*) malloc(sizeof(double) * height);
  }

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

int main(int argc, char** args){

  //char* imageInput = "../images/3x3.bmp";
  if(argc == 2 && args[1] != NULL)
  {
    binary_pattern_t finalPattern = load_picture(args[1]);
    for(int i = 0; i < finalPattern.size; i++)
      printf("%d ", finalPattern.pattern[i]);
    printf("\n");
  }
  else
    printf("FREEZE!!! no image input provided!!!\n");


  return 0;
}
