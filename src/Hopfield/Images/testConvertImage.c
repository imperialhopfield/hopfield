#include <stdlib.h>
#include <stdio.h>
#include "convertImage.h"


int main(int argc, char** argv){

  //char* imageInput = "../images/3x3.bmp";
  if (argc == 2 && argv[1] != NULL)
  {
    const int width = 40;
    const int height = 40;

    binary_pattern_t* p = load_picture(argv[1], width, height);

    for (int j = 0; j < argc - 1; j++)
    {
      printf("pattern (%d x %d):", width, height);
      for (uint32_t i = 0; i < height; i++)
      {
        for (uint32_t j = 0; j < width; j++)
          printf("%d", p->pattern[height*i + j]);
        printf("\n");
      }
      printf("\n");
    }
  }
  else
    printf("FREEZE!!! no image paths provided!!!\n");



  return 0;
}
