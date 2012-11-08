#include <stdlib.h>
#include <stdio.h>
#include "convertImage.h"


int main(int argc, char** args){

  //char* imageInput = "../images/3x3.bmp";
  if(argc == 2 && args[1] != NULL)
  {
    binary_pattern_t * finalPattern = load_picture(args[1]);
    for(uint32_t i = 0; i < finalPattern->size; i++)
      printf("%d ", finalPattern->pattern[i]);
    printf("\n");
  }
  else
    printf("FREEZE!!! no image input provided!!!\n");


  return 0;
}
