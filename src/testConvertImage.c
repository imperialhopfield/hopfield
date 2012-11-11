#include <stdlib.h>
#include <stdio.h>
#include "convertImage.h"


int main(int argc, char** args){

  //char* imageInput = "../images/3x3.bmp";
  if(argc == 2 && args[1] != NULL)
  {
    binary_pattern_t** finalPatterns = image_to_binary(args + sizeof(char*), argc - 1);
  
    for(int j = 0; j < argc - 1; j++)
    {
      printf("pattern %d:", j);
      for(uint32_t i = 0; i < finalPatterns[j]->size; i++)
        printf("%d ", finalPatterns[j]->pattern[i]);
      printf("\n");
    }
  }
  else
    printf("FREEZE!!! no image paths provided!!!\n");

  

  return 0;
}
