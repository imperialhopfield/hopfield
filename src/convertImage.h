#ifndef CONVERT_IMAGE_H
#define CONVERT_IMAGE_H

struct BinaryPattern
{
  int size;
  int * pattern;
};

typedef struct BinaryPattern binary_pattern_t;

binary_pattern_t load_picture(char* inputImg);

#endif
