#ifndef CONVERT_IMAGE_H
#define CONVERT_IMAGE_H

#include <stdint.h>
#include <stddef.h>
struct BinaryPattern
{
  uint32_t size;
  uint32_t * pattern;
};

typedef struct BinaryPattern binary_pattern_t;

binary_pattern_t * load_picture(char* inputImg, size_t width, size_t height);

#endif
