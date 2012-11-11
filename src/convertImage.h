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

binary_pattern_t** image_to_binary (char** filepaths, int nr_images);

#endif
