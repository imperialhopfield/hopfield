CC=clang -Wall -Wextra
CFLAGS=`pkg-config --cflags MagickWand`
LDFLAGS=`pkg-config --libs MagickWand`

.PHONY: all
all: c_convertImage

.PHONY: c_convertImage
c_convertImage:
	$(CC) testConvertImage.c $(CFLAGS) $(LDFLAGS) convertImage.o -o convertimage
