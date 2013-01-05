CC=clang -Wall -Wextra
CFLAGS=`pkg-config --cflags MagickWand`
LDFLAGS=`pkg-config --libs MagickWand`

.PHONY: all
all: c_convertImage

convertImage.o:
	$(CC) -c convertImage.c $(CFLAGS)

.PHONY: c_convertImage
c_convertImage: convertImage.o
	$(CC) testConvertImage.c $(CFLAGS) $(LDFLAGS) convertImage.o -o convertimage

.PHONY: clean
clean:
	rm -f convertImage.o
	rm -f convertimage
