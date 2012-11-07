CC=clang -Wall -Wextra
CFLAGS=`pkg-config --cflags --libs MagickWand`

all:
	$(CC) convertImage.c $(CFLAGS) -o convertimage
