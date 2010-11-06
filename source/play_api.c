#define JPEG_INCLUDED		/* added by Qobi W23Oct96 */
#include <stdio.h>
#include "video.h"
#include "proto.h"
#ifndef NOCONTROLS
#include "ctrlbar.h"
#endif
#include <math.h>
#include <sys/types.h>
#include <signal.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <malloc.h>
#include "util.h"
#include "dither.h"
#include "jpeglib.h"

/* Define buffer length. */

#define BUF_LENGTH 80000

/* External declaration of main decoding call. */

extern VidStream *mpegVidRsrc();
extern VidStream *NewVidStream();

/* Declaration of global variable to hold dither info. */

int ditherType = ORDERED_DITHER;
int matched_depth = 8;

#ifdef DCPREC
/* Declaration of global variable to hold DC precision */
int dcprec = 0;
#endif

/* Global file pointer to incoming data. */
FILE *input;
char *inputName;

/* End of File flag. */
int EOF_flag = 0;

/* Loop flag. */
int loopFlag = 0;

/* Shared memory flag. */
int shmemFlag = 0;

/* Quiet flag. */
#ifdef QUIET
int quietFlag = 1;
#else
int quietFlag = 0;
#endif

/* Own Color Map flag. */
int owncmFlag = 0;

/* "Press return" flag, requires return for each new frame */
int requireKeypressFlag = 0;

/* Display image on screen? */
int noDisplayFlag = 0;

/* Seek Value.
   0 means do not seek.
   N (N>0) means seek to N after the header is parsed
   N (N<0) means the seek has beeen done to offset N
*/
long seekValue = 0;

/* Framerate, -1: specified in stream (default)
               0: as fast as possible
               N (N>0): N frames/sec
               */
int framerate = -1;

/* Flags/values to control Arbitrary start/stop frames. */
int partialFlag = 0, startFrame = -1, endFrame = -1;

/* Flag for gamma correction */
int gammaCorrectFlag = 0;
double gammaCorrect = 1.0;

/* Flag for chroma correction */
int chromaCorrectFlag = 0;
double chromaCorrect = 1.0;

/* Setjmp/Longjmp env. */
jmp_buf env;

/* Flag for high quality at the expense of speed */
#ifdef QUALITY
int qualityFlag = 1;
#else
int qualityFlag = 0;
#endif

/* no further error messages */
static BOOLEAN exiting=FALSE;

/*
 *--------------------------------------------------------------
 *
 * DoDitherImage --
 *
 *      Called when image needs to be dithered. Selects correct
 *      dither routine based on info in ditherType.
 *
 * Results:
 *        None.
 *
 * Side effects:
 *        None.
 *
 *--------------------------------------------------------------
 */

void
DoDitherImage(l, Cr, Cb, disp, h, w)
unsigned char *l, *Cr, *Cb, *disp;
int h, w;
{

  switch(ditherType) {
  case HYBRID_DITHER:
    HybridDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case HYBRID2_DITHER:
    HybridErrorDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case FS2FAST_DITHER:
    FS2FastDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case FS2_DITHER:
    FS2DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case FS4_DITHER:
    FS4DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case Twox2_DITHER:
    Twox2DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case FULL_COLOR2_DITHER:
    if (matched_depth == 32)
      Twox2Color32DitherImage(l, Cr, Cb, disp, h, w);
    else
      Twox2Color16DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case FULL_COLOR_DITHER:
    if (matched_depth == 32)
      Color32DitherImage(l, Cr, Cb, disp, h, w);
    else
      Color16DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case GRAY_DITHER:
  case GRAY256_DITHER:
    if (matched_depth == 8)
      GrayDitherImage(l, Cr, Cb, disp, h, w);
    else if (matched_depth == 16)
      Gray16DitherImage(l, Cr, Cb, disp, h, w);
    else if (matched_depth == 32 || matched_depth == 24)
      Gray32DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case GRAY2_DITHER:
  case GRAY2562_DITHER:
    if (matched_depth == 8)
      Gray2DitherImage(l, Cr, Cb, disp, h, w);
    else if (matched_depth == 16)
      Gray216DitherImage(l, Cr, Cb, disp, h, w);
    else if (matched_depth == 32 || matched_depth == 24)
      Gray232DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case NO_DITHER:
    break;

  case PPM_DITHER:
    Color32DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case ORDERED_DITHER:
    OrderedDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case MONO_DITHER:
    MonoDitherImage(l, Cr, Cb, disp, h, w);
    break;

  case MONO_THRESHOLD:
    MonoThresholdImage(l, Cr, Cb, disp, h, w);
    break;

  case ORDERED2_DITHER:
    Ordered2DitherImage(l, Cr, Cb, disp, h, w);
    break;

  case MBORDERED_DITHER:
    MBOrderedDitherImage(l, Cr, Cb, disp, h, w);
    break;
  }
}

/* Qobi's `Real Simple' TM mpeg_play API, now with JPEG support! */

/* note: These routines are not reentrant so there can be only one video input
         port open at a time. */

enum VideoType {JPEG, MPEG1};

struct jpeg_decompress_struct in_cinfo;
struct jpeg_error_mgr in_jerr;
enum VideoType in_video_type;
JSAMPLE *in_image_buffer = NULL;

int frame_ready;
extern int sys_layer;

VidStream *open_video_input_file(char *pathname, enum VideoType video_type)
{ VidStream *video_input_port;
  in_video_type = video_type;
  switch (in_video_type)
  { case JPEG:
    in_cinfo.err = jpeg_std_error(&in_jerr);
    jpeg_create_decompress(&in_cinfo);
    if ((video_input_port = (VidStream *)fopen(pathname, "rb"))==NULL)
    { fprintf(stderr, "can't open %s\n", pathname);
      exit(1);}
    jpeg_stdio_src(&in_cinfo, (FILE *)video_input_port);
    break;
    case MPEG1:
    sys_layer = -1;
    FilmState = FILM_NORMAL;
    EOF_flag = 0;
    LUM_RANGE = 8;
    CR_RANGE = CB_RANGE = 4;
    ditherType = FULL_COLOR_DITHER;
    noDisplayFlag = 1;
    qualityFlag = 1;
    quietFlag = 1;
    inputName = pathname;
    input = fopen(inputName, "r");
    if ((input = fopen(inputName, "r"))==NULL)
    { fprintf(stderr, "mpeg_play: Could not open MPEG input file\n");
      exit(1);}
    lum_values = (int *)malloc(LUM_RANGE*sizeof(int));
    cr_values = (int *)malloc(CR_RANGE*sizeof(int));
    cb_values = (int *)malloc(CB_RANGE*sizeof(int));
    init_tables();
    shmemFlag = 0;
    wpixel[0] = 0xff;
    wpixel[1] = 0xff00;
    wpixel[2] = 0xff0000;
    matched_depth = 32;
    InitColorDither(1);
    video_input_port = NewVidStream((unsigned int)BUF_LENGTH);
    frame_ready = 0;
    mpegVidRsrc(0, video_input_port, 1);}
  return video_input_port;}

int read_video(VidStream *video_input_port)
{ int c;
  JSAMPROW row_pointer[1];
  switch (in_video_type)
  { case JPEG:
    if ((c = getc((FILE *)video_input_port))==EOF) return 0;
    ungetc(c, (FILE *)video_input_port);
    jpeg_read_header(&in_cinfo, TRUE);
    if (in_image_buffer!=NULL)
    { free(in_image_buffer);
      in_image_buffer = NULL;}
    in_image_buffer =
      (JSAMPLE *)malloc(in_cinfo.image_height*in_cinfo.image_width*3);
    jpeg_start_decompress(&in_cinfo);
    while (in_cinfo.output_scanline<in_cinfo.output_height)
    { row_pointer[0] = &in_image_buffer[in_cinfo.output_scanline*
                                        in_cinfo.output_width*
                                        in_cinfo.output_components];
      jpeg_read_scanlines(&in_cinfo, row_pointer, 1);}
    jpeg_finish_decompress(&in_cinfo);
    break;
    case MPEG1:
    if (FilmState==FILM_END) return 0;
    frame_ready = 0;
    while (FilmState!=FILM_END&&!frame_ready)
    { mpegVidRsrc(0, video_input_port, 0);}}
  return 1;}

int get_video_width(VidStream *video_input_port)
{ switch (in_video_type)
  { case JPEG: return in_cinfo.image_width;
    case MPEG1: return video_input_port->h_size;}}

int get_video_height(VidStream *video_input_port)
{ switch (in_video_type)
  { case JPEG: return in_cinfo.image_height;
    case MPEG1: return video_input_port->v_size;}}

unsigned int get_video_red(VidStream *video_input_port, int y, int x)
{ switch (in_video_type)
  { case JPEG:
    return in_image_buffer[3*(y*in_cinfo.image_width+x)];
    case MPEG1:
    return ((unsigned int *)(video_input_port->current->display))
           [y*video_input_port->h_size+x]&0xff;}}

unsigned int get_video_green(VidStream *video_input_port, int y, int x)
{ switch (in_video_type)
  { case JPEG:
    return in_image_buffer[3*(y*in_cinfo.image_width+x)+1];
    case MPEG1:
    return (((unsigned int *)(video_input_port->current->display))
            [y*video_input_port->h_size+x]>>8)&0xff;}}

unsigned int get_video_blue(VidStream *video_input_port, int y, int x)
{ switch (in_video_type)
  { case JPEG:
    return in_image_buffer[3*(y*in_cinfo.image_width+x)+2];
    case MPEG1:
    return (((unsigned int *)(video_input_port->current->display))
 	    [y*video_input_port->h_size+x]>>16)&0xff;}}

void close_video_input_port(VidStream *video_input_port)
{ switch (in_video_type)
  { case JPEG:
    if (in_image_buffer!=NULL)
    { free(in_image_buffer);
      in_image_buffer = NULL;}
    fclose((FILE *)video_input_port);
    jpeg_destroy_decompress(&in_cinfo);
    break;
    case MPEG1:
    DestroyVidStream(video_input_port);
    fclose(input);}}
