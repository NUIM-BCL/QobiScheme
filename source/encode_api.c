/*===========================================================================*
 * main.c								     *
 *									     *
 *	Main procedure							     *
 *									     *
 * EXPORTED PROCEDURES:							     *
 *	main								     *
 *									     *
 *===========================================================================*/

/*
 * Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/*
 *  $Header: /aux/qobi/cvsroot/QobiScheme/source/encode_api.c,v 1.1 2006-07-30 15:08:14 qobi Exp $
 *  $Log: encode_api.c,v $
 *  Revision 1.1  2006-07-30 15:08:14  qobi
 *  add
 *
 *  Revision 1.25  1995/08/07 21:44:21  smoot
 *  renamed index -> idx; save the encoder's name; compute frame types ahead of time
 *
 *  Revision 1.24  1995/06/21 18:25:57  smoot
 *  added binary write flag (DOS!)
 *
 * Revision 1.23  1995/05/16  06:25:28  smoot
 * added TUNEing init and float-dct == float_dct
 *
 * Revision 1.22  1995/05/16  00:15:05  smoot
 * fixed usage print
 *
 * Revision 1.21  1995/05/11  23:59:56  smoot
 * *** empty log message ***
 *
 * Revision 1.20  1995/02/02  20:05:37  eyhung
 * fixed smoot typo in 1.19
 *
 * Revision 1.19  1995/02/02  18:56:11  smoot
 * ANSI-ified some prototypes
 *
 * Revision 1.18  1995/02/01  21:47:37  smoot
 * cleanup
 *
 * Revision 1.17  1995/01/31  22:22:49  eyhung
 * Fixed steve's typo and added float_dct to Usage()
 *
 * Revision 1.16  1995/01/31  21:44:08  smoot
 * Added -float_dct option
 *
 * Revision 1.15  1995/01/31  01:19:39  eyhung
 * removed -interactive
 *
 * Revision 1.14  1995/01/27  21:56:57  eyhung
 * Deleted setting JMOVIE_TYPE to JPEG_TYPE since we need to know
 * if we started with a JMOVIE for getting input files
 *
 * Revision 1.13  1995/01/19  23:50:06  eyhung
 * Removed printing of output file to screen - done at end of encoding now.
 *
 * Revision 1.12  1995/01/19  23:08:41  eyhung
 * Changed copyrights
 *
 * Revision 1.11  1995/01/17  08:25:44  eyhung
 * added -interactive to Usage
 *
 * Revision 1.10  1995/01/17  08:24:53  eyhung
 * Added -interactive option
 *
 * Revision 1.9  1995/01/16  08:04:10  eyhung
 * More realQuiet stuff.
 *
 * Revision 1.8  1995/01/16  07:38:49  eyhung
 * Added realquiet option
 *
 * Revision 1.7  1994/11/14  22:32:01  smoot
 * Merged specifics and rate control
 *
 * Revision 1.6  1994/11/12  02:11:52  keving
 * nothing
 *
 * Revision 1.5  1994/03/15  00:27:11  keving
 * nothing
 *
 * Revision 1.4  1993/12/22  19:19:01  keving
 * nothing
 *
 * Revision 1.3  1993/07/22  22:23:43  keving
 * nothing
 *
 * Revision 1.2  1993/06/30  20:06:09  keving
 * nothing
 *
 * Revision 1.1  1993/02/17  23:18:20  dwallach
 * Initial revision
 *
 */


/*==============*
 * HEADER FILES *
 *==============*/

#include <assert.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
#include <malloc.h>
#include "all.h"
#define JPEG_INCLUDED		/* added by Qobi W23Oct96 */
#include <stdio.h>
#include "jpeglib.h"
#include "mtypes.h"
#include "mpeg.h"
#include "search.h"
#include "prototypes.h"
#include "param.h"
#include "parallel.h"
#include "readframe.h"
#include "combine.h"
#include "frames.h"
#include "jpeg.h"
#include "specifics.h"
#include "opts.h"

/*==================*
 * STATIC VARIABLES *
 *==================*/

static int	frameStart = -1;
static int	frameEnd;


/*==================*
 * GLOBAL VARIABLES *
 *==================*/

extern time_t IOtime;
int	whichGOP = -1;
boolean	childProcess = FALSE;
boolean	ioServer = FALSE;
boolean	outputServer = FALSE;
boolean	decodeServer = FALSE;
int	quietTime = 0;
boolean realQuiet = FALSE;
boolean	frameSummary = TRUE;
boolean debugSockets = FALSE;
boolean debugMachines = FALSE;
boolean showBitRatePerFrame = FALSE;
boolean	computeMVHist = FALSE;
int     baseFormat;
extern  boolean specificsOn;
extern  FrameSpecList *fsl;
boolean pureDCT=FALSE;
char    encoder_name[1024] = "foo";

/*===============================*
 * INTERNAL PROCEDURE prototypes *
 *===============================*/

static void Usage _ANSI_ARGS_((void));
static void CompileTests _ANSI_ARGS_((void));


/*================================*
 * External PROCEDURE prototypes  *
 *================================*/

void init_idctref _ANSI_ARGS_((void));
void init_fdct _ANSI_ARGS_((void));

/*=====================*
 * EXPORTED PROCEDURES *
 *=====================*/


/*===========================================================================*
 *
 * main
 *
 *	see man page.  run without arguments to see usage
 *
 * RETURNS:	0 if all is well; 1 on most if not all errors
 *
 *===========================================================================*/
int
old_main(argc, argv)
    int argc;
    char **argv;
{
    FILE *ofp = NULL;
    register int idx;
    int	    function = ENCODE_FRAMES;
    int	    portNumber = 0;
    char    *hostName = NULL;
    int32   totalTime = -1;
    int	    maxMachines = 0x7fffffff;
    int	    outputFrames = 0;
    time_t  initTimeStart;
    time_t  framesTimeStart, framesTimeEnd;

    strcpy(encoder_name, argv[0]);

    CompileTests();

    time(&initTimeStart);

    if ( argc == 1 ) {
	Usage();
    }

    SetStatFileName("");

    /* parse the arguments */
    idx = 1;
    while ( idx < argc-1 ) {
	if ( argv[idx][0] != '-' ) {
	    Usage();
	}

	if ( strcmp(argv[idx], "-stat") == 0 ) {
	    if ( idx+1 < argc-1 ) {
		SetStatFileName(argv[idx+1]);
		idx += 2;
	    } else {
		Usage();
	    }
	} else if ( strcmp(argv[idx], "-gop") == 0 ) {
	    if ( (function != ENCODE_FRAMES) || (frameStart != -1) ) {
		Usage();
	    }

	    if ( idx+1 < argc-1 ) {
		whichGOP = atoi(argv[idx+1]);
		idx += 2;
	    } else {
		Usage();
	    }
	} else if ( strcmp(argv[idx], "-frames") == 0 ) {
	    if ( (function != ENCODE_FRAMES) || (whichGOP != -1) ) {
		Usage();
	    }

	    if ( idx+2 < argc-1 ) {
		frameStart = atoi(argv[idx+1]);
		frameEnd = atoi(argv[idx+2]);

		if ( (frameStart > frameEnd) || (frameStart < 0) ) {
		    fprintf(stderr, "ERROR:  bad frame numbers!\n");
		    Usage();
		}

		idx += 3;
	    } else {
		Usage();
	    }
	} else if ( strcmp(argv[idx], "-combine_gops") == 0 ) {
	    if ( (function != ENCODE_FRAMES) || (whichGOP != -1) ||
		 (frameStart != -1) ) {
		Usage();
	    }

	    function = COMBINE_GOPS;
	    idx++;
	} else if ( strcmp(argv[idx], "-combine_frames") == 0 ) {
	    if ( (function != ENCODE_FRAMES) || (whichGOP != -1) ||
		 (frameStart != -1) ) {
		Usage();
	    }

	    function = COMBINE_FRAMES;
	    idx++;
	} else if ( strcmp(argv[idx], "-child") == 0 ) {
	    if ( idx+7 < argc-1 ) {
		hostName = argv[idx+1];
		portNumber = atoi(argv[idx+2]);
		ioPortNumber = atoi(argv[idx+3]);
		combinePortNumber = atoi(argv[idx+4]);
		decodePortNumber = atoi(argv[idx+5]);
		machineNumber = atoi(argv[idx+6]);
		remoteIO = atoi(argv[idx+7]);

		IOhostName = hostName;
	    } else {
		Usage();
	    }

	    childProcess = TRUE;
	    idx += 8;
	} else if ( strcmp(argv[idx], "-io_server") == 0 ) {
	    if ( idx+2 < argc-1 ) {
		hostName = argv[idx+1];
		portNumber = atoi(argv[idx+2]);
	    } else {
		Usage();
	    }

	    ioServer = TRUE;
	    idx += 3;
	} else if ( strcmp(argv[idx], "-output_server") == 0 ) {
	    if ( idx+3 < argc-1 ) {
		hostName = argv[idx+1];
		portNumber = atoi(argv[idx+2]);
		outputFrames = atoi(argv[idx+3]);
	    } else {
		Usage();
	    }

	    function = COMBINE_FRAMES;
	    outputServer = TRUE;
	    idx += 4;
	} else if ( strcmp(argv[idx], "-decode_server") == 0 ) {
	    if ( idx+3 < argc-1 ) {
		hostName = argv[idx+1];
		portNumber = atoi(argv[idx+2]);
		outputFrames = atoi(argv[idx+3]);
	    } else {
		Usage();
	    }

	    function = COMBINE_FRAMES;
	    decodeServer = TRUE;
	    idx += 4;
	} else if ( strcmp(argv[idx], "-nice") == 0 ) {
	    niceProcesses = TRUE;
	    idx++;
	} else if ( strcmp(argv[idx], "-max_machines") == 0 ) {
	    if ( idx+1 < argc-1 ) {
		maxMachines = atoi(argv[idx+1]);
	    } else {
		Usage();
	    }

	    idx += 2;
	} else if ( strcmp(argv[idx], "-quiet") == 0 ) {
	    if ( idx+1 < argc-1 ) {
		quietTime = atoi(argv[idx+1]);
	    } else {
		Usage();
	    }

	    idx += 2;
	} else if ( strcmp(argv[idx], "-realquiet") == 0 ) {
            realQuiet = TRUE;
	    idx++;
	} else if (( strcmp(argv[idx], "-float_dct") == 0 ) ||
		   ( strcmp(argv[idx], "-float-dct") == 0 )) {
	    pureDCT = TRUE;
  	    init_idctref();
	    init_fdct();
	    idx++;
	} else if ( strcmp(argv[idx], "-no_frame_summary") == 0 ) {
	    if ( idx < argc-1 ) {
		frameSummary = FALSE;
	    } else {
		Usage();
	    }

	    idx++;
	} else if ( strcmp(argv[idx], "-snr") == 0 ) {
	    printSNR = TRUE;
	    idx++;
	} else if ( strcmp(argv[idx], "-mse") == 0 ) {
	    printSNR =  printMSE = TRUE;
	    idx++;
	} else if ( strcmp(argv[idx], "-debug_sockets") == 0 ) {
	    debugSockets = TRUE;
	    idx++;
	} else if ( strcmp(argv[idx], "-debug_machines") == 0 ) {
	    debugMachines = TRUE;
	    idx++;
	} else if ( strcmp(argv[idx], "-bit_rate_info") == 0 ) {
	    if ( idx+1 < argc-1 ) {
		showBitRatePerFrame = TRUE;
		SetBitRateFileName(argv[idx+1]);
		idx += 2;
	    } else {
		Usage();
	    }
	} else if ( strcmp(argv[idx], "-mv_histogram") == 0 ) {
	    computeMVHist = TRUE;
	    idx++;
	} else {
	    Usage();
	}
    }

    if ( ! ReadParamFile(argv[argc-1], function) ) {
	Usage();
    }

    /* Jim Boucher's stuff:
	if we are using a movie format then break up into frames*/
    if ( (!childProcess) && (baseFormat == JMOVIE_FILE_TYPE) ) {
         JM2JPEG();
    }

    if ( printSNR || (referenceFrame == DECODED_FRAME) ) {
	decodeRefFrames = TRUE;
    }

    numMachines = min(numMachines, maxMachines);

    Tune_Init();
    Frame_Init();

#ifdef BLEAH
    time_t  initTimeEnd;

    time(&initTimeEnd);
    fprintf(stdout, "INIT TIME:  %d seconds\n",
	    initTimeEnd-initTimeStart);
    fflush(stdout);
#endif

    if (specificsOn) Specifics_Init();

    ComputeFrameTable();

    if ( ioServer ) {
	StartIOServer(numInputFiles, hostName, portNumber);
	return 0;
    } else if ( outputServer ) {
	StartCombineServer(outputFrames, outputFileName, hostName, portNumber);
	return 0;
    } else if ( decodeServer ) {
	StartDecodeServer(outputFrames, outputFileName, hostName, portNumber);
	return 0;
    }

    if ( (frameStart == -1) &&
	 ((numMachines == 0) || (function != ENCODE_FRAMES)) ) {
	if ( (ofp = fopen(outputFileName, "wb")) == NULL ) {
	    fprintf(stderr, "ERROR:  Could not open output file!\n");
	    exit(1);
	}
    }

    if ( function == ENCODE_FRAMES ) {
	if ( (numMachines == 0) || (frameStart != -1) ) {
	    time(&framesTimeStart);
	    totalTime = GenMPEGStream(whichGOP, frameStart, frameEnd,
				      customQtable, customNIQtable,
				      numInputFiles, ofp,
				      outputFileName);
	    time(&framesTimeEnd);
	    if ( childProcess && (! realQuiet) ) {
#ifdef BLEAH
		fprintf(stdout, "SCHEDULE:  MACHINE %d FRAMES %d-%d TIME %d-%d IOTIME %d\n",
			machineNumber, frameStart, frameEnd,
			framesTimeStart, framesTimeEnd,
			IOtime);
#endif
		fprintf(stdout, "%s:  FRAMES %d-%d (%d seconds)\n",
			getenv("HOST"), frameStart, frameEnd,
			(int) (framesTimeEnd-framesTimeStart));
		fflush(stdout);
	    }
	} else {
	    /* check if parameter file has absolute path */
	    if ( (argv[argc-1][0] != '/') && (argv[argc-1][0] != '~') ) {
	      char *buf;
	      buf = malloc(MAXPATHLEN+1);
	      ERRCHK(buf, "main");
	      getcwd(buf, MAXPATHLEN);
	      strcat(buf, argv[argc-1]);
	      StartMasterServer(numInputFiles, buf, outputFileName);
	    } else {
		StartMasterServer(numInputFiles, argv[argc-1], outputFileName);
	    }
	}
    } else if ( function == COMBINE_GOPS ) {
	GOPStoMPEG(numInputFiles, outputFileName, ofp);
    } else if ( function == COMBINE_FRAMES ) {
	FramesToMPEG(numInputFiles, outputFileName, ofp, FALSE);
    }

    if ( childProcess ) {
	while ( NotifyMasterDone(hostName, portNumber, machineNumber,
				 totalTime,
				 &frameStart, &frameEnd) ) {
	    /* do more frames */
	    time(&framesTimeStart);
	    totalTime = GenMPEGStream(-1, frameStart, frameEnd,
				      customQtable, customNIQtable,
				      numInputFiles, NULL,
				      outputFileName);
	    time(&framesTimeEnd);

	    if (! realQuiet) {
#ifdef BLEAH
		fprintf(stdout, "SCHEDULE:  MACHINE %d FRAMES %d-%d TIME %d-%d IOTIME %d\n",
			machineNumber, frameStart, frameEnd,
			framesTimeStart, framesTimeEnd,
			IOtime);
#endif
		fprintf(stdout, "%s:  FRAMES %d-%d (%d seconds)\n",
			getenv("HOST"), frameStart, frameEnd,
			(int) (framesTimeEnd-framesTimeStart));
	    fflush(stdout);
	    }

	}
    }

    Frame_Exit();

    return 0;	/* all is well */
}


/*=====================*
 * INTERNAL PROCEDURES *
 *=====================*/

/*===========================================================================*
 *
 * Usage
 *
 *	prints out usage for the program
 *
 * RETURNS:	nothing
 *
 * SIDE EFFECTS:    none
 *
 *===========================================================================*/
static void
Usage()
{
    fprintf(stderr, "Usage:  mpeg_encode [options] param_file\n");
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "\t-stat stat_file:  append stats to stat_file\n");
    fprintf(stderr, "\t-quiet n:  don't report remaining time for at least n seconds\n");
    fprintf(stderr, "\t-realquiet:  output nothing at all if successful\n");
    fprintf(stderr, "\t-no_frame_summary:  suppress frame summary lines\n");
    fprintf(stderr, "\t-float_dct:  use more accurate floating point DCT\n");
    fprintf(stderr, "\t-gop gop_num:  encode only the numbered GOP\n");
    fprintf(stderr, "\t-combine_gops:  combine GOP files instead of encode\n");
    fprintf(stderr, "\t-frames first_frame last_frame:  encode only the specified frames\n");
    fprintf(stderr, "\t-combine_frames:  combine frame files instead of encode\n");
    fprintf(stderr, "\t-nice:  run slave processes nicely\n");
    fprintf(stderr, "\t-max_machines num_machines:  use at most num_machines machines\n");
    fprintf(stderr, "\t-snr:  print signal-to-noise ratio\n");
    fprintf(stderr, "\t-bit_rate_info rate_file:  put bit rate in specified file\n");
    fprintf(stderr, "\t-mv_histogram:  show histograms of motion vectors\n");
    exit(1);

/* extended usage (used by parallel code; shouldn't be called by user):
    -child parallelHostName portNumber ioPortNumber combinePortNumber machineNumber remote
    -io_server parallelHostName portNumber

    (remote = 1 if need to use ioPortNumber)
 */
}


static void
CompileTests()
{
    assert(sizeof(uint8) == 1);
    assert(sizeof(uint16) == 2);
    assert(sizeof(uint32) == 4);
    assert(sizeof(int8) == 1);
    assert(sizeof(int16) == 2);
    assert(sizeof(int32) == 4);

    if ( (-8 >> 3) != -1 ) {
	fprintf(stderr, "ERROR:  Right shifts are NOT arithmetic!!!\n");
	fprintf(stderr, "Change >> to multiplies by powers of 2\n");
	exit(1);
    }
}

/* Qobi's `Real Simple' TM mpeg_encode API, now with JPEG support! */

/* note: These routines are not reentrant so there can be only one video output
         port open at a time.  Also B frames are not yet supported in this
         API. */

enum VideoType {JPEG, MPEG1};
struct jpeg_compress_struct out_cinfo;
struct jpeg_error_mgr out_jerr;
enum VideoType out_video_type;
JSAMPLE *out_image_buffer;

#define VARIABLE_RATE 0
#define FIXED_RATE 1
#define	FPS_30	0x5
#define ASPECT_1 0x1
#define DEFAULT_BUFFER_SIZE 327680

extern boolean use_cache;
extern int Fsize_x, Fsize_y;
extern int32 bit_rate, buf_size;
extern MpegFrame *pastRefFrame, *futureRefFrame;
extern int framesOutput, realStart, realEnd, currentGOP, timeMask;
extern int numI, numP, numB, outputWidth, outputHeight;
extern boolean resizeFrame, GammaCorrection;
extern char *CurrFile;
extern int GOP_X, GOP_I, GOP_P, GOP_B;
extern Block **dct, **dctr, **dctb;
extern int mult_seq_headers, **pmvHistogram, **bbmvHistogram, **bfmvHistogram;
extern int Nx, Ni, Np, Nb, rc_numBlocks, rc_totalQuant, rc_totalOverheadBits;
extern int RC_MB_SAMPLE_RATE;
extern boolean BC_on;

/*
extern int numBIBlocks;
extern int numBBBlocks;
extern int numBSkipped;
extern int numBIBits;
extern int numBBBits;
extern int numFrames;
extern int numFrameBits;
extern int32 totalTime;
extern float totalSNR;
extern float totalPSNR;
extern int numBFOBlocks;
extern int numBBABlocks;
extern int numBINBlocks;
extern int numBFOBits;
extern int numBBABits;
extern int numBINBits;
extern FrameTable *frameTable;
extern boolean use_cache;
extern int firstI;
extern int lastNumBits;
extern int lastIFrame;
extern int numBlocks;
extern int numFrames;
extern int numFrameBits;
extern int32 totalTime;
extern float totalSNR;
extern float totalPSNR;
extern int gopStartFrame;
extern int lastGOPStart;
extern struct hostent *hostEntry;
extern boolean parallelPerfect;
extern int current_max_forked_pid;
extern int numInputFileEntries;
extern int numPIBlocks;
extern int numPPBlocks;
extern int numPSkipped;
extern int numPIBits;
extern int numPPBits;
extern int numFrames;
extern int numFrameBits;
extern int32 totalTime;
extern float totalSNR;
extern float totalPSNR;
extern int RateControlMode;
extern int32 buffer_size;
extern int32 bit_rate;
extern int32 VBV_delay;
extern int32 VBV_buffer;
extern int32 bufferFillRate;
extern int32 frameDelayIncrement;
extern float Ki;
extern float Kp;
extern float Kb;
extern int MB_cnt;
extern int fileType;
*/

void ProcessRefFrame _ANSI_ARGS_((MpegFrame *frame,
				  BitBucket *bb, int lastFrame,
				  char *outputFileName));

BitBucket *bb;
MpegFrame *frame = NULL, tempFrame, *framePtr;
boolean firstFrameDone = FALSE;
int frame_number;

int myGetFrameRate(p)
char *p;
{ float rate;
  int thouRate;
  sscanf(p, "%f", &rate);
  thouRate = (int)(0.5+1000.0*rate);
  if (thouRate == 23976) return 1;
  else if (thouRate==24000) return 2;
  else if (thouRate==25000) return 3;
  else if (thouRate==29970) return 4;
  else if (thouRate==30000) return 5;
  else if (thouRate==50000) return 6;
  else if (thouRate==59940) return 7;
  else if (thouRate==60000) return 8;
  else { fprintf(stderr,"INVALID FRAME RATE: %s frames/sec\n", p);
         exit(1);}}

FILE *open_video_output_file(char *pathname,
			     enum VideoType video_type,
			     int numFrames)
{ FILE *video_output_port;
  char frameType;
  int i;
  out_video_type = video_type;
  switch (out_video_type)
  { case JPEG:
    out_cinfo.err = jpeg_std_error(&out_jerr);
    jpeg_create_compress(&out_cinfo);
    if ((video_output_port = fopen(pathname, "wb"))==NULL)
    { fprintf(stderr, "can't open %s\n", pathname);
      exit(1);}
    jpeg_stdio_dest(&out_cinfo, video_output_port);
    break;
    case MPEG1:
    Fsize_x = 0;
    Fsize_y = 0;
    printSNR = FALSE;
    printMSE = FALSE;
    decodeRefFrames = FALSE;
    dct = NULL;
    dctr = NULL;
    dctb = NULL;
    gopSize = 100;
    bitRateFile = NULL;
    frameRate = FPS_30;
    frameRateRounded = 30;
    frameRateInteger = TRUE;
    aspectRatio = ASPECT_1;
    parallelTestFrames = 10;
    parallelTimeChunks = 60;
    niceProcesses = FALSE;
    forceIalign = FALSE;
    machineNumber = -1;
    remoteIO = FALSE;
    IOtime = 0;
    tuneingOn = FALSE;
    block_bound = 128;
    collect_quant = FALSE;
    collect_quant_detailed = 0;
    kill_dim = FALSE;
    SearchCompareMode = DEFAULT_SEARCH;
    squash_small_differences = FALSE;
    LocalDCTRateScale = 1.0;
    LocalDCTDistortScale = 1.0;
    IntraPBAllowed = TRUE;
    WriteDistortionNumbers = FALSE;
    collect_distortion_detailed = 0;
    DoLaplace = FALSE;
    BSkipBlocks = TRUE;
    BC_on = FALSE;
    parallelTestFrames = 10;
    parallelTimeChunks = 60;
    niceProcesses = FALSE;
    forceIalign = FALSE;
    machineNumber = -1;
    remoteIO = FALSE;
    IOtime = 0;
    GammaCorrection = FALSE;
    specificsOn = FALSE;
    stdinUsed = FALSE;
    mult_seq_headers = 0;
    customQtable = NULL;
    customNIQtable = NULL;
    pmvHistogram = NULL;
    bbmvHistogram = NULL;
    bfmvHistogram = NULL;
    GOP_X = 0;
    GOP_I = 0;
    GOP_P = 0;
    GOP_B = 0;
    Nx = 0;
    Ni = 0;
    Np = 0;
    Nb = 0;
    rc_numBlocks = 0;
    rc_totalQuant = 0;
    rc_totalOverheadBits = 0;
    RC_MB_SAMPLE_RATE = 0;

    frameStart = -1;
    whichGOP = -1;
    childProcess = FALSE;
    ioServer = FALSE;
    outputServer = FALSE;
    decodeServer = FALSE;
    quietTime = 0;
    realQuiet = FALSE;
    frameSummary = TRUE;
    debugSockets = FALSE;
    debugMachines = FALSE;
    showBitRatePerFrame = FALSE;
    computeMVHist = FALSE;
    pureDCT = FALSE;
    firstFrameDone = FALSE;

    /*
    numBIBlocks = 0;
    numBBBlocks = 0;
    numBSkipped = 0;
    numBIBits = 0;
    numBBBits = 0;
    numFrames = 0;
    numFrameBits = 0;
    totalTime = 0;
    totalSNR = 0.0;
    totalPSNR = 0.0;
    numBFOBlocks = 0;
    numBBABlocks = 0;
    numBINBlocks = 0;
    numBFOBits = 0;
    numBBABits = 0;
    numBINBits = 0;
    frameTable = NULL;
    use_cache = FALSE;
    firstI = 0;
    lastNumBits = 0;
    lastIFrame = 0;
    numBlocks = 0;
    numFrames = 0;
    numFrameBits = 0;
    totalTime = 0;
    totalSNR = 0.0;
    totalPSNR = 0.0;
    gopStartFrame = 0;
    lastGOPStart = 0;
    hostEntry = NULL;
    parallelPerfect = FALSE;
    current_max_forked_pid = 0;
    numInputFileEntries = 0;
    numPIBlocks = 0;
    numPPBlocks = 0;
    numPSkipped = 0;
    numPIBits = 0;
    numPPBits = 0;
    numFrames = 0;
    numFrameBits = 0;
    totalTime = 0;
    totalSNR = 0.0;
    totalPSNR = 0.0;
    RateControlMode = VARIABLE_RATE;
    buffer_size = DEFAULT_BUFFER_SIZE;
    bit_rate = -1;
    VBV_delay = 0;
    VBV_buffer = 0;
    bufferFillRate = 0;
    frameDelayIncrement = 0;
    Ki = .7;
    Kp = 1;
    Kb = 1.4;
    MB_cnt = -1;
    fileType = BASE_FILE_TYPE;
    */
    use_cache = FALSE;

    realQuiet = TRUE;
    CompileTests();
    SetStatFileName("");
    /* SetFramePattern("IBBPBBPBBPBBPBBP"); */
    SetFramePattern("IIII");
    strcpy(outputFileName, pathname);
    SetFileFormat("PPM");
    strcpy(inputConversion, "*");
    SetGOPSize(16);
    SetSlicesPerFrame(1);
    strcpy(currentPath, "/tmp");
    SetPixelSearch("HALF");
    SetSearchRange(10, 10);
    SetPSearchAlg("LOGARITHMIC");
    SetBSearchAlg("CROSS2");
    SetIQScale(8);
    SetPQScale(10);
    SetBQScale(25);
    SetReferenceFrameType("ORIGINAL");
    setBitRate("1000000");
    setBufferSize("327680");
    frameRate = myGetFrameRate("30");
    frameRateRounded = (int)VidRateNum[frameRate];
    if ((frameRate%3)==1) frameRateInteger = FALSE;
    forceEncodeLast = TRUE;
    numMachines = 0;
    numInputFiles = numFrames;
    SetFCode();
    if (psearchAlg==PSEARCH_TWOLEVEL) SetPixelSearch("HALF");
    if (printSNR||(referenceFrame==DECODED_FRAME)) decodeRefFrames = TRUE;
    Tune_Init();
    Frame_Init();
    if (specificsOn) Specifics_Init();
    ComputeFrameTable();
    if ((video_output_port = fopen(outputFileName, "wb"))==NULL)
    { fprintf(stderr, "mpeg_encode: Could not open MPEG output file\n");
      exit(1);}
    ResetIFrameStats();
    ResetPFrameStats();
    ResetBFrameStats();
    Fsize_Reset();
    framesOutput = 0;
    SetFileType(inputConversion);
    realStart = 0;
    realEnd = numFrames-1;
    numI = 0;
    numP = 0;
    numB = 0;
    timeMask = 0;
    for (i = 0; i<=realEnd; i++)
    { frameType = FType_Type(i);
      switch (frameType)
      { case 'i': numI++; timeMask |= 0x1; break;
        case 'p': numP++; timeMask |= 0x2; break;
        case 'b': numB++; timeMask |= 0x4; break;}}
    bb = Bitio_New(video_output_port);
    tc_hrs = 0;
    tc_min = 0;
    tc_sec = 0;
    tc_pict = 0;
    tc_extra = 0;
    totalFramesSent = 0;
    currentGOP = gopSize;
    if (getRateMode()==FIXED_RATE) initRateControl();
    pastRefFrame = NULL;
    futureRefFrame = NULL;
    frame_number = 0;}
  return video_output_port;}

void begin_video_frame(int width, int height, int maxVal)
{ char frameType;
  switch (out_video_type)
  { case JPEG:
    out_cinfo.image_width = width;
    out_cinfo.image_height = height;
    out_cinfo.input_components = 3;
    out_cinfo.in_color_space = JCS_RGB;
    jpeg_set_defaults(&out_cinfo);
    jpeg_start_compress(&out_cinfo, TRUE);
    out_image_buffer = (JSAMPLE *)malloc(height*width*3);
    break;
    case MPEG1:
    frameType = FType_Type(frame_number++);
    /* skip non-reference frames if non-interactive
       read in non-reference frames if interactive */
    if ( frameType == 'b' ) return;
    frame = Frame_New(frame_number-1, frameType);
    pastRefFrame = futureRefFrame;
    futureRefFrame = frame;
    if (resizeFrame)
    { tempFrame.inUse = FALSE;
      tempFrame.ppm_data = NULL;
      tempFrame.rgb_data = NULL;
      tempFrame.orig_y = NULL;
      tempFrame.y_blocks = NULL;
      tempFrame.decoded_y = NULL;
      tempFrame.halfX = NULL;
      framePtr = &tempFrame;}
    else framePtr = frame;
    Fsize_Note(framePtr->id, width, height);
    framePtr->rgb_maxval = maxVal;
    Frame_AllocPPM(framePtr);}}

void put_video_red(int y, int x, int value)
{ switch (out_video_type)
  { case JPEG:
    out_image_buffer[3*(y*out_cinfo.image_width+x)] = (JSAMPLE)value;
    break;
    case MPEG1:
    ((char *)(framePtr->ppm_data[y]))[3*x] = value;}}

void put_video_green(int y, int x, int value)
{ switch (out_video_type)
  { case JPEG:
    out_image_buffer[3*(y*out_cinfo.image_width+x)+1] = (JSAMPLE)value;
    break;
    case MPEG1:
    ((char *)(framePtr->ppm_data[y]))[3*x+1] = value;}}

void put_video_blue(int y, int x, int value)
{ switch (out_video_type)
  { case JPEG:
    out_image_buffer[3*(y*out_cinfo.image_width+x)+2] = (JSAMPLE)value;
    break;
    case MPEG1:
    ((char *)(framePtr->ppm_data[y]))[3*x+2] = value;}}

void end_video_frame(void)
{ JSAMPROW row_pointer[1];
  switch (out_video_type)
  { case JPEG:
    while (out_cinfo.next_scanline<out_cinfo.image_height)
    { row_pointer[0] =
      &out_image_buffer[out_cinfo.next_scanline*out_cinfo.image_width*3];
      jpeg_write_scanlines(&out_cinfo, row_pointer, 1);}
    jpeg_finish_compress(&out_cinfo);
    free(out_image_buffer);
    break;
    case MPEG1:
    PPMtoYUV(framePtr);
    if (resizeFrame)
    { Frame_Resize(frame, &tempFrame, Fsize_x, Fsize_y,
                   outputWidth, outputHeight);}
    if (GammaCorrection) DoGamma(frame, Fsize_x, Fsize_y);
    if (kill_dim) DoKillDim(frame, Fsize_x, Fsize_y);
    MotionSearchPreComputation(frame);
    if (!firstFrameDone)
    { SetBlocksPerSlice();
      if (getRateMode()==FIXED_RATE)
      { bit_rate = getBitRate();
        buf_size = getBufferSize();}
      else
      { bit_rate = -1;
        buf_size = -1;}
      Mhead_GenSequenceHeader(bb, Fsize_x, Fsize_y, aspectRatio, frameRate,
  		              bit_rate, buf_size, 1,
  			      customQtable, customNIQtable,
                              NULL, 0, NULL, 0);
      firstFrameDone = TRUE;}
    ProcessRefFrame(frame, bb, realEnd, outputFileName);}}

void close_video_output_port(FILE *video_output_port)
{ switch (out_video_type)
  { case JPEG:
    fclose(video_output_port);
    jpeg_destroy_compress(&out_cinfo);
    break;
    case MPEG1:
    if (frame!=NULL) Frame_Free(frame);
    Mhead_GenSequenceEnder(bb);
    Bitio_Flush(bb);
    bb = NULL;
    fclose(video_output_port);
    Frame_Exit();}}
