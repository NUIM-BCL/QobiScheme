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

 contributed by (but no rights held by):

 Michael J. Donahue
 National Institute of Standards and Technology
 Gaithersburg MD USA
 donahue@ulexite.nist.gov

 */

#include <X11/Xlib.h>
#include <X11/Xutil.h>

/* Declaration of global video display window */
extern Window window;

#define DISPLAY_WINDOW_BORDER 4

/* Control bar display flag */
extern int ControlShow;
#define CTRLBAR_OFF  0
#define CTRLBAR_ON   1
#define CTRLBAR_NONE 2  /* No control bar at all */

/* Video play states */
extern int ControlState;
#define CTRL_UNDEFINED  -1
#define CTRL_PAUSE       0
#define CTRL_PLAY        1
#define CTRL_STEP        2
#define CTRL_EOF         3
#define CTRL_REWIND      4
#define CTRL_FFWD        5   /* Fast Forward (skips frames w/o display) */
#define CTRL_EXIT        6

/* ControlMotion tracks Play/Pause status (for resumption after e.g. Rewind) */
extern int ControlMotion;
#define CTRLMOTION_OFF 0   /* Pause */
#define CTRLMOTION_ON  1   /* Play  */

/* Film status */
extern int FilmState;      /* Note: This is slightly different that EOF_flag, */
#define FILM_UNDEFINED -1  /*  which denotes end-of-file on reading, whereas  */
#define FILM_NORMAL     0  /*  FilmState is end-of-film on displaying.        */
#define FILM_END        1 

/* StopWatch constants */
extern long TotalFrameCount; /* Total number of frames processed, with loops */
#define STOPWATCH_RESET 0  /* Resets timing, returns 0.0.                   */
#define STOPWATCH_START 1  /* Starts timing, returns 0.0.                   */
#define STOPWATCH_STOP  2  /* Stops timing,  returns elapsed time in secs.  */
#define STOPWATCH_READ  3  /* Returns elapsed time in seconds.              */

/* Miscellaneous externals */
extern int loopFlag;      /* Video loop flag */
extern int totNumFrames;  /* Frame number */
extern int EOF_flag;      /* EOF flag set by readfile routines */
extern int ditherType;    /* NO_DITHER => no windows */
extern FILE *input;       /* Video (mpeg) input file */
extern long seekValue;    /* Used by SeekStream() as global import */
extern int startFrame;    /* != -1 => skip all frames up to this one */
extern int endFrame;      /* Either last frame to display or else frame count,
                           * depending on the value of seekValue.             */
