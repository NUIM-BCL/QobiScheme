/*===========================================================================*
 * fsize.c								     *
 *									     *
 *	procedures to keep track of frame size				     *
 *									     *
 * EXPORTED PROCEDURES:							     *
 *	Fsize_Reset							     *
 *	Fsize_Note							     *
 *	Fsize_Validate							     *
 *									     *
 * EXPORTED VARIABLES:							     *
 *	Fsize_x								     *
 *	Fsize_y								     *
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


/*==============*
 * HEADER FILES *
 *==============*/

#include "all.h"
#include "fsize.h"
#include "dct.h"


/*==================*
 * GLOBAL VARIABLES *
 *==================*/
int Fsize_x = 0;
int Fsize_y = 0;


/*=====================*
 * EXPORTED PROCEDURES *
 *=====================*/

/*===========================================================================*
 *
 * Fsize_Reset
 *
 *	reset the frame size to 0
 *
 * RETURNS:	nothing
 *
 * SIDE EFFECTS:    Fsize_x, Fsize_y
 *
 *===========================================================================*/
void
Fsize_Reset()
{
    Fsize_x = Fsize_y = 0;
}


/*===========================================================================*
 *
 * Fsize_Validate
 *
 *	make sure that the x, y values are 16-pixel aligned
 *
 * RETURNS:	modifies the x, y values to 16-pixel alignment
 *
 * SIDE EFFECTS:    none
 *
 *===========================================================================*/
void
Fsize_Validate(x, y)
    int *x;
    int *y;
{
    *x &= ~(DCTSIZE * 2 - 1);
    *y &= ~(DCTSIZE * 2 - 1);
}


/*===========================================================================*
 *
 * Fsize_Note
 *
 *	note the given frame size and modify the global values as appropriate
 *
 * RETURNS:	nothing
 *
 * SIDE EFFECTS:    Fsize_x, Fsize_y
 *
 *===========================================================================*/
void
Fsize_Note(id, width, height)
    int id;
    int width;
    int height;
{
  static boolean warned16 = FALSE,
                 warnedSz = FALSE;
  int oFsize_x, oFsize_y;

  oFsize_x = Fsize_x;
  oFsize_y = Fsize_y;
  Fsize_x = width;
  Fsize_y = height;
  Fsize_Validate(&Fsize_x, &Fsize_y);
  
  if ((Fsize_x != width || Fsize_y != height) && !warned16) {
    warned16 = TRUE;
    fprintf(stderr, "WARNING: The input images are not a multiple of 16 in dimension\n\tthey will be trimmed.\n\n");
  }
  if ((((oFsize_x !=0) && (oFsize_x != Fsize_x)) ||
       ((oFsize_y !=0) && (oFsize_y != Fsize_y))) &&
      !warnedSz) {
    fprintf(stderr, "\nWARNING:: Your images seem to be of different sizes.\n");
    fprintf(stderr, "\tThis is likely to not work, and dump core.\n");
    fprintf(stderr, "\tBut we're feeling risky, we'll try it anyway!\n\n");
    warnedSz = TRUE;
  }

  if ((Fsize_x==0) || (Fsize_y==0)) {
    fprintf(stderr,"Frame %d:  size is zero!\n",id);
    /*      exit(1); */
  }
  
#ifdef BLEAH
    if (Fsize_x == 0) {
	Fsize_x = width;
	Fsize_y = height;
	Fsize_Validate(&Fsize_x, &Fsize_y);
    } else if (width < Fsize_x || height < Fsize_y) {
	fprintf(stderr, "Frame %d: wrong size: (%d,%d).  Should be greater or equal to: (%d,%d)\n",
		id, width, height, Fsize_x, Fsize_y);
	exit(1);
    }
#endif
}
