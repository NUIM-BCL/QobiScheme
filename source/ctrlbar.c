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

#ifndef NOCONTROLS
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dither.h"
#include "video.h"
#include "ctrlbar.h"
#include "proto.h"

#ifndef MIPS
#include <sys/time.h>
#else
#include <sys/types.h>
#include <sys/system.h>
#endif

/* Default is to play asap.  If you want it to start paused, change to CTRL_PAUSE */
#define INITIAL_STATE CTRL_PLAY

/* Global variable definitions */
int ControlShow     = CTRLBAR_ON;         /* ON => Show (display) control bar */
int ControlState    = CTRL_UNDEFINED;     /* Current control state */
int ControlMotion   = CTRLMOTION_OFF;     /* Pause mode */
int FilmState       = FILM_NORMAL;        /* Current film (display) status */
long TotalFrameCount= 0; /* Total number of frames processed, including loops */

/* File statics */
static int CtrlBarWidth;
static int CtrlBarHeight =  31;
static int CtrlBarBorder =   4;
static int ChildBorder   =   2;
static int ChildMargin   =   6;

static Window ctrlwindow = 0;
static int screen = -1;
static XFontStruct* ctrlfont = NULL;
static int fontheight, fontwidth;
static unsigned long fgcolor, bgcolor;

static int ctrl_init = 0;  /* 1 => Control windows have been initialized */

/* Support for WM_DELETE_WINDOW */
static Atom protocol_atom = (Atom)None;
static Atom delete_atom   = (Atom)None;

/* Child windows */
static int ChildCount    =   7;
static Window frametotalwin;
static int ftw_width, ftw_height;
static Window rewindwin;
static int rww_width, rww_height;
static Window pausewin;
static int psw_width, psw_height;
static Window playwin;
static int plw_width, plw_height;
static Window stepwin;
static int stw_width, stw_height;
static Window exitwin;
static int exw_width, exw_height;
static Window loopwin;
static int lpw_width, lpw_height;


/*
 *--------------------------------------------------------------
 *
 * StopWatch --
 *
 *        On/off timing routine (in real elapsed time).
 *
 * Results:
 *        If import is:
 *                STOPWATCH_START ---- Starts timing, returns 0.0.
 *                STOPWATCH_STOP  ---- Stops timing,  returns elapsed
 *                                    time in seconds.
 *                STOPWATCH_RESET ---- Resets timing, returns 0.0.
 *                STOPWATCH_READ  ---- Returns elapsed time in seconds.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

double
StopWatch(action)
int action;
{
  static struct timeval start,current;  /* Working times */
  static struct timeval elapsed;        /* Previously accumulated time */
  static int state = STOPWATCH_STOP;
  double dtemp = 0.0;
  long ltemp;

  if (action == state) {
    if (state == STOPWATCH_START) {
      return 0.0;
    }
    else return elapsed.tv_sec + elapsed.tv_usec/1000000.0;
  }
  switch(action) {
  case STOPWATCH_START:
    state = STOPWATCH_START;
    gettimeofday(&start, (struct timezone *)NULL);
    break;
  case STOPWATCH_STOP:
    gettimeofday(&current, (struct timezone *)NULL);
    state = STOPWATCH_STOP;
    ltemp = elapsed.tv_usec + current.tv_usec - start.tv_usec;
    elapsed.tv_sec += current.tv_sec-start.tv_sec + ltemp/1000000;
    elapsed.tv_usec = ltemp % 1000000;
    /* And fall through to STOPWATCH_READ */
  case STOPWATCH_READ:
    if (state == STOPWATCH_START) { /* Stopwatch is running */
      gettimeofday(&current,(struct timezone *)NULL);
      dtemp = (current.tv_sec-start.tv_sec + elapsed.tv_sec)
        + (current.tv_usec-start.tv_usec + elapsed.tv_usec) / 1000000.0;
    }
    else dtemp = elapsed.tv_sec + elapsed.tv_usec/1000000.0;
    break;
  case STOPWATCH_RESET:
    state = STOPWATCH_STOP;
    elapsed.tv_sec = elapsed.tv_usec = 0;
    break;
  default:
    fprintf(stderr,"Illegal action (%d) requested of StopWatch()",action);
    exit(1);
  }
  return dtemp;
}


/*
 *--------------------------------------------------------------
 *
 * GetWindowOrigins --
 *
 *        Determines window coordinates with respect to root window.
 *
 * Results:
 *        Sets (xwhole,ywhole) to root coordinates of upper lefthand corner
 *        of the specified window (including decorations), and (xclient,yclient)
 *        to the root coordinates of the client region.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static void GetWindowOrigins(win,xclient,yclient,xwhole,ywhole)
Window win;
int *xclient;
int *yclient;
int *xwhole;
int *ywhole;
{
  Window dummy_window;

  /* First get coordinates for client "sub"-window */
  XTranslateCoordinates(display,win,DefaultRootWindow(display),
                        0,0,xclient,yclient,&dummy_window);
  if (dummy_window == None) { /* Shouldn't happen, but if so, then punt */
    *xwhole = *xclient;  
    *ywhole = *yclient;
    return;
  }

  /* Now dummy_window should be a direct child of root, so find */
  /* its base coordinates.                                      */
  XTranslateCoordinates(display,dummy_window,DefaultRootWindow(display),
                        0,0,xwhole,ywhole,&dummy_window);

  /* And finally, subtract 1 for good luck ;-) */
  if ((*xwhole) > 0) {
    (*xwhole)--;
  }
  if ((*ywhole) > 0) {
    (*ywhole)--;
  }
}



/*
 *--------------------------------------------------------------
 *
 * WindowMapped --
 *
 *        Check event to see if window is mapped.  A procedure
 *        intended to be passed to XIfEvent().
 *
 * Results:
 *        Read the code.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

Bool WindowMapped(dsp,xev,window)
Display *dsp;
XEvent *xev;
char *window;
{
  if (xev->type == MapNotify && xev->xmap.event == *((Window *)window))
    return TRUE;
  return FALSE;
}

/*
 *--------------------------------------------------------------
 *
 * IfEventType --
 *
 *        Check event type.  A procedure intended to be passed to XIfEvent().
 *
 * Results:
 *        Read the code.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

Bool IfEventType(dsp,xev,type)
Display *dsp;
XEvent *xev;
char *type;
{
  if (xev->type == *((int *)type)) {
    return TRUE;
  }
  return FALSE;
}



/*
 *--------------------------------------------------------------
 * ShowHideControls---
 *
 *        Maps or unmaps control bar as dictated by the value of the
 *        global variable ControlShow.
 *
 * Results:
 *        None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
static void ShowHideControls()
{
  if (ControlShow == CTRLBAR_ON) {
    XEvent event;
    XMapRaised(display, ctrlwindow);
    XIfEvent(display, &event, WindowMapped, (char *)(&ctrlwindow));
                                               /* Wait for map. */
  }
  else {
    XUnmapWindow(display, ctrlwindow);
  }
}



/*
 *--------------------------------------------------------------
 *
 * MakeControlBar --
 *
 *        Creates and (possibly) displays Control Bar in root window
 *      at position (x,y) relative to video output window.
 *
 * Results:
 *        Sets ctrlwindow.
 *
 * Side effects:
 *        Sets ctrl_init to 1 if successful.
 *
 *--------------------------------------------------------------
 */
void MakeControlBar()
{
  char *ctrlname = "MPEG Player Controls";
  XSizeHints hint;
  int xpos, ypos;

  if (ctrl_init) {
    fprintf(stderr,
            "Warning from MakeControlBar(): Controls already initialized\n");
    return;
  }
  if (ditherType == NO_DITHER) return;
  if (display == (Display*)NULL) {
    fprintf(stderr,
      "Fatal error in MakeControlBar(): Display pointer not initialized\n");
    exit(1);
  }

  /* Get font (used in window sizing) */
  if ((ctrlfont = XLoadQueryFont(display, "fixed")) == NULL) {
    fprintf(stderr,"Error: Unable to load font \"fixed\" for Control Bar\n");
    exit(1);
  }
  fontheight = ctrlfont->ascent + ctrlfont->descent;
  fontwidth = ctrlfont->max_bounds.width;

  if (fontheight < 4 || fontheight > 75 || fontwidth < 2 || fontwidth > 30) {
    fprintf(stderr,"Warning: Font size seems suspect...guessing\n");
    fontheight = 20; 
    fontwidth = 10; /* Maybe 13 and 6 are better */
  }

  /* Set window sizes */
  ftw_height = CtrlBarHeight-2*(ChildBorder+ChildMargin);
                                             ftw_width = fontwidth * 21;
  rww_height = ftw_height;                   rww_width = fontwidth * 8;
  psw_height = rww_height;                   psw_width = fontwidth * 7;
  stw_height = psw_height;                   stw_width = fontwidth * 6;
  plw_height = stw_height;                   plw_width = fontwidth * 6;
  lpw_height = plw_height;                   lpw_width = fontwidth * 10;
  exw_height = lpw_height;                   exw_width = fontwidth * 6;
  CtrlBarWidth = rww_width + psw_width + stw_width + plw_width + lpw_width
    + exw_width + ftw_width + ChildMargin  
    + ChildCount*(ChildMargin + 2*ChildBorder);

  /* Figure out how to place control bar just above display window */
  GetWindowOrigins(window,&xpos,&ypos,&hint.x,&hint.y);

  /* Leave room for title bar decorations. Note this assumes */
  /* control bar dec. same height as for display window */
  hint.y = 2*hint.y - ypos; 

  hint.y -= (CtrlBarHeight + 2*CtrlBarBorder + 1);  /* +1 for luck ;-) */
  if (hint.y < 0) { 
    hint.y = 0;
  }
  hint.max_width  = hint.base_width  = hint.width = CtrlBarWidth;
  hint.max_height = hint.base_height = hint.height = CtrlBarHeight;
  hint.min_width  = hint.min_height  = 0;
  hint.flags =  PMinSize | PMaxSize | PBaseSize |
    PPosition | PSize | USPosition;

  screen = DefaultScreen (display);
  fgcolor = BlackPixel(display, screen);
  bgcolor = WhitePixel(display, screen);

  /* Create main control window */  
  ctrlwindow = XCreateSimpleWindow (display,DefaultRootWindow(display),
                                    hint.x, hint.y,
                                    hint.base_width, hint.base_height,
                                    CtrlBarBorder, fgcolor, bgcolor);

  /* Create child windows */
  xpos = ChildMargin;
  ypos = ChildMargin;
  frametotalwin = XCreateSimpleWindow (display, ctrlwindow, xpos, ypos,
             ftw_width, ftw_height, ChildBorder, fgcolor, bgcolor);
  xpos += ftw_width + 2*ChildBorder + ChildMargin;
  rewindwin = XCreateSimpleWindow (display, ctrlwindow, xpos, ypos,
             rww_width, rww_height, ChildBorder, fgcolor, bgcolor);
  xpos += rww_width + 2*ChildBorder + ChildMargin;
  pausewin = XCreateSimpleWindow (display, ctrlwindow, xpos, ypos,
             psw_width, psw_height, ChildBorder, fgcolor, bgcolor);
  xpos += psw_width + 2*ChildBorder + ChildMargin;
  stepwin = XCreateSimpleWindow (display, ctrlwindow, xpos, ypos,
             stw_width, stw_height, ChildBorder, fgcolor, bgcolor);
  xpos += stw_width + 2*ChildBorder + ChildMargin;
  playwin = XCreateSimpleWindow (display, ctrlwindow, xpos, ypos,
             plw_width, plw_height, ChildBorder, fgcolor, bgcolor);
  xpos += plw_width + 2*ChildBorder + ChildMargin;
  loopwin = XCreateSimpleWindow (display, ctrlwindow, xpos, ypos,
             lpw_width, lpw_height, ChildBorder, fgcolor, bgcolor);
  xpos += lpw_width + 2*ChildBorder + ChildMargin;
  exitwin = XCreateSimpleWindow (display, ctrlwindow, xpos, ypos,
             exw_width, exw_height, ChildBorder, fgcolor, bgcolor);

  /* Set up windows */
  XSelectInput(display, ctrlwindow, StructureNotifyMask);
  XSetStandardProperties(display, ctrlwindow, ctrlname, ctrlname, None,
                         NULL, 0, &hint);
  /*Safety*/
  XResizeWindow(display,ctrlwindow, hint. base_width, hint. base_height);

  XSelectInput(display, rewindwin, ExposureMask | ButtonPressMask);
  XSelectInput(display, pausewin,  ExposureMask | ButtonPressMask);
  XSelectInput(display, stepwin,   ExposureMask | ButtonPressMask);
  XSelectInput(display, playwin,   ExposureMask | ButtonPressMask);
  XSelectInput(display, loopwin,   ExposureMask | ButtonPressMask);
  XSelectInput(display, exitwin,   ExposureMask | ButtonPressMask);
  XSelectInput(display, frametotalwin, ExposureMask );

  /* Add "Delete" option to system menus */
  protocol_atom = XInternAtom(display,"WM_PROTOCOLS",False);
  delete_atom   = XInternAtom(display,"WM_DELETE_WINDOW",False);
  if ( protocol_atom != (Atom)None && delete_atom != (Atom)None) {
    XSetWMProtocols(display, window, &delete_atom, 1);      /* Video window */
    XSetWMProtocols(display, ctrlwindow, &delete_atom, 1);  /* Control bar */
  }

  /* Map windows (or not ;-) */
  XMapSubwindows(display, ctrlwindow);
  ShowHideControls();

  ctrl_init = 1;  /* Control Bar successfully initialized */
}



/*
 *--------------------------------------------------------------
 *
 * Child window draw functions --
 *
 *        (Re)draws child window
 *
 * Results:
 *        None.
 *
 * Side effects:
 *        None.
 *
 *--------------------------------------------------------------
 */

static void WriteCenteredText(win,gc,winwidth,winheight,str)
Window win;
GC gc;
int winwidth, winheight;
char *str;
{
  int xpos, ypos, len;
  len = strlen(str);
  ypos = (winheight + ctrlfont->ascent)/2 - 1; /* Ignore descenders */
  xpos = (winwidth - len*fontwidth)/2;
  XDrawImageString(display, win, gc, xpos, ypos, str, len);
}

static void ExposeWindow(win)
Window win;
{
  static XEvent event;
  event.xexpose.type = Expose;
  event.xexpose.serial = 0;
  event.xexpose.send_event = TRUE;
  event.xexpose.display = display;
  event.xexpose.window = win;
  event.xexpose.x = 0;      
  event.xexpose.y = 0;
  event.xexpose.width = 0;
  event.xexpose.height = 0;
  event.xexpose.count = 0;
  XSendEvent(display, win, FALSE, ExposureMask, &event);
}

static void NormalColors(gc)
GC gc;
{
  XSetForeground(display, gc, fgcolor);
  XSetBackground(display, gc, bgcolor);  
}

static void InvertColors(gc)
GC gc;
{
  XSetForeground(display, gc, bgcolor);
  XSetBackground(display, gc, fgcolor);  
}

static void DrawRewind(gc)
GC gc;
{
  if (ControlState == CTRL_REWIND) {
    XFillRectangle(display, rewindwin, gc, 0, 0, rww_width, rww_height);
    InvertColors(gc);
    WriteCenteredText(rewindwin, gc, rww_width, rww_height, "Rewind");
    NormalColors(gc);
  }
  else {
    WriteCenteredText(rewindwin, gc, rww_width, rww_height, "Rewind");
  }
}

static void DrawPause(gc)
GC gc;
{
  if (ControlState == CTRL_EOF || ControlState == CTRL_FFWD 
     || ControlState == CTRL_PAUSE) {
    XFillRectangle(display,pausewin,gc,0,0,psw_width,psw_height);
    InvertColors(gc);
    switch(ControlState) {
    case CTRL_EOF:
      WriteCenteredText(pausewin,gc,psw_width,psw_height,"End");
      break;
    case CTRL_FFWD:
      WriteCenteredText(pausewin,gc,psw_width,psw_height,"Wait");
      break;
    default:
      WriteCenteredText(pausewin,gc,psw_width,psw_height,"Pause");
      break;
    }
    NormalColors(gc);
  }
  else {
    WriteCenteredText(pausewin,gc,psw_width,psw_height,"Pause");
  }
}

static void DrawStep(gc)
GC gc;
{
  if (ControlState == CTRL_STEP) {
    XFillRectangle(display, stepwin, gc, 0, 0, stw_width, stw_height);
    InvertColors(gc);
  }
  WriteCenteredText(stepwin, gc, stw_width, stw_height, "Step");
  if (ControlState == CTRL_STEP) {
    NormalColors(gc);
  }
}

static void DrawPlay(gc)
GC gc;
{
  if (ControlState == CTRL_PLAY ||
     (ControlState == CTRL_EOF && ControlMotion == CTRLMOTION_ON)) {
    XFillRectangle(display, playwin, gc, 0, 0, plw_width, plw_height);
    InvertColors(gc);
    WriteCenteredText(playwin, gc, plw_width, plw_height, "Play");
    NormalColors(gc);
  }
  else {
    WriteCenteredText(playwin, gc, plw_width, plw_height, "Play");
  }
}

static void DrawLoop(gc)
GC gc;
{
  if (loopFlag) {
    XFillRectangle(display, loopwin, gc, 0, 0, lpw_width, lpw_height);
    InvertColors(gc);
    WriteCenteredText(loopwin, gc, lpw_width, lpw_height, "Loop ON");
    NormalColors(gc);
  }
  else WriteCenteredText(loopwin, gc, lpw_width, lpw_height, "Loop OFF");
}

static void DrawExit(gc)
GC gc;
{
  if (ControlState == CTRL_EXIT) {
    XFillRectangle(display,exitwin, gc, 0, 0, exw_width, exw_height);
    InvertColors(gc);
  }
  WriteCenteredText(exitwin, gc, exw_width, exw_height, "Exit");
  if (ControlState == CTRL_EXIT) {
    NormalColors(gc);
  }
}

void UpdateFrameTotal()
{
  if (!ctrl_init) {
    return;
  }
  ExposeWindow(frametotalwin);
}

void UpdateControlDisplay()
{
  if (!ctrl_init) {
    return;
  }
  ExposeWindow(rewindwin);
  ExposeWindow(pausewin);
  ExposeWindow(stepwin);
  ExposeWindow(playwin);
  ExposeWindow(loopwin);
  ExposeWindow(exitwin);
  ExposeWindow(frametotalwin);
}

static void DrawFrameTotal(gc)
GC gc;
{
  char str[32];
  double dtemp;
  dtemp = StopWatch(STOPWATCH_READ);
  if (dtemp > 0.0) {
    sprintf(str, "Frame/Rate%4d/%4.1f", totNumFrames, TotalFrameCount/dtemp);
  }
  else {
    sprintf(str, "Frame/Rate%4d/----", totNumFrames);
  }
  WriteCenteredText(frametotalwin, gc, ftw_width, ftw_height, str);
}

static GC CreateCtrlGC()
{
  XGCValues gcv;
  GC gc;
  gcv.foreground = BlackPixel(display, screen);
  gcv.background = WhitePixel(display, screen);
  gcv.font = ctrlfont->fid;
  gcv.subwindow_mode = ClipByChildren;
/*  gcv.subwindow_mode = IncludeInferiors; */
  gc=XCreateGC(display, ctrlwindow,
               GCForeground|GCBackground|GCFont|GCSubwindowMode, &gcv);
  return gc;
}


/*
 *--------------------------------------------------------------
 *
 * GetStateButton --
 *
 *        Determines the window of the button associated with the
 *        import ctrlstate.
 *
 * Results:
 *        The associated window, if any; Otherwise 0.
 *
 * Side effects:
 *        None.
 *
 *--------------------------------------------------------------
 */

static Window GetStateButton(ctrlstate)
int ctrlstate;
{
  switch(ctrlstate) {
    case CTRL_REWIND:
      return rewindwin;
    case CTRL_PAUSE:
    case CTRL_FFWD:
    case CTRL_EOF:
      return pausewin;
    case CTRL_STEP:
      return stepwin;
    case CTRL_PLAY:
      return playwin;
    case CTRL_EXIT:
      return exitwin;
    default:
      break;
    }
  return (Window)0;
}


/*
 *--------------------------------------------------------------
 *
 * DrawButton --
 *
 *        Calls the draw button function associated with the import
 *        window button.
 *
 * Results:
 *        None.
 *
 * Side effects:
 *        None.
 *
 *--------------------------------------------------------------
 */

static void DrawButton(button,gc)
Window button;
GC gc;
{
  if (button == frametotalwin)  DrawFrameTotal(gc);
  else if (button == rewindwin) DrawRewind(gc);
  else if (button == pausewin)  DrawPause(gc);
  else if (button == stepwin)   DrawStep(gc);
  else if (button == playwin)   DrawPlay(gc);
  else if (button == loopwin)   DrawLoop(gc);
  else if (button == exitwin)   DrawExit(gc);
  else fprintf(stderr, "DrawButton called with unknown button\n");
}


/*
 *--------------------------------------------------------------
 *
 * ControlBar --
 *
 *        Checks XEvent queue for control commands.
 *
 * Results:
 *        Adjusts global variables ControlState, loopFlag as appropriate.
 *
 * Side effects:
 *        May adjust StopWatch state.
 *
 *--------------------------------------------------------------
 */

void ControlBar()
{
  GC gc;
  int gcflag;
  Window oldbutton, newbutton;
  XEvent event;
  static int LastState = CTRL_UNDEFINED;

  gcflag = 0;

  /* Check to see if ControlState was modified outside this function, */
  /* and update control displays if it was.                           */
  if (LastState != ControlState) {
    if (!gcflag) { 
      gc = CreateCtrlGC(); 
      gcflag = 1; 
    }
    if ((oldbutton = GetStateButton(LastState)) != (Window)NULL) {
      XClearWindow(display, oldbutton);
      DrawButton(oldbutton, gc);
      if (LastState == CTRL_EOF) {
        XClearWindow(display, playwin);
        DrawButton(playwin, gc);
      }
    }    
    DrawButton(GetStateButton(LastState = ControlState), gc);
  }

  /* Process events, if any */
  if (XPending(display) < 1) { /* No events */
    if (gcflag) { 
      XFreeGC(display, gc);
    }
    LastState = ControlState;
    return;
  }

  if (!gcflag) { 
    gc=CreateCtrlGC();
    gcflag=1;
  }
  do {
    XNextEvent(display, &event);
#ifdef HAVE_XFILTEREVENT /* Define if needed; Some older X's don't have this */
    if (XFilterEvent(&event, ctrlwindow)) continue;
#endif
    switch(event.type) {
      case ButtonPress:
        /* Toggle Buttons */
        if (event.xbutton.window == loopwin) {
          if (loopFlag) { 
            XClearWindow(display,loopwin); 
            loopFlag = 0;
          }
          else {
            loopFlag = 1;
            if (ControlState == CTRL_EOF && ControlMotion == CTRLMOTION_ON) {
              ControlState = CTRL_REWIND;
              DrawRewind(gc);
              XClearWindow(display, playwin);  
              DrawPlay(gc);
              XClearWindow(display, pausewin); 
              DrawPause(gc);
            }
          }
          DrawLoop(gc);
          break;
        }
        else if (event.xbutton.window == window) { /* Click in display window */
          if (ControlShow) {
            ControlShow = CTRLBAR_OFF;
          } else {
            ControlShow = CTRLBAR_ON;
          }
          ShowHideControls();
          break;
        }
        /* ControlState buttons --- */
        /* Get currently selected button */
        oldbutton = GetStateButton(ControlState);
        /* Update state */
        if (event.xbutton.window == pausewin)  {
          if ((ControlState == CTRL_EOF || ControlState == CTRL_FFWD)
             && ControlMotion == CTRLMOTION_ON) {
            ControlMotion = CTRLMOTION_OFF;
            XClearWindow(display, playwin); 
            DrawPlay(gc);
          }
          else if (ControlState == CTRL_PLAY) {
            ControlMotion = CTRLMOTION_OFF;
            ControlState = CTRL_PAUSE;
          }
          else if (ControlState == CTRL_PAUSE) {
            ControlMotion = CTRLMOTION_ON; 
            ControlState = CTRL_PLAY;
          }
        }
        else if (event.xbutton.window == stepwin) {
          if (ControlState == CTRL_PAUSE || ControlState == CTRL_PLAY)
            ControlState = CTRL_STEP;
          else if (ControlState == CTRL_EOF && loopFlag)
            ControlState = CTRL_REWIND;
          ControlMotion = CTRLMOTION_OFF;
        }
        else if (event.xbutton.window == playwin) {
          ControlMotion = CTRLMOTION_ON; 
          if (ControlState == CTRL_EOF) {
            if (loopFlag) {
              ControlState = CTRL_REWIND;
            }
            DrawButton(playwin, gc);
          }
          else if (ControlState == CTRL_PAUSE) {
            ControlState = CTRL_PLAY;
          }
        }
        else if (event.xbutton.window == rewindwin) {
          ControlState = CTRL_REWIND;
         }
        else if (event.xbutton.window == exitwin) {
          ControlState = CTRL_EXIT;
        }
        /* Get newly selected button */
        newbutton = GetStateButton(ControlState);
        if (LastState == ControlState) break;
        /* Adjust stopwatch */
        if (LastState == CTRL_PLAY)
          StopWatch(STOPWATCH_STOP);  /* Stop playing */
        else if (ControlState == CTRL_PLAY)
          StopWatch(STOPWATCH_START); /* Start playing */
        /* Update button display */
        if (oldbutton != (Window)NULL) {
          XClearWindow(display,oldbutton);
          DrawButton(oldbutton,gc);
        }
        DrawButton(newbutton,gc);
        break;
      case ClientMessage:
        if (event.xclient.message_type != protocol_atom ||
            event.xclient.data.l[0] != delete_atom      ||
            event.xclient.format != 32) break; /* Not WM_DELETE_WINDOW */
        /* Otherwise drop through to DestroyNotify */
      case DestroyNotify:
        ControlState=CTRL_EXIT;
        break;
      case Expose:
        if (event.xexpose.count > 0) { 
          break; /* Wait for last expose event */
        }
        if (event.xexpose.window == window) {
	  if ( curVidStream->current != NULL) {	  
	    ExecuteDisplay(curVidStream,0);
	  }
        }
        else { 
          DrawButton(event.xexpose.window,gc);
        }
        break;
      default:
        break;
    }
  } while (XPending(display) > 0);
  if (gcflag) {
    XFreeGC(display, gc);
  }
  LastState = ControlState;
}


/*
 *--------------------------------------------------------------
 *
 * ControlLoop --
 *
 *        Main control loop.  Intermixes video processing (as
 *        determined by ControlState) with user input.
 *
 * Results:
 *        None.
 *
 * Side effects:
 *        Adjusts StopWatch state, totNumFrames, ControlState,
 *        FilmState and EOF_flag.  May also reset video state.
 *
 *--------------------------------------------------------------
 */

void ControlLoop(vid_stream)
VidStream *vid_stream;
{
  int itemp;
  XEvent event;

  if (!ctrl_init) {
    fprintf(stderr,"ControlLoop() accessed without initialization\n");
    exit(1);
  }
  StopWatch(STOPWATCH_RESET);
  TotalFrameCount = 0;
  StopWatch(STOPWATCH_START);
  itemp = totNumFrames;
  while (totNumFrames == itemp)
    mpegVidRsrc(0, vid_stream, 0); /* Advance to 1st frame */
  if (startFrame > totNumFrames) {
    ControlState = CTRL_FFWD; 
    ControlMotion = CTRLMOTION_OFF;
    StopWatch(STOPWATCH_STOP);
  }
  else if (ControlShow != CTRLBAR_ON || noDisplayFlag) {
    ControlState = CTRL_PLAY;  
    ControlMotion = CTRLMOTION_ON;
  }
  else {
    ControlState = INITIAL_STATE;
    ControlMotion = CTRLMOTION_OFF;
    if (ControlState == CTRL_PAUSE) {
      StopWatch(STOPWATCH_STOP);
    } else {
      StopWatch(STOPWATCH_START);
    }
  }

  while (1) {
    ControlBar();
    if (ControlState == CTRL_PAUSE || ControlState == CTRL_EOF) {
      XPeekEvent(display, &event);  /* Block until user input */
    }
    else {
      switch(ControlState) {
      case CTRL_PLAY:
        mpegVidRsrc(0, vid_stream,0);
        if (FilmState == FILM_END) { /* Film end just reached */
          StopWatch(STOPWATCH_STOP);
          XSync(display, FALSE); /* Kludge to update frametotalwin */
          if (loopFlag) {
            ControlState = CTRL_REWIND;
          }
          else {
            ControlState = CTRL_EOF;
          }
        }
        break;
      case CTRL_FFWD:
        mpegVidRsrc(0, vid_stream, 0);
        if (startFrame <= totNumFrames) {
          if (ControlMotion == CTRLMOTION_ON) {
            ControlState = CTRL_PLAY; 
            StopWatch(STOPWATCH_START);
          }
          else {
            ControlState = CTRL_PAUSE;
          }
        }
        if (FilmState == FILM_END) { /* Film end just reached---Degenerate case */
          StopWatch(STOPWATCH_STOP);
          if (loopFlag) { 
            ControlState = CTRL_REWIND;
          }
          else {
            ControlState = CTRL_EOF;
          }
        }
        break;
      case CTRL_EOF:
        if (loopFlag) {
          ControlState = CTRL_REWIND;
        }
        break;
      case CTRL_STEP:
        StopWatch(STOPWATCH_START);
        itemp = totNumFrames;                /* Advance 1 frame */
        while (totNumFrames == itemp) {
          mpegVidRsrc(0, vid_stream,0);
        }
        StopWatch(STOPWATCH_STOP);
        ControlState = CTRLMOTION_OFF;
        if (FilmState == FILM_END) /* Film end just reached */
          ControlState = CTRL_EOF;
        else
          ControlState = CTRL_PAUSE;
        break;
      case CTRL_REWIND:
        StopWatch(STOPWATCH_STOP);
        rewind(input);
        EOF_flag = 0;   
        FilmState = FILM_NORMAL;  
        ControlBar();
        ResetVidStream(vid_stream);  /* Reinitialize vid_stream pointers */
        totNumFrames = 0;
        if (seekValue < 0) {
          seekValue = 0 - seekValue;
        }
        mpegVidRsrc(0, vid_stream, 1); /* Process start codes */
        if (startFrame > 0) {
          ControlState = CTRL_FFWD;
        }
        else {
          realTimeStart = ReadSysClock(); /* Time like original pass */
          StopWatch(STOPWATCH_START);
 
            /* Do 1st frame */
            while (totNumFrames == 0) {
              mpegVidRsrc(0, vid_stream, 0); 
            }

            if (ControlMotion == CTRLMOTION_ON) {
              ControlState = CTRL_PLAY;
            } else {
              ControlState = CTRL_PAUSE; 
              StopWatch(STOPWATCH_STOP);
            }
          }
        break;
      case CTRL_EXIT:
        if (ctrlfont != NULL) {
          XFreeFont(display, ctrlfont);
        }
        XDestroyWindow(display, ctrlwindow);
        return;
      default:
        fprintf(stderr,"Code error: Illegal control state: %d (main())",
                ControlState);
        exit(1);
        break;
      }
    }
  }
}
#endif /* !NOCONTROLS */
