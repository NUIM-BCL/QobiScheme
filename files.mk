OBJECTS = \
	source/QobiScheme.o \
	$(C-OBJECTS)


AD-OBJECTS = \
	source/QobiScheme-AD.o \
	$(C-OBJECTS)


C-OBJECTS = \
	$(ENCODE-OBJECTS) \
	$(JPEG-OBJECTS) \
	$(PLAY-OBJECTS) \
	$(VANILLA-OBJECTS)


ENCODECFLAGS = -I/usr/include -O2
ENCODE-OBJECTS = \
	source/bframe.o \
	source/bitio.o \
	source/block.o \
	source/bsearch.o \
	source/combine.o \
	source/encode_api.o \
	source/encode_jrevdct.o \
	source/frame.o \
	source/frametype.o \
	source/fsize.o \
	source/huff.o \
	source/iframe.o \
	source/libpnmrw.o \
	source/mfwddct.o \
	source/mheaders.o \
	source/mpeg.o \
	source/nojpeg.o \
	source/noparallel.o \
	source/opts.o \
	source/param.o \
	source/pframe.o \
	source/postdct.o \
	source/psearch.o \
	source/rate.o \
	source/readframe.o \
	source/rgbtoycc.o \
	source/specifics.o \
	source/subsample.o


PLAYCFLAGS = -O2 -DNDEBUG -DNONANSI_INCLUDES -I/usr/include -I/usr/openwin/include
PLAY-OBJECTS = \
	source/16bit.o \
	source/2x2.o \
	source/ctrlbar.o \
	source/decoders.o \
	source/floatdct.o \
	source/fs2.o \
	source/fs2fast.o \
	source/fs4.o \
	source/gdith.o \
	source/gray.o \
	source/hybrid.o \
	source/hybriderr.o \
	source/mb_ordered.o \
	source/mono.o \
	source/motionvector.o \
	source/ordered.o \
	source/ordered2.o \
	source/parseblock.o \
	source/play_api.o \
	source/play_jrevdct.o \
	source/readfile.o \
	source/util.o \
	source/util32.o \
	source/video.o


# this may contain files which are not *actually* meant to be built
# based on the original list of object files in source/makefile; however,
# there are rules to build all of these...
VANILLA-OBJECTS = \
	$(USLEEP) \
	source/dither.o \
	source/nobuff.o \
	source/os_dep.o \
	source/xflush.o


JPEGCFLAGS = -O2 -DINLINE_YES

# this may contain files which are not *actually* meant to be built
# based on the original list of object files in source/makefile; however,
# there are rules to build all of these...
JPEG-OBJECTS = \
	source/cdjpeg.o \
	source/cjpeg.o \
	source/djpeg.o \
	source/jcapimin.o \
	source/jcapistd.o \
	source/jccoefct.o \
	source/jccolor.o \
	source/jcdctmgr.o \
	source/jchuff.o \
	source/jcinit.o \
	source/jcmainct.o \
	source/jcmarker.o \
	source/jcmaster.o \
	source/jcomapi.o \
	source/jcparam.o \
	source/jcphuff.o \
	source/jcprepct.o \
	source/jcsample.o \
	source/jctrans.o \
	source/jdapimin.o \
	source/jdapistd.o \
	source/jdatadst.o \
	source/jdatasrc.o \
	source/jdcoefct.o \
	source/jdcolor.o \
	source/jddctmgr.o \
	source/jdhuff.o \
	source/jdinput.o \
	source/jdmainct.o \
	source/jdmarker.o \
	source/jdmaster.o \
	source/jdmerge.o \
	source/jdphuff.o \
	source/jdpostct.o \
	source/jdsample.o \
	source/jdtrans.o \
	source/jerror.o \
	source/jfdctflt.o \
	source/jfdctfst.o \
	source/jfdctint.o \
	source/jidctflt.o \
	source/jidctfst.o \
	source/jidctint.o \
	source/jidctred.o \
	source/jmemmgr.o \
	source/jmemnobs.o \
	source/jpegtran.o \
	source/jquant1.o \
	source/jquant2.o \
	source/jutils.o \
	source/rdbmp.o \
	source/rdcolmap.o \
	source/rdgif.o \
	source/rdjpgcom.o \
	source/rdppm.o \
	source/rdrle.o \
	source/rdswitch.o \
	source/rdtarga.o \
	source/wrbmp.o \
	source/wrgif.o \
	source/wrjpgcom.o \
	source/wrppm.o \
	source/wrrle.o \
	source/wrtarga.o

JPEG-DOS-OBJECTS = \
	source/jmemdos.o
