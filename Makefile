# $Id$
# Edit the directories to show where you want things installed:

# You probably want this:
# lispdir = /usr/share/emacs/site-lisp/notebook
# infodir = /usr/share/info
# But I use this:
lispdir = $(HOME)/Emacs
infodir = $(HOME)/Emacs/info

# This should change after an upload.
# Note to myself: change these here and in notebook-mode.el -- FDGC
VERSION = 2.1

LISP = notebook-mode.el \
	matlab-notebook-mode.el mupad-notebook-mode.el octave-notebook-mode.el \

SAMPLES = samp4.shell samp5.matlab samp5-b.matlab samp6.shell 

DISTFILES = $(LISP) $(SAMPLES) COPYING INSTALL Makefile notebook.info notebook.texi \
	README TODO debug-notebook.el version.texi samp8.octave samp4.shell

all: notebook.info

install: notebook.info
	mkdirhier $(lispdir)
	install $(LISP) $(lispdir)
	install notebook.info $(infodir)
	install-info $(infodir)/notebook.info  $(infodir)/dir

version.texi: Makefile 
	@date +"@set UPDATED %d %B %Y" > version.texi
	@echo "@set VERSION $(VERSION)" >> version.texi

notebook.info: notebook.texi version.texi
	makeinfo $<

clean:
	rm -rf notebook.dvi notebook.info $(distdir) 
	rm -rf samp5.tex *.dvi *.log *.aux x-*.tex x-*.html x-*.txt


## The stuff below is for developers only:
dist: notebook-$(VERSION).tgz

distdir = notebook-$(VERSION)

notebook-$(VERSION).tgz: $(DISTFILES)
	-rm -rf $(distdir)
	mkdir $(distdir)
	cp $(DISTFILES) $(distdir)
	tar -czf $@ $(distdir)

# I use this to release a new version.  
# It should only be used by the maintainer -- Fred GC.
sourceforge: notebook-$(VERSION).tgz
	scp notebook.html fredgc@shell.sourceforge.net:/home/groups/n/no/notebook/htdocs/index.html
	ncftpput -p  fredgc@users.sourceforge.net  upload.sourceforge.net /incoming notebook-$(VERSION).tgz
	cp notebook.html $(HOME)/public_html/ktb/notebook.html
