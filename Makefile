# $Id$
# Edit the directories to show where you want things installed:

# You probably want this:
# lispdir = /usr/share/emacs/site-lisp/notebook
# infodir = /usr/share/info
# But I use this:
lispdir = $(HOME)/Emacs
infodir = $(HOME)/Emacs/info

# This should change after an upload.
VERSION = 1.4

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
	@echo "@set VERSION $(VERSION)" >> version.texi

notebook.info: notebook.texi version.texi
	makeinfo $<

clean:
	rm -rf notebook.dvi notebook.info $(distdir) 
	rm -rf samp5.tex samp5.dvi samp5.log samp5.aux


## The stuff below is for developers only:
dist: notebook-$(VERSION).tgz

distdir = notebook-$(VERSION)

notebook-$(VERSION).tgz: $(DISTFILES)
	-rm -rf $(distdir)
	mkdir $(distdir)
	cp $(DISTFILES) $(distdir)
	tar -czf $@ $(distdir)

sourceforge: notebook-$(VERSION).tgz
	scp notebook.html fredgc@shell.sourceforge.net:/home/groups/n/no/notebook/htdocs/index.html
	ncftpput -p  fredgc@users.sourceforge.net  upload.sourceforge.net /incoming notebook-$(VERSION).tgz


