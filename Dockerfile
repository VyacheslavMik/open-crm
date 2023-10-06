FROM postgres:11.2

RUN rm -r /etc/apt/sources.list.d/pgdg.list
RUN echo 'deb http://archive.debian.org/debian stretch main' > /etc/apt/sources.list
RUN apt-get update && apt-get -y upgrade

RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && locale-gen

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN apt-get install -y wget make libssl1.0.2 screen gconf-service libasound2 libatk1.0-0 libc6 libcairo2 libcups2 libdbus-1-3 libexpat1 libfontconfig1 libgcc1 libgconf-2-4 libgdk-pixbuf2.0-0 libglib2.0-0 libgtk-3-0 libnspr4 libpango-1.0-0 libpangocairo-1.0-0 libstdc++6 libx11-6 libx11-xcb1 libxcb1 libxcomposite1 libxcursor1 libxdamage1 libxext6 libxfixes3 libxi6 libxrandr2 libxrender1 libxss1 libxtst6 ca-certificates fonts-liberation libappindicator1 libnss3 lsb-release xdg-utils

RUN wget http://prdownloads.sourceforge.net/sbcl/sbcl-1.4.16-x86-64-linux-binary.tar.bz2
RUN tar xjf sbcl-1.4.16-x86-64-linux-binary.tar.bz2
WORKDIR /sbcl-1.4.16-x86-64-linux
RUN ./install.sh
WORKDIR /
RUN wget https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp --eval '(progn (quicklisp-quickstart:install) (exit))'
RUN echo '#-quicklisp \
    	  (let ((quicklisp-init (merge-pathnames "/root/quicklisp/setup.lisp" \
	                                         (user-homedir-pathname)))) \
		   (when (probe-file quicklisp-init) \
		     (load quicklisp-init)))' > /root/.sbclrc
RUN sbcl --eval '(progn (ql:quickload "hunchentoot") \
			(ql:quickload "parenscript") \
			(ql:quickload "cl-who") \
			(ql:quickload "postmodern") \
			(ql:quickload "cl-json") \
			(ql:quickload "uuid") \
			(ql:quickload "ironclad") \
			(ql:quickload "cl-ppcre") \
    	 		(ql:quickload "lass") \
			(ql:quickload "cl-smtp") \
			(ql:quickload "clip") \
			(ql:quickload "flexi-streams") \
			(ql:quickload "drakma") \
			(ql:quickload "cl-mustache") \
			(ql:quickload "clss") \
			(ql:quickload "plump") \
			(ql:quickload "str") \
                        (exit))'
RUN mkdir /data
RUN chown postgres /data
USER root
RUN mkdir open-crm

WORKDIR open-crm

ADD . /open-crm
RUN echo '(push #p"/open-crm/" asdf:*central-registry*) \
    	  (asdf:load-system "open-crm")' > /open-crm/load.lisp

RUN mv open-crm.cldn $HOME/open-crm.cldn

ENV NO_SANDBOX true

ENTRYPOINT ["/open-crm/entrypoint.sh"]