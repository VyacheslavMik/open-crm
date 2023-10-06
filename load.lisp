(push (directory-namestring *load-truename*) asdf:*central-registry*)
(asdf:load-system "open-crm")
(server.core::start)
