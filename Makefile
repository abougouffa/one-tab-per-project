EMACS = emacs


README.md: make-readme-markdown.el one-tab-per-project.el
	$(EMACS) --script $< <one-tab-per-project.el >$@ 2>/dev/null

make-readme-markdown.el:
	wget -q -O $@ https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el

.INTERMEDIATE: make-readme-markdown.el
