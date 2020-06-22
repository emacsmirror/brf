#
# Create the Brf-mode Info manual from README.md
# Open the Info file in Emacs if successful
#
brf-mode.info: README.texi
	makeinfo README.texi
	emacsclient -q -e "(info \"./brf-mode.info\")"

README.texi: README.md
	pandoc --read=gfm --write=texinfo --output=README.texi --standalone --include-before-body=manual/header.texi README.md
	patch -i manual/README.texi.patch
