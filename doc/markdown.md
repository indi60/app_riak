how to view markdown

sudo -v && wget -nv -O- https://raw.githubusercontent.com/kovidgoyal/calibre/master/setup/linux-installer.py | sudo python -c "import sys; main=lambda:sys.stderr.write('Download failed\n'); exec(sys.stdin.read()); main()"

run: ebook-viewer myfile.md.

source: http://stackoverflow.com/questions/9843609/view-markdown-files-offline