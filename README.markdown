# SBCL Speedrun Timer
This is a complete rewrite of my first Lisp project: a speedrun timer. It uses the amazing ncurses wrapper library [croatoan](https://github.com/McParen/croatoan) and a SQLite database - with the [MITO ORM](https://github.com/fukamachi/mito).

https://user-images.githubusercontent.com/25559600/204612689-05c2bbab-5130-43b0-8b09-f3a9583c506b.mp4

## Requirements
+ [Quicklisp](https://www.quicklisp.org/beta/)
+ [SBCL](http://www.sbcl.org/platform-table.html)
+ [SQLite](https://www.sqlite.org/download.html)

## Usage
1. Load the package: `sbcl --load lispruns.asd`
2. Get dependencies: `(ql:quickload 'lispruns)`
3. `(main)`
