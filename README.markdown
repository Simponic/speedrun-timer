# SBCL Speedrun Timer
This is a complete rewrite of my first Lisp project: a speedrun timer. It uses the amazing ncurses wrapper library (croatoan)[https://github.com/McParen/croatoan] and a SQLite database - with the (MITO ORM)[https://github.com/fukamachi/mito].

## Requirements
+ (Quicklisp)[https://www.quicklisp.org/beta/]
+ (SBCL)[http://www.sbcl.org/platform-table.html]
+ (SQLite)[[https://www.sqlite.org/download.html]

## Usage
1. Load the package ~sbcl --load lispruns.asd~
2. Load it with quicklisp: ~(ql:quickload 'lispruns)~
3. ~(main)~
