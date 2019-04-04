# My-Book-Shelf

My first Common Lisp application.

This app use as follows.

- SQLite3
- roswell(SBCL)
- cl-dbi

## Usage

- Clone this repository. 
- Put in your `/.roswell/local-projects/`

    $ ros run

    * (ql:quickload :my-book-shelf)
    * (in-package :my-book-shelf)
    * (init-db)

- `init-db` is function to create new `books.db` in current directory.

    * (setf x (make-instance 'book :title "Foo" :author "Bar"))
    * (add-book x)
    * (show-all)
    title: Foo
    author: Bar
    publisher: Unknown
    isbn: 9784000000000
    * (search-author "Bar")
    title: Foo
    author: Bar
    publisher :Unknown
    isbn: 9784000000000

## License

Licensed under the LLGPL License.
