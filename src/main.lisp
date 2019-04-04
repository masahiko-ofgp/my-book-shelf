(defpackage my-book-shelf
  (:use :cl
        :cl-dbi)
  (:export :init-db
           :drop-tb
           :add-book
           :show-all
           :search-author
           :book))
(in-package :my-book-shelf)

;;; Create books.db.
(defun init-db ()
  (dbi:with-connection 
    (conn :sqlite3
          :database-name #p"books.db")
    (dbi:do-sql conn "CREATE TABLE books (\
    id INTEGER PRIMARY KEY AUTOINCREMENT, \
    title VARCHAR(80), \
    author VARCHAR(50), \
    publisher VARCAR(50), \
    isbn INTEGER)")))

;;; Drop table
(defun drop-tb ()
  (dbi:with-connection
    (conn :sqlite3
          :database-name #p"books.db")
    (dbi:do-sql conn "DROP TABLE books")))

;;; If you want to add new book,
;;; you must do make-instance 'book.
;;; Then you use add-book.
(defclass book ()
  ((title :initarg :title
          :initform (error "Required")
          :accessor title)
   (author :initarg :author
           :initform "Unknown"
           :accessor author)
   (publisher :initarg :publisher
              :initform "Unknown"
              :accessor publisher)
   (isbn :initarg :isbn
         :initform 9784000000000
         :accessor isbn)))

;;; Add new book data to database.
(defun add-book (book)
  (with-slots (title author publisher isbn)
    book
    (dbi:with-connection 
      (conn :sqlite3
            :database-name #p"books.db")
      (dbi:do-sql conn (format nil "INSERT INTO books \
                               (title, author, publisher, isbn) \
                               VALUES ('~a', '~a', '~a', '~a')" 
                               title author publisher isbn)))))

;;; Show all data.
(defun show-all ()
  (dbi:with-connection
      (conn :sqlite3
            :database-name #p"books.db")
    (let* ((query (dbi:prepare conn "SELECT * FROM books")) 
           (result (dbi:execute query)))
      (loop for row = (dbi:fetch result)
            while row
            do (format t "~{~a: ~a~%~}~%" row)))))

(defun search-author (author-name)
  (dbi:with-connection
    (conn :sqlite3
          :database-name #p"books.db")
    (let* ((query (dbi:prepare conn 
                               (format nil "SELECT * FROM books \
                                       WHERE author='~a'" author-name)))
           (result (dbi:execute query)))
      (loop for row = (dbi:fetch result)
            while row
            do (format t "~{~a: ~a~%~}~%" row)))))
