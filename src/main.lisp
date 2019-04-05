(defpackage my-book-shelf
  (:use :cl
        :cl-dbi)
  (:export :init-db
           :drop-tb
           :add-book
           :show-all
           :search-book
           :book))
(in-package :my-book-shelf)


;;; Create books.db.
(defun init-db ()
  (dbi:with-connection 
    (conn :sqlite3
          :database-name #p"books.db")
    (dbi:do-sql conn "CREATE TABLE books (\
    id INTEGER PRIMARY KEY AUTOINCREMENT, \
    title VARCHAR(80) NOT NULL, \
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


;;; Search title
(defun search-title (title-name)
  (dbi:with-connection
    (conn :sqlite3
          :database-name #p"books.db")
    (let* ((query (dbi:prepare conn "SELECT * FROM books WHERE title = ?"))
           (result (dbi:execute query title-name)))
      (loop for row = (dbi:fetch result)
            while row
            do (format t "~{~a: ~a~%~}~%" row)))))


;;; Search author
(defun search-author (author-name)
  (dbi:with-connection
    (conn :sqlite3
          :database-name #p"books.db")
    (let* ((query (dbi:prepare conn "SELECT * FROM books WHERE author = ?"))
           (result (dbi:execute query author-name)))
      (loop for row = (dbi:fetch result)
            while row
            do (format t "~{~a: ~a~%~}~%" row)))))


;;; Search publisher
(defun search-publisher (publisher-name)
  (dbi:with-connection
    (conn :sqlite3
          :database-name #p"books.db")
    (let* ((query (dbi:prepare conn "SELECT * FROM books WHERE publisher = ?"))
           (result (dbi:execute query publisher-name)))
      (loop for row = (dbi:fetch result)
            while row
            do (format t "~{~a: ~a~%~}~%" row)))))


;;; Search ISBN
(defun search-isbn (isbn-num)
  (if (not (typep isbn-num 'integer))
      (error "Not number")
      (dbi:with-connection
          (conn :sqlite3
                :database-name #p"books.db")
        (let* ((query (dbi:prepare conn "SELECT * FROM books WHERE isbn = ?"))
               (result (dbi:execute query isbn-num)))
          (loop for row = (dbi:fetch result)
             while row
             do (format t "~{~a: ~a~%~}~%" row))))))

;;; You can search by specifying the key(title, author, publisher, isbn)
(defun search-book (key value)
  (cond
    ((string= key "title") (search-title value))
    ((string= key "author") (search-author value))
    ((string= key "publisher") (search-publisher value))
    ((string= key "isbn") (search-isbn value))
    (t (error "Not exist this key"))))
