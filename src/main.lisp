(defpackage my-book-shelf
  (:use :cl
        :cl-dbi
        :ltk)
  (:export :init-db
           :drop-tb
           :add-book
           :show-all
           :search-title
           :search-author
           :search-publisher
           :search-isbn
           :book
           :gui-main))
(in-package :my-book-shelf)


;;; Create books.db.
(defun init-db ()
  (dbi:with-connection 
    (conn :sqlite3
          :database-name #p"books.db")
    (dbi:do-sql conn "CREATE TABLE books (\
    title VARCHAR(80) PRIMARY KEY NOT NULL, \
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
         :accessor isbn
         :documentation "ISBN is 13 digits. 9784xxxxxxxxx.")))


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
            do (format t "~{~A: ~A~%~}~%" row)))))


;;; Search title
(defun search-title (title-name)
  (dbi:with-connection
    (conn :sqlite3
          :database-name #p"books.db")
    (let* ((query (dbi:prepare conn "SELECT * FROM books WHERE title = ?"))
           (result (dbi:execute query title-name))
           (row (dbi:fetch result)))
      (format nil "~{~A: ~A~%~}~%" row))))


;;; Search author
(defun search-author (author-name)
  (dbi:with-connection
    (conn :sqlite3
          :database-name #p"books.db")
    (let* ((query (dbi:prepare conn "SELECT * FROM books WHERE author = ?"))
           (result (dbi:execute query author-name)))
      (loop for row = (dbi:fetch result)
            while row
            do (format t "~{~A: ~A~%~}~%" row)))))


;;; Search publisher
(defun search-publisher (publisher-name)
  (dbi:with-connection
    (conn :sqlite3
          :database-name #p"books.db")
    (let* ((query (dbi:prepare conn "SELECT * FROM books WHERE publisher = ?"))
           (result (dbi:execute query publisher-name)))
      (loop for row = (dbi:fetch result)
            while row
            do (format t "~{~A: ~A~%~}~%" row)))))


;;; Search ISBN
(defun search-isbn (isbn-num)
  (if (not (typep isbn-num 'integer))
      (error "Not number")
      (dbi:with-connection
          (conn :sqlite3
                :database-name #p"books.db")
        (let* ((query (dbi:prepare conn "SELECT * FROM books WHERE isbn = ?"))
               (result (dbi:execute query isbn-num))
               (row (dbi:fetch result)))
          (format nil "~{~A: ~A~%~}~%" row)))))


;;; Validate whether input-text is able to parse-integer.
(defun check-parse-integer (text)
  (let ((result nil))
    (ignore-errors
      (parse-integer text)
      (setf result t))
    (unless result)
    result))


;;; Helper function for GUI
(defun search-title-or-isbn (book)
  (if (check-parse-integer book)
    (search-isbn (parse-integer book))
    (search-title book)))


;;; GUI mainloop
(defun gui-main ()
  (with-ltk ()
    (wm-title *tk* "My Book Shelf")
    (let* ((ent-f (make-instance 'frame))
           (ent-label (make-instance 'label
                                     :master ent-f
                                     :text "Title|ISBN"))
           (ent (make-instance 'entry
                               :master ent-f))
           (res-label (make-instance 'label
                                     :master nil))
           (b1 (make-instance 'button
                              :master ent-f
                              :text "search"
                              :command (lambda ()
                                         (setf (text res-label)
                                               (search-title-or-isbn (text ent))))))
           (f (make-instance 'frame))
           (exit-btn (make-instance 'button
                                  :master f
                                  :text "Exit"
                                  :command (lambda ()
                                             (setf *exit-mainloop* t)))))
      (pack ent-f :side :top)
      (pack ent-label :side :left)
      (pack ent :side :left)
      (pack b1 :side :left)
      (pack res-label)
      (pack f :side :bottom)
      (pack exit-btn :side :left))))
