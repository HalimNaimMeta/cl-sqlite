(defpackage :sqlite-tests
  (:use :cl :sqlite :5am :iter :bordeaux-threads)
  (:export :run-all-sqlite-tests))

(in-package :sqlite-tests)

(def-suite sqlite-suite)

(defun run-all-sqlite-tests ()
  (run! 'sqlite-suite))

(in-suite sqlite-suite)

(test test-connect
  (with-open-database (db ":memory:")))

(test test-disconnect-with-statements
  (finishes
    (with-open-database (db ":memory:")
      (prepare-statement db "create table users (id integer primary key, user_name text not null, age integer null)"))))

(defmacro with-inserted-data ((db) &body body)
  `(with-open-database (,db ":memory:")
     (execute-non-query ,db "create table users (id integer primary key, user_name text not null, age integer null)")
     (execute-non-query ,db "insert into users (user_name, age) values (?, ?)" "joe" 18)
     (execute-non-query ,db "insert into users (user_name, age) values (?, ?)" "dvk" 22)
     (execute-non-query ,db "insert into users (user_name, age) values (?, ?)" "qwe" 30)
     ,@body))

(test create-table-insert-and-error
  (with-inserted-data (db)
    (signals sqlite-constraint-error
      (execute-non-query db "insert into users (user_name, age) values (?, ?)" nil nil))))

(test create-table-insert-and-error/named
  (with-inserted-data (db)
    (signals sqlite-constraint-error
      (execute-non-query/named db "insert into users (user_name, age) values (:name, :age)" ":name" nil ":age" nil))))

(test test-select-single
  (with-inserted-data (db)
    (is (= (execute-single db "select id from users where user_name = ?" "dvk")
           2))))

(test test-select-single/named
  (with-inserted-data (db)
    (is (= (execute-single/named db "select id from users where user_name = :name" ":name" "dvk")
           2))))

(test test-select-m-v
  (with-inserted-data (db)
    (is (equalp (multiple-value-list (execute-one-row-m-v db "select id, user_name, age from users where user_name = ?" "joe"))
                (list 1 "joe" 18)))))

(test test-select-m-v/named
  (with-inserted-data (db)
    (is (equalp (multiple-value-list (execute-one-row-m-v/named db "select id, user_name, age from users where user_name = :name" ":name" "joe"))
                (list 1 "joe" 18)))))

(test test-select-list
  (with-inserted-data (db)
    (is (equalp (execute-to-list db "select id, user_name, age from users")
                '((1 "joe" 18) (2 "dvk" 22) (3 "qwe" 30))))))

(test test-iterate
  (with-inserted-data (db)
    (is (equalp (iter (for (id user-name age) in-sqlite-query "select id, user_name, age from users where age < ?" on-database db with-parameters (25))
                      (collect (list id user-name age)))
                '((1 "joe" 18) (2 "dvk" 22))))))

(test test-iterate/named
  (with-inserted-data (db)
    (is (equalp (iter (for (id user-name age) in-sqlite-query/named "select id, user_name, age from users where age < :age" on-database db with-parameters (":age" 25))
                      (collect (list id user-name age)))
                '((1 "joe" 18) (2 "dvk" 22))))))

(test test-loop-with-prepared-statement
  (with-inserted-data (db)
    (is (equalp (loop
                   with statement = (prepare-statement db "select id, user_name, age from users where age < ?")
                   initially (bind-parameter statement 1 25)
                   while (step-statement statement)
                   collect (list (statement-column-value statement 0) (statement-column-value statement 1) (statement-column-value statement 2))
                   finally (finalize-statement statement))
                '((1 "joe" 18) (2 "dvk" 22))))))

(test test-loop-with-prepared-statement/named
  (with-inserted-data (db)
    (let ((statement
           (prepare-statement db "select id, user_name, age from users where age < $x")))
      (unwind-protect
           (progn
             (is (equalp (statement-column-names statement)
                         '("id" "user_name" "age")))
             (is (equalp (statement-bind-parameter-names statement)
                         '("$x")))
             (bind-parameter statement "$x" 25)
             (flet ((fetch-all ()
                      (loop while (step-statement statement)
                         collect (list (statement-column-value statement 0)
                                       (statement-column-value statement 1)
                                       (statement-column-value statement 2))
                         finally (reset-statement statement))))
               (is (equalp (fetch-all) '((1 "joe" 18) (2 "dvk" 22))))
               (is (equalp (fetch-all) '((1 "joe" 18) (2 "dvk" 22))))
               (clear-statement-bindings statement)
               (is (equalp (fetch-all) '()))))
        (finalize-statement statement)))))
