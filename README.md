<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/> 
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right">
    <strong>Студентка</strong>: <em><strong>Саюн Дарина Миколаївна</strong></em>
</p>
<p align="right">
    <strong>Група</strong>: <em><strong>КВ-13</strong></em>
</p>
<p align="right">
    <strong>Рік</strong>: <em><strong>2024</strong></em>
</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної роботи 3 з такими змінами:
  - використати функції вищого порядку для роботи з послідовностями (де це доречно);
  - додати до інтерфейсу функції (та використання в реалізації) два ключових параметра: `key` та `test` , що працюють аналогічно до того, як працюють параметри з такими назвами в функціях, що працюють з послідовностями. При цьому `key` має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за можливості, має бути мінімізоване.

## Перша частина завдання
```lisp
(defun shake-it (lst R i k &key (key #'identity) (test #'<))
  (if (and lst (cdr lst))
      (if (< i R)
          (let* ((z1 (car lst))
                 (z2 (cadr lst))
                 (key-z1 (funcall key z1))
                 (key-z2 (funcall key z2))
                 (cond1 (funcall test key-z2 key-z1))
                 (z (if cond1 z2 z1))
                 (key-z (if cond1 key-z2 key-z1))
                 (new-lst (if cond1 (cons z1 (cddr lst)) (cdr lst)))
                 (new-k (if cond1 i k)))
            (multiple-value-bind (temp-lst bind-k bind-R)
                (shake-it new-lst R (1+ i) new-k :key key :test test)
              (if (> i bind-R)
                  (values (cons z temp-lst) bind-k bind-R)
                  (let* ((key-temp (if temp-lst (funcall key (car temp-lst))))
                         (cond2 (and temp-lst (funcall test key-z key-temp)))
                         (back-k (if cond2 bind-k i)))
                    (if cond2
                        (values (cons z temp-lst) back-k bind-R)
                        (values (cons (car temp-lst) (cons z (cdr temp-lst))) back-k bind-R))))))
          (values lst k k))
      (values lst k k)))

(defun left-side-apart (lst L)
  (values (subseq lst 0 L) (nthcdr L lst)))

(defun shaker-sort-inner (lst L R k &key (key #'identity) (test #'<))
  (if (< L R)
      (multiple-value-bind (left-side right-side) (left-side-apart lst L)
        (multiple-value-bind (res res-k res-R) (shake-it right-side R L k :key key :test test)
          (shaker-sort-inner (append left-side res) (1+ res-k) res-R res-k :key key :test test)))
      lst))

(defun shaker-sort (lst &key (key #'identity) (test #'<))
  (shaker-sort-inner lst 0 (1- (length lst)) 0 :key key :test test))
```

### Тестові набори та утиліти
```lisp
(defun check-shaker-sort (name input-lst expected &key (key #'identity) (test #'<)) 
  "Execute `shaker-sort' on `input', compare result with `expected' and print comparison status" 
  (format t "~:[FAILED~;passed~]... ~a~%" 
          (equal (shaker-sort input-lst :key key :test test) expected) 
          name))

(defun test-shaker-sort ()
  (check-shaker-sort "test-1"
                     '(2 5 3 1 4 7 3 5 6 2)
                     '(1 2 2 3 3 4 5 5 6 7))
  (check-shaker-sort "test-2"
                     '(3 -5 6 -7 1 -2 2 4 8)
                     '(1 -2 2 3 4 -5 6 -7 8)
                     :key #'abs)
  (check-shaker-sort "test-3"
                     '(3 -5 6 -7 1 -2 2 4 8)
                     '(8 -7 6 -5 4 3 -2 2 1)
                     :key #'abs
                     :test #'>)
  (check-shaker-sort "test-4"
                     '((2 . 1) (5 . 2) (1 . 3) (6 . 4) (4 . 5) (3 . 6))
                     '((1 . 3) (2 . 1) (3 . 6) (4 . 5) (5 . 2) (6 . 4))
                     :key #'car)
  (check-shaker-sort "test-4"
                     '((2 . 1) (5 . 2) (1 . 3) (6 . 4) (4 . 5) (3 . 6))
                     '((3 . 6) (4 . 5) (6 . 4) (1 . 3) (5 . 2) (2 . 1))
                     :key #'cdr
                     :test #'>)
  (check-shaker-sort "test-5"
                     '()
                     '()))
```

### Тестування
```lisp
CL-USER> (test-shaker-sort)
passed... test-1
passed... test-2
passed... test-3
passed... test-4
passed... test-4
passed... test-5
NIL
```

## Друга частина завдання. Варіант 4 (16)
Написати функцію `add-next-reducer` , яка має один ключовий параметр — функцію
`transform` . `add-next-reducer` має повернути функцію, яка при застосуванні в якості
першого аргументу `reduce` робить наступне: кожен елемент списку-аргументу `reduce`
перетворюється на точкову пару, де в комірці CAR знаходиться значення поточного 
елемента, а в комірці CDR знаходиться значення наступного елемента списку (тобто
того, що знаходиться "справа"). Якщо функція `transform` передана, тоді значення
поточного і наступного елементів, що потраплять у результат, мають бути змінені згідно
`transform` . Обмеження, які накладаються на використання функції-результату
`add-next-reduce` при передачі у `reduce` визначаються розробником (тобто, наприклад,
необхідно чітко визначити, якими мають бути значення ключових параметрів функції
`reduce` `from-end` та `initial-value` ). `transform` має виконатись мінімальну кількість разів.
```lisp
(defun add-next-reducer (&key (transform #'identity))
  "For correct work you must use `reducer` with parameters:
    -:from-end t
    -:initial-value nil"
  (lambda (elem acc)
    (let ((next (if acc (car acc) '(nil))))
      (cons (cons (funcall transform elem) (car next)) acc))))
```

### Тестові набори та утиліти
```lisp
(defun check-reducer (name input-lst expected &key (transform #'identity)) 
  "Execute `reducer' on `input', compare result with `expected' and print comparison status" 
  (let ((result (reduce (add-next-reducer :transform transform)
                        input-lst
                        :initial-value nil
                        :from-end t)))
    (format t "~:[FAILED~;passed~]... ~a~%" 
                  (equal result expected) 
                  name)))

(defun test-reducer ()
  (check-reducer "test-1"
                 '(1 2 3)
                 '((1 . 2) (2 . 3) (3 . nil)))
  (check-reducer "test-2" 
                 '(1 2 3) 
                 '((10 . 20) (20 . 30) (30 . nil))
                 :transform (lambda (x) (* x 10)))
  (check-reducer "test-3"
                 '(42)
                 '((42 . nil)))
  (check-reducer "test-4"
                 '(-1 -2 -3)
                 '((1 . 2) (2 . 3) (3 . nil))
                 :transform #'abs)
  (check-reducer "test-5"
                 '()
                 '()))
```

### Тестування
```lisp
CL-USER> (test-reducer)
passed... test-1
passed... test-2
passed... test-3
passed... test-4
passed... test-5
NIL
```
