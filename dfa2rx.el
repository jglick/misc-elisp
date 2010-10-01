;;; Was originally being used in Column Shape Mode, but was ripped
;;; out. Anyway, I put a lot of effort into it, so if you ever need to
;;; convert a DFA to a regexp in Elisp:

;;; Node format is a list whose car is some arbitrary identifier
;;; (preserved but ignored by this code), and whose cdr is a list of
;;; edges, each of which being a cons from a node to a regexp. Regexp
;;; structure is list-oriented internally, with converters.

(require 'cl)

;; These three do very basic optimizations; more are certainly
;; possible. E.g. distributive law [(alt (seq x . ys) (seq x . zs)) =>
;; (seq x (alt (seq . ys) (seq . zs)))].

(defun dfa2rx-rx-collapse (rxs key)
  (mapcan (lambda (portion)
	    (if (and (listp portion) (eq (car portion) key))
		(cdr portion)
	      (list portion)))
	  rxs))

;; XXX these two still have Column-Shade-Mode-specific stuff in them,
;; for the spaces & tabs. Ideally there would be some general way to
;; collapse character ranges instead of this.

(defun dfa2rx-rx-alternate (&rest rxs)
  (let ((real-rxs (remove-duplicates
		   (dfa2rx-rx-collapse rxs 'alt)
		   :test 'equal)))
    (case (length real-rxs)
      (0 'empty)
      (1 (car real-rxs))
      (t (if (> (length (remove-if-not (lambda (x) (memq x '(spc tab spc-or-tab)))
				       real-rxs))
		1)
	   (apply 'dfa2rx-rx-alternate 'spc-or-tab
		  (remove-if (lambda (x) (memq x '(spc tab spc-or-tab))) real-rxs))
	   (cons 'alt real-rxs))))))

(defun dfa2rx-rx-sequential (&rest rxs)
  (let ((real-rxs (remove* 'empty
			   (dfa2rx-rx-collapse rxs 'seq)
			   :test 'eq)))
    (case (length real-rxs)
      (0 'empty)
      (1 (car real-rxs))
      (t (cons 'seq real-rxs)))))

(defun dfa2rx-rx-kleene (rx)
  (if (eq rx 'empty) 'empty (list 'kle rx)))

(defun dfa2rx-consolidate-node (node)
  "Consolidate all edges pointing to the same destination."
  (let ((hash nil))			; alist from dests to regex
    (dolist (edge (cdr node))
      (let ((dest (car edge))
	    (rx (cdr edge)))
	(let ((existing (assq dest hash)))
	  (if existing
	      (setf (cdr existing) (dfa2rx-rx-alternate (cdr existing) rx))
	    (push (cons dest rx) hash)))))
    (setf (cdr node) hash)))

(defun dfa2rx-node-dfs (start)
  "Return a uniquified list of nodes reachable from this one, incl. itself."
  (let ((done nil)
	(todo (list start)))
    (while todo
      (let ((search (pop todo)))
	(when (not (memq search done))
	  (push search done)
	  (dolist (child (mapcar 'car (cdr search)))
	    (pushnew child todo)))))
    done))

(defun dfa2rx-flatten-dfa (start end)
  "Do the dirty work of collapsing a DFA until no interior nodes
remain. All nodes should already be consolidated. You should pass in
the start & (unique) end state. Do the usual trick to get only one end
state: make all putative final states link to it w/ rx 'empty."
  ;; First, find the remaining interior nodes. We will remove them as
  ;; we go.
  (let ((interior (remq start (remq end (dfa2rx-node-dfs start)))))
    (while interior
      ;; The next interior node to remove.
      (let ((dead (pop interior)))
	;; Tos is a list of edges; froms is reverse edges (i.e. conses
	;; of origin + regexp of real edge). Self is the self-loop rx,
	;; if any.
	(let ((tos (cdr dead))
	      (froms (mapcan
		      (lambda (node)
			(mapcan
			 (lambda (edge)
			   (if (eq (car edge) dead)
			       (list (cons node (cdr edge)))
			     nil))
			 (cdr node)))
		      (cons start interior)))
	      self)
	  ;; Find the self-edge, if any, & treat specially.
	  (let ((self-edge (assq dead tos)))
	    (when self-edge
	      (setq tos (remq self-edge tos))
	      (setq froms (remove* dead froms :key 'car :test 'eq))
	      (setq self (cdr self-edge))))
	  ;; For each node with an edge into dead node...
	  (dolist (from froms)
	    (let ((from-node (car from))
		  (from-rx (cdr from)))
	      ;; First, no longer point to the dead node.
	      (let ((check (length from-node)))
		(setf (cdr from-node) (remove* dead (cdr from-node) :key 'car :test 'eq))
		(when (not (= check (+ 1 (length from-node))))
		  (error "Did not remove dead node!")))
	      ;; Now, for each node the dead node had an edge to...
	      (dolist (to tos)
		(let ((to-node (car to))
		      (to-rx (cdr to)))
		  ;; Add an edge from that from-node to that to-node,
		  ;; whose regexp represents following both edges in
		  ;; sequence, possibly with the Kleene of the
		  ;; self-loop on the dead node in the middle.
		  (setf (cdr from-node)
			(cons
			 (cons
			  to-node
			  (dfa2rx-rx-sequential
			   from-rx
			   (if self (dfa2rx-rx-kleene self) 'empty)
			   to-rx))
			 (cdr from-node)))))
	      (dfa2rx-consolidate-node from-node))))))))

(defun dfa2rx-rx-is-term (rx)
  (or (atom rx) (eq (car rx) 'kle)))

(defun dfa2rx-stringify-rx (rx)
  (cond
   ((eq rx 'empty) "")
   ;;; XXX again, col-shd specifics here
   ((eq rx 'spc) " ")
   ((eq rx 'tab) "\t")
   ((eq rx 'spc-or-tab) "[ \t]")
   ((eq (car rx) 'kle) (concat "\\(" (dfa2rx-stringify-rx (cadr rx)) "\\)*"))
   ((eq (car rx) 'alt) (mapconcat (lambda (rx2)
				    (let ((s (dfa2rx-stringify-rx rx2)))
				      (if (dfa2rx-rx-is-term rx2)
					  s
					(concat "\\(" s "\\)"))))
				  (cdr rx)
				  "\\|"))
   ((eq (car rx) 'seq) (mapconcat (lambda (rx2)
				    (let ((s (dfa2rx-stringify-rx rx2)))
				      (if (dfa2rx-rx-is-term rx2)
					  s
					(concat "\\(" s "\\)"))))
				  (cdr rx)
				  ""))
   (t (error "What is %S?" rx))))
