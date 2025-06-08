(in-package #:rl)

(defconst +letters+
  (loop for c from (char-code #\a) to (char-code #\z)
        collecting (code-char c)))

(defmacro define-class (name direct-superclasses direct-slots &rest options)
  "used the same way as `defclass', but any classes that need to be
serialized for writing to disk should use this instead"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (macrolet ((defencoding (class-name)
                  `(defmethod ms:class-persistent-slots ((self ,class-name))
                     ',(mapcar #'c2mop:slot-definition-name (c2mop:class-slots (find-class class-name))))))
       (c2mop:ensure-finalized
        (defclass ,name ,direct-superclasses
          ,direct-slots
          ,@options))
       (defencoding ,name))))

(defmethod ms:class-persistent-slots ((self mixin-object))
  (mapcar #'c2mop:slot-definition-name (c2mop:class-slots (class-of self))))

(defmethod ms:marshal ((object mixin-object) &optional (circle-hash nil))
  (let* ((class   (class-of object))
         (pslots  (ms:class-persistent-slots object))
         (dummy   nil)
         (outlist nil))
    (setq dummy (ms::getvalue circle-hash object))
    (if dummy
        (setq outlist (list (ms::coding-idiom :reference) dummy))
        (progn
          (when pslots
            (setq dummy (ms::genkey circle-hash))
            (ms::setvalue circle-hash object dummy)
            (setf outlist (list (ms::coding-idiom :object)
				dummy
				(mapcar #'class-name (slot-value class 'dynamic-mixins::classes))
				(intern (package-name (symbol-package (class-name class)))
					:keyword)))
            (dolist (walker pslots)
              (setq outlist
		    (nconc outlist
			   (list (ms:marshal (slot-value object walker)
                                             circle-hash))))))))
    outlist))

(defmethod ms::unmarshal-fn ((version (eql (ms::coding-idiom :coding-release-no)))
                             (type (eql (ms::coding-idiom :object))) token &optional (circle-hash nil))
  ;; Token structure: (:object dummy-key class-names package-name . slot-values)
  (destructuring-bind (object-marker dummy-key class-names package-name &rest values) token
    (declare (ignore object-marker))
    (let* ((package (find-package package-name))
           (out (if (listp class-names)
                    ;; Mixin object
                    (make-instance (apply #'dynamic-mixins:mix class-names))
                    ;; Regular object
                    (allocate-instance (find-class (intern (symbol-name class-names)
                                                          package)))))
           (slots (ms:class-persistent-slots out)))

      (setf (gethash dummy-key circle-hash) out)

      (loop
        for slot in slots
        for value in values
        do (if (listp value)
               (setf (slot-value out slot)
                     (ms::unmarshal-fn version
                                       (first value)
                                       value
                                       circle-hash))
               (setf (slot-value out slot) (ms::unmarshal-fn version t value circle-hash))))
      (ms:initialize-unmarshalled-instance out))))
