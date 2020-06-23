(in-package #:rl)

(defconst +letters+
  (loop for c from (char-code #\a) to (char-code #\z)
        collecting (code-char c)))

(defmacro define-class (name direct-superclasses direct-slots &rest options)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (macrolet ((defencoding (class-name)
                  (c2mop:ensure-finalized
                   (defclass ,name ,direct-superclasses
                     ,direct-slots
                     ,@options))
                  `(defmethod ms:class-persistent-slots ((self ,class-name))
                     ',(mapcar #'c2mop:slot-definition-name (c2mop:class-slots (find-class class-name))))))
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
  (let* ((package    (find-package (fmt:object-package-name token)))
         (values     (fmt:class-slots-values  token))
         (class      (fmt:object-class-name token))
	 (out        (etypecase class
                       (cons (make-instance (apply #'dynamic-mixins:mix (fmt:object-class-name token))))
                       (t (allocate-instance (find-class (intern (symbol-name class)
                                                                 package))))))
	 (slots      (ms:class-persistent-slots  out)))

    (setf (gethash (fmt:id token) circle-hash) out)

    (loop
      for slot in slots
      for value in values
      do (if (listp value)
             (setf (slot-value out slot)
                   (ms::unmarshal-fn version
                                     (fmt:data-type value)
                                     value
                                     circle-hash))
             (setf (slot-value out slot) (ms::unmarshal-fn version t value circle-hash))))
    (ms:initialize-unmarshalled-instance out)))
