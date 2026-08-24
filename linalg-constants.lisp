(in-package :quickdraw)

(defparameter +xy-plane-mat+ (on-plane-mat (vec-3 1 0 0)
                                           (vec-3 0 1 0)
                                           (vec-3 0 0 0)))

(defparameter +yz-plane-mat+ (on-plane-mat (vec-3 0 1 0)
                                           (vec-3 0 0 1)
                                           (vec-3 0 0 0)))

(defparameter +xz-plane-mat+ (on-plane-mat (vec-3 1 0 0)
                                           (vec-3 0 0 1)
                                           (vec-3 0 0 0)))

(defparameter +drop-z+ (mat-3-4 (list 1 0 0 0)
                                (list 0 1 0 0)
                                (list 0 0 0 1)))

(defparameter +placement-identity+ (mat-3-3 (list 1 0 0)
                                            (list 0 1 0)
                                            (list 0 0 1)))

(defparameter +ortho-z+ (mat-4-4 (list 1 0 0 0)
                                 (list 0 1 0 0)
                                 (list 0 0 1 0)
                                 (list 0 0 0 1)))

(defparameter +identity-4+ (mat-4-4 (list 1 0 0 0)
                                    (list 0 1 0 0)
                                    (list 0 0 1 0)
                                    (list 0 0 0 1)))
