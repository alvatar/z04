;;
;; SceneGraph
;;
;; A Scene graph is a list with internal references made of simple definitions of geometrical
;; objects. It is transformed into a tree, which contains the information for rendering and transformations.
;; This tree is made of nodes, starting from a root node (root:)
;;

;; Get fake graph for testing
(define (get-test-data width height)
  (let [(obj-uuid (uuid-v4))]
    `(root:
      (objects:
       ((id: ,obj-uuid) (polyline: (points: (0.0 0.0) (,width ,height))))
       ((id: ,(uuid-v4)) (polyline: (points: (9 1) (0 0) (2 2) (3 3)))))
      (group:
       (id: ,(uuid-v4))
       (prop: (name: "my group")
              (tags: "main"))
       (elements:
        (polyline: (id: ,(uuid-v4)) (prop: (tags: "helpers"))
                   (points: (0.0 0.0) (,width ,height)))
        ;; (polyline: (id: ,(uuid-v4))
        ;;            (points: (3 200) (100 400) (500 20) (20 3000)))
        ;; (polyline: (id: ,(uuid-v4))
        ;;            (points: (-200 100) (400 400) (20 400) (0 30)))
        ;; (ref: (id: ,(uuid-v4)) (object: ,obj-uuid))
        ))
      (text:
       (id: ,(uuid-v4))
       (content: "Hello CAD!")
       (font: ("assailand" 34))
       (color: (255 255 255 255))
       (box2d: (100.0 100.0) (40.0 34.0)))
      (text:
       (id: ,(uuid-v4))
       (content: "Step by Step")
       (font: ("assailand" 34))
       (color: (255 255 255 255))
       (box2d: (10.0 10.0) (40.0 34.0)))
      (text:
       (id: ,(uuid-v4))
       (content: "Looking good...")
       (font: ("assailand" 34))
       (color: (255 255 255 255))
       (box2d: (500.0 10.0) (40.0 34.0))))))
