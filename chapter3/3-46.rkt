#lang racket
;;test cell               ->true->set cell to false->process
;;         test cell->true->                                ->process
;;in this case, two process in the same serializer will be both processed.