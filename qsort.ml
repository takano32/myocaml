let rec quicksort = function
        [] -> []
        | hd::tl ->
                        let (lt, gt) = List.partition (fun i -> i < hd) tl in
                        (quicksort lt)@[hd]@(quicksort gt) ;;


