#light

module hw10

//
// pairwise L1 L2
//
// Given 2 lists L1 and L2, both the same length, merges the corresponding 
// elements into pairs, returning a list of pairs.
//
// Example:
//   hw10.pairwise [1;3;5;7] [10;20;30;40] ;;
//     => [(1,10); (3,20); (5,30); (7,40)] 
//
// You can solve using recursion, or higher-order, or both; tail recursion
// is not necessary.
//


let rec pairwise L1 L2 = 
    let rec _pairwise L1 L2 result =
        match L1, L2 with
        | [], [] -> List.rev result
        | hd1::tl1, hd2::tl2 -> _pairwise tl1 tl2 ((hd1, hd2) :: result)
    _pairwise L1 L2 []