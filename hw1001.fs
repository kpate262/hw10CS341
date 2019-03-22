#light
module hw10

//
// examAverages LT
//
// Given a list of tuples of the form ("netid", [score;score;score;...]),
// computes each netid's average score as a real number and returns a 
// list of tuples of the form ("netid", average).
//
// Example:
//   hw10.examAverages [("sdeitz2",[100;90;91]); ("psankar",[100;100;100;100;98])] ;;
//     => [("sdeitz2",93.66666667); ("psankar",99.6)]
//
// NOTE: F# offers a function List.average L that computes the average of
// a list of real numbers.  However, the list of scores in the tuple are
// integers, so in order to use List.average, you would first need to convert
// the integers to reals --- List.map float L would work nicely here.
//
// You can solve using recursion, or higher-order, or both; tail recursion
// is not necessary.
//


let floatScoreList L =
    List.map float L


let rec examAverages LT = 
    let rec _examAverages LT result =
        match LT with
        | [] -> List.rev result
        | e::tail -> _examAverages tail ((fst e, (List.average (floatScoreList (snd e)) )) :: result )
    _examAverages LT []