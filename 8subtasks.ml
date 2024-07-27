let member compare t l =
  List.exists (fun e -> compare t e = 0) l


let equal_second_components (_, x) (_, y) = compare x y;;

let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2);;




let count_occurrences lst =
  let rec count_helper element lst count =
    match lst with
    | [] -> count
    | x :: xs when x = element -> count_helper element xs (count + 1)
    | _ :: xs -> count_helper element xs count
  in
  let rec count_occurrences_helper lst acc =
    match lst with
    | [] -> acc
    | x :: xs ->
      if List.mem_assoc x acc then
        count_occurrences_helper xs acc
      else
        let count = count_helper x lst 0 in
        count_occurrences_helper xs ((x, count) :: acc)
  in
  let occurrences = count_occurrences_helper lst [] in
  List.sort (fun (_, count1) (_, count2) -> compare count2 count1) occurrences





let drop_last l =
  let rec drop_last_helper acc l =
    match l with
    | [] -> failwith "Empty list has no last element"
    | [_] -> List.rev acc
    | x :: xs -> drop_last_helper (x :: acc) xs
  in
  drop_last_helper [] l




let drop_last_opt l =
  let rec drop_last_helper acc l =
    match l with
    | [] -> None
    | [_] -> Some (List.rev acc)
    | x :: xs -> drop_last_helper (x :: acc) xs
  in
  drop_last_helper [] l





let zip_with f l1 l2 =
  let rec aux_zip_with f l1 l2 acc =
    match l1, l2 with
    | [], _ | _, [] -> List.rev acc
    | x :: xs, y :: ys -> aux_zip_with f xs ys ((f x y) :: acc)
  in
  aux_zip_with f l1 l2 []
  
  


let unzip lst =
  let rec aux_unzip lst acc_x acc_y =
    match lst with
    | [] -> (List.rev acc_x, List.rev acc_y)
    | (x, y) :: xs -> aux_unzip xs (x :: acc_x) (y :: acc_y)
  in
  aux_unzip lst [] []





first we Invoke the unzip function  ```unzip [('a',1);('b',2)]```, then we Invoke the aux_unzip 
function with initial accumulators acc_x and acc_y as empty lists ```aux_unzip [('a',1);('b',2)] [] []```,
we match the first element of the list (head) with the pattern '''(x, y) :: xs''', where 
'''x = 'a', y = 1, and xs = [('b',2)]''', we then match the first element of the list (head) with the pattern
```(x, y) :: xs, where x = 'b', y = 2, and xs = []```, we Match the empty list with the pattern [] and apply
 the List.rev function to both acc_x and acc_y, finally reverse both lists and return the result:
  




   
type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha

let games x list =
  let count_games count match_data =
    let (team1, _, team2, _) = match_data in
    if team1 = x || team2 = x then count + 1 else count
  in
  List.fold_left count_games 0 list;;


let wins x list =
  let win (team1, scorers1, team2, scorers2) =
    (x = team1 && List.length scorers1 > List.length scorers2) ||
    (x = team2 && List.length scorers2 > List.length scorers1)
  in
  let wcount count match_data =
    if win match_data then count + 1 else count
  in
  List.fold_left wcount 0 list;;

    
let draws x list =
  let draw (team1, scorers1, team2, scorers2) =
    (x = team1 || x = team2) && List.length scorers1 = List.length scorers2
  in
  let cdraw count match_data =
    if draw match_data then count + 1 else count
  in
  List.fold_left cdraw 0 list;;

      
let loses x list =
  let loss (team1, scorers1, team2, scorers2) =
    (x = team1 && List.length scorers1 < List.length scorers2) ||
    (x = team2 && List.length scorers2 < List.length scorers1)
  in
  let closses count match_data =
    if loss match_data then count + 1 else count
  in
  List.fold_left closses 0 list;;

        
let goalsFor x list =
  let for match_data =
    let (team1, scorers1, team2, scorers2) = match_data in
    if x = team1 then List.length scorers1
    else if x = team2 then List.length scorers2
    else 0
  in
  let cfor count match_data =
    count + for match_data
  in
  List.fold_left cfor 0 list;;

          
let goalsAgainst x list =
  let against match_data =
    let (team1, scorers1, team2, scorers2) = match_data in
    if x = team1 then List.length scorers2
    else if x = team2 then List.length scorers1
    else 0
  in
  let cagainst count match_data =
    count + against match_data
  in
  List.fold_left cagainst 0 list;;

            
let points x list =
  let points_for match_data =
    let (team1, scorers1, team2, scorers2) = match_data in
    let goals1 = List.length scorers1 in
    let goals2 = List.length scorers2 in
    if x = team1 then
      if goals1 > goals2 then 3 else if goals1 = goals2 then 1 else 0
    else if x = team2 then
      if goals2 > goals1 then 3 else if goals2 = goals1 then 1 else 0
    else 0
  in
  let cpoints count match_data =
    count + points_for match_data
  in
  List.fold_left cpoints 0 list;;

              
let unique_teams list =
  let add_unique acc team =
    if List.mem team acc then acc else team :: acc
  in
  let fold_fn acc (team1, _, team2, _) =
    let new_acc = add_unique acc team1 in
    let new_acc = add_unique new_acc team2 in
    new_acc
  in
  List.fold_left fold_fn [] list;;

                
let compare_parameters (t, g, w, d, l, gf, ga, p) (t1, g1, w1, d1, l1, gf1, ga1, p1) =
  match (p - p1, (gf - ga) - (gf1 - ga1), gf - gf1) with
  | (0, 0, 0) -> 0
  | (0, 0, diff) -> if diff < 0 then 1 else -1
  | (0, diff, _) -> if diff < 0 then 1 else -1
  | (diff, _, _) -> if diff < 0 then 1 else -1;;

                
let table list =
  let teams = unique_teams list in
  let stats team =
    (team, games team list, wins team list, draws team list, loses team list,
     goalsFor team list, goalsAgainst team list, points team list)
  in
  let table = List.map stats teams in
  List.sort compare_parameters table;;

let sorted_table list = 
  List.sort compare_parameters (table list);;   


let footballers list =
  let all (_, scorers1, _, scorers2) = scorers1 @ scorers2 in
  List.concat (List.map all list);;

                    
let goals goal list =
  let cgoal h = if goal = h then 1 else 0 in
  List.fold_left (fun count h -> count + cgoal h) 0 list;;

                      
let filteredFootballers list =
  let filtered h = goals h list > 0 in
  List.filter filtered list;;

                        
let findPlayers player scorers =
  List.exists (fun p -> p = player) scorers;;

                          
let teams goal list =
  let team_in_match (team1, scorers1, team2, scorers2) =
    if List.exists (fun p -> p = goal) scorers1 then Some team1
    else if List.exists (fun p -> p = goal) scorers2 then Some team2
    else None
  in
  let rec team_in_lst = function
    | [] -> failwith "No team found"
    | h :: t -> (
        match team_in_match h with
        | Some team -> team
        | None -> team_in_lst t)
  in
  team_in_lst list;;

                            
let unsorted list =
  let footballer_list = footballers list in
  let unique_footballers = filteredFootballers footballer_list in
  let scorer_data h = (h, teams h list, goals h footballer_list) in
  unique_footballers |> List.map scorer_data;;

                              
let scorers_comparison (goal, _, g) (goal1, _, g1) =
  let goal_diff = g - g1 in
  if goal_diff <> 0 then -goal_diff
  else String.compare goal goal1;;

                                
let sorted list =
  let unsorted_scorers_list = unsorted list in
  List.sort scorers_comparison unsorted_scorers_list;;

                                  
  let table_and_scorers list =
  let sorted_table_list = sorted_table list in
  let sorted_scorers_list = sorted list in
  (sorted_table_list, sorted_scorers_list);;
  