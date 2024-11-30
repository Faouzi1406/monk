module List = struct
  let filter_first f l =
    try
      let first = List.hd @@ List.filter f l in
      Some first
    with
    | Failure _ -> None
  ;;

  let rec last = function
    | last :: [] -> Some last
    | [] -> None
    | _ :: rest -> last rest
  ;;
end
