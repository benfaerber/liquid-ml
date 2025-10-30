let rec first_successful name =
  function
  | getter :: other_getters -> begin
    match getter name with
    | Some f -> Some f
    | _ -> first_successful name other_getters
  end
  | _ -> None


let function_from_id name =
  let sources =
    [ Liquid_number.function_from_id
    ; Liquid_string.function_from_id
    ; Liquid_list.function_from_id
    ; Liquid_helpers.function_from_id ]
  in

  first_successful name sources
