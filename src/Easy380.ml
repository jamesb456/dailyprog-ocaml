(** Challenge 380 (easy): https://www.reddit.com/r/dailyprogrammer/comments/cmd1hb/20190805_challenge_380_easy_smooshed_morse_code_1/ *)

let morse_alphabet = ".- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.."
let morse_arr = String.split_on_char ' ' morse_alphabet |> Array.of_list
let get_morse chr : string = let char_code = Char.code(chr) in
    match char_code with
        | code when code >= Char.code('a') && code <= Char.code('z') -> morse_arr.(code - Char.code('a'))
        | _ -> ""
let smorse str = 
    let sq =  String.to_seq str |> Seq.map get_morse in
    let buffer = Buffer.create 16 in
    Seq.iter (fun x -> Buffer.add_string buffer x) sq;
    Buffer.contents buffer
let main () =
    print_endline (smorse "bits");
    print_endline (smorse "three")
    ;;
main ()