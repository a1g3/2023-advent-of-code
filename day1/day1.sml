load "Int";

fun day1 file =
  let
    fun firstnum [] = 999
      | firstnum (first :: tail) = if (ord first) > 47 andalso (ord first) < 58 then
                                    (ord first) - 48
                                  else
                                    (firstnum tail);
    fun	rev []   = []
      | rev(h::t) = (rev t) @ [h];

    fun getnum s = ((firstnum (explode s)) * 10) + (firstnum (rev (explode s)));
    val stream = TextIO.openIn file
    val done = ref false
    val total = ref 0
    
  in
    (while not (!done) do
      case TextIO.inputLine stream of
          SOME s => total := !total + (getnum s)
        | NONE => done := true; total)
  end;

(*

https://stackoverflow.com/questions/66257455/how-to-read-a-file-in-sml-line-by-line     
;https://www.cs.nmsu.edu/~rth/cs/cs471/sml.html
    ; https://homepages.inf.ed.ac.uk/stg/NOTES/node93.html *)