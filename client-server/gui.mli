(**The module that holds string functions for pretty printing to the terminal*)

(**string for the colored logo ELOCAML*)
val centered: string

(**[set_cursor x y] returns the ANSI escape sequence as a string which
   when printed, sets the terminals cursor to [x] [y]*)
val set_cursor: int -> int -> string

(**[set_cursor x1 y1 x2 y2] returns a string which when printed will
   erase the rectangle with top left [(x1,y1)], and bottom left [(x2, y2)].
   The string sets the cursor to do this, so if placed after another string
   it will overlay the rectangle.*)
val erase_square: int -> int -> int -> int -> string

(**the same as [erase_square], except the borders of the rectangle are
   replaced with lines to form a box*)
val draw_rect: int -> int -> int -> int -> string

(**[print_object_tl x y s] returns a string which will move
   a users cursor to print [s] at [(x,y)] in the terminal*)
val print_object_tl: int -> int -> string -> string
