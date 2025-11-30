package Operations is
   --  Defining Operations / Instructions

   type OpSize is range 0 .. 4;
   --  subtype OpName is String (1 .. 6);

   type Op is record
      Name : String (1 .. 4);
      Length : OpSize;
   end record;

   Ops : constant array (Natural range <>) of Op :=
     (0 => (Name => "exit", Length => 0),
      1 => (Name => " set", Length => 3),
      2 => (Name => "push", Length => 2),
      3 => (Name => " pop", Length => 2),
      4 => (Name => "  eq", Length => 4),
      5 => (Name => "  gt", Length => 4),
      6 => (Name => " jmp", Length => 0),
      7 => (Name => "  jt", Length => 0),
      8 => (Name => "  jf", Length => 0),
      9 => (Name => " add", Length => 4),
      10 => (Name => "mult", Length => 4),
      11 => (Name => " mod", Length => 4),
      12 => (Name => " and", Length => 4),
      13 => (Name => "  or", Length => 4),
      14 => (Name => " not", Length => 3),
      15 => (Name => "rmem", Length => 3),
      16 => (Name => "wmem", Length => 3),
      17 => (Name => "call", Length => 0),
      18 => (Name => " ret", Length => 0),
      19 => (Name => " out", Length => 2),
      20 => (Name => "  in", Length => 2),
      21 => (Name => "noop", Length => 1)
     );

   type Ops_Reverse is (Op_Exit,
                        Op_Set,
                        Op_Push,
                        Op_Pop,
                        Op_Eq,
                        Op_Gt,
                        Op_Jmp,
                        Op_Jt,
                        Op_Jf,
                        Op_Add,
                        Op_Mult,
                        Op_Mod,
                        Op_And,
                        Op_Or,
                        Op_Not,
                        Op_Rmem,
                        Op_Wmem,
                        Op_Call,
                        Op_Ret,
                        Op_Out,
                        Op_In,
                        Op_Noop);

end Operations;
