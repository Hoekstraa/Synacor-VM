with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Expect; use GNAT.Expect;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with GNAT.Regpat; use GNAT.Regpat;

procedure Explore is
   --  Command : constant String := "/bin/sh"; --  Let's spawn a program.
   Command : constant String := "/Users/zonoia/Workspace/synacorvm/bin/synacorvm"; --  Let's spawn a program.
   Pd      : Process_Descriptor;
   Args    : Argument_List_Access;
   Result  : Expect_Match;
   Matched : Match_Array (0 .. 1);

begin
   --  First we spawn the process. Splitting the program name and the
   --  arguments is done as in the previous example, using
   --  Argument_String_To_List. Remember to free the memory at the end.

   Args := Argument_String_To_List (Command);

   Non_Blocking_Spawn
     (Pd,
      Command     => Args (Args'First).all,
      Args        => Args (Args'First + 1 .. Args'Last),
      Buffer_Size => 0);

   --  The program is now running. Let's read it's output.

   Expect (Pd, Result, Regexp => "Things of interest here:\n- ([a-z]*)\n$", Matched => Matched, Timeout => 1_000);
   declare
      Output : String := Expect_Out(Pd);
      Match : String := Expect_Out_Match(Pd);
   begin
      --  Output := Expect_Out(Pd);
      Put_Line ("---");
      Put_Line (Match);
      Put_Line ("---");
      Put_Line (Expect_Out (Pd) (Matched (1).First .. Matched (1).Last));
   end;

   Expect (Pd, Result, Regexp => "What do you do\?", Timeout => 1_000);

   case Result is
      when Expect_Timeout =>
         null;  --  never replied
      when 1  =>
         null;              --  the regexp matched
         --  We then have access to all the output of gdb since the
         --  last call to Expect, through the Expect_Out subprogram.
         declare
            Output : String := Expect_Out(Pd);
         begin
            --  Output := Expect_Out(Pd);
            Put_Line (Output);
         end;
      when others =>
         null;

   end case;


   Send (Pd, "echo 'hi'");


   --  When we are done, terminate the called program.
   Close (Pd);
end Explore;
