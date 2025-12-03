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
   Matched : Match_Array (0 .. 10);

   procedure Clear_Buffer is
   begin
      Expect (Pd, Result, ".*", Timeout => 0);
   end Clear_Buffer;

   function Capture_Match (regex : String) return String is
   begin
      Expect (Pd, Result,
              Regexp => regex,
              Matched => Matched,
              Timeout => 1_000);
      case Result is
         when Expect_Timeout =>
            Put_Line (Standard_Error, "-- Timeout --");
            return "";
         when 1 .. 100  => --  Regex match
            --  Put_Line (Standard_Error, Expect_Out(Pd));
            --  Put_Line (Standard_Error, Expect_Out_Match(Pd));
            return (Expect_Out (Pd) (Matched (1).First .. Matched (1).Last));
         when others =>
            Put_Line (Standard_Error, "-- Unknown, no match --");
            return "";
      end case;
   end Capture_Match;

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

   Put_Line (Capture_Match("== (.*) =="));
   -- Put_Line (Capture_Match("Things of interest here:\n(?:- ([a-z]*)\n)"));
   Put_Line (Capture_Match("Things of interest here:\n(?:- ([a-z]*)\n)+"));

   -- Get all list items BEFORE the exit list items
   Put_Line (Capture_Match("- ([a-z]*).*There .* \d exit.*\n"));

   -- Get amount of exits

   Put_Line (Capture_Match("There .* (\d) exit.*\n"));

   -- Loop for the exits

   Put_Line (Capture_Match("- ([a-z]*)\n"));
   Put_Line (Capture_Match("- ([a-z]*)\n"));

   --  Ensure that the output has been fully realised.
   Expect (Pd, Result, Regexp => "What do you do\?", Timeout => 100);
   --  Clearing buffer just in case. The previous line should've done this already.
   Clear_Buffer;

   Send (Pd, "echo 'hi'");

   --  When we are done, terminate the called program.
   Close (Pd);
end Explore;
