--  with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams.Stream_IO;
with Hardware; use Hardware;
with Ada.Containers;

procedure Synacorvm is
   --  For temporary storage of operands
   A : Hardware.UInt16;
   B : Hardware.UInt16;
   C : Hardware.UInt16;

   --  Create a stream from the Standard Input.
   Input_File : constant Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Input;
   Input_Stream : constant access Ada.Streams.Root_Stream_Type'Class :=
     Ada.Text_IO.Text_Streams.Stream ( File => Input_File);

   Debug : constant Boolean := False;
   procedure Put_Dbg (S : String) is
   begin
      if Debug then
         Put_Line (Standard_Error, S);
      end if;
   end Put_Dbg;

begin
   Put_Line ("Loading disk image into memory.");
   Hardware.Load_Binary;
   Put_Line ("Done, starting machine.");
   Put_Line ("");

   loop
      Put_Dbg ("----------");
      Put_Dbg ("PC value: " & Integer'Image (Integer (PC)));
      Put_Dbg ("PC point: " & Integer'Image (Integer (Memory (PC))));
      Put_Dbg ("Next instruction:"
               & Memory (PC)'Image
               & Memory (PC + 1)'Image
               & Memory (PC + 2)'Image
               & Memory (PC + 3)'Image);
      Put_Dbg ("Stack has "
               & Ada.Containers.Count_Type'Image (Stack.Length)
               & " elements");
      Put_Dbg ("----------");

      case Memory (PC) is
         when 0 =>
            Put_Dbg ("0: exit");
            exit;
         when 1 =>
            --  Here we want the register number, not the value in it
            A := Memory (PC + 1);
            B := Value_From_Mem (PC + 2);
            Registers (A) := B;
            PC_Inc (3);
            Put_Dbg ("1: set" & A'Image & B'Image);
         when 2 =>
            A := Value_From_Mem (PC + 1);
            PC_Inc (2);
            Stack.Append (A);
            Put_Dbg ("2: push" & A'Image);
         when 3 =>
            --  Here we want the memory address / register number, not the value in it
            A := Memory (PC + 1);
            PC_Inc (2);
            if Stack.Is_Empty /= True then
               Value_To_Mem (A, Stack.Last_Element);
               Stack.Delete_Last;
            end if;
            Put_Dbg ("3: pop" & A'Image);
         when 4 =>
            --  Here we want the memory address / register number, not the value in it
            A := Memory (PC + 1);
            B := Value_From_Mem (PC + 2);
            C := Value_From_Mem (PC + 3);
            PC_Inc (4);
            Int_To_Mem (A, (if B = C then 1 else 0));
            Put_Dbg ("4: eq" & A'Image & B'Image & C'Image);
         when 5 =>
            --  Here we want the memory address / register number, not the value in it
            A := Memory (PC + 1);
            B := Value_From_Mem (PC + 2);
            C := Value_From_Mem (PC + 3);
            PC_Inc (4);
            Int_To_Mem (A, (if B > C then 1 else 0));
            Put_Dbg ("5: gt" & A'Image & B'Image & C'Image);
         when 6 =>
            A := Value_From_Mem (PC + 1);
            PC := UInt15 (A);
            Put_Dbg ("6: jmp" & A'Image);
         when 7 =>
            A := Value_From_Mem (PC + 1);
            B := Value_From_Mem (PC + 2);
            PC_Inc (3);
            if A /= 0 then
               PC := UInt15 (B);
            end if;
            Put_Dbg ("7: jt" & A'Image & B'Image);
         when 8 =>
            A := Value_From_Mem (PC + 1);
            B := Value_From_Mem (PC + 2);
            PC_Inc (3);
            if A = 0 then
               PC := UInt15 (B);
            end if;
            Put_Dbg ("8: jf" & A'Image & B'Image);
         when 9 =>
            --  Here we want the memory address / register number, not the value in it
            A := Memory (PC + 1);
            B := Value_From_Mem (PC + 2);
            C := Value_From_Mem (PC + 3);
            PC_Inc (4);
            Int_To_Mem (A, UInt15 (B) + UInt15 (C));
            Put_Dbg ("9: add" & A'Image & B'Image & C'Image);
         when 10 =>
            --  Here we want the memory address / register number, not the value in it
            A := Memory (PC + 1);
            B := Value_From_Mem (PC + 2);
            C := Value_From_Mem (PC + 3);
            PC_Inc (4);
            Int_To_Mem (A, UInt15 (B) * UInt15 (C));
            Put_Dbg ("10: mult" & A'Image & B'Image & C'Image);
         when 11 =>
            --  Here we want the memory address / register number, not the value in it
            A := Memory (PC + 1);
            B := Value_From_Mem (PC + 2);
            C := Value_From_Mem (PC + 3);
            PC_Inc (4);
            Int_To_Mem (A, UInt15 (B) mod UInt15 (C));
            Put_Dbg ("11: mod" & A'Image & B'Image & C'Image);
         when 12 =>
            --  Here we want the memory address / register number, not the value in it
            A := Memory (PC + 1);
            B := Value_From_Mem (PC + 2);
            C := Value_From_Mem (PC + 3);
            PC_Inc (4);
            Value_To_Mem (A, B and C);
            Put_Dbg ("12: and" & A'Image & B'Image & C'Image);
         when 13 =>
            --  Here we want the memory address / register number, not the value in it
            A := Memory (PC + 1);
            B := Value_From_Mem (PC + 2);
            C := Value_From_Mem (PC + 3);
            PC_Inc (4);
            Value_To_Mem (A, B or C);
            Put_Dbg ("13: or" & A'Image & B'Image & C'Image);
         when 14 =>
            --  Here we want the memory address / register number, not the value in it
            A := Memory (PC + 1);
            B := Value_From_Mem (PC + 2);
            PC_Inc (3);
            --  Has to be 15-bit bitwise, according to spec, unlike other bitwise ops
            Int_To_Mem (A, not UInt15 (B));
            Put_Dbg ("14: not" & A'Image & B'Image);
         when 15 =>
            A := Memory (PC + 1);
            B := Value_From_Mem (UInt15 (Value_From_Mem (PC + 2)));
            Value_To_Mem (A, B);
            PC_Inc (3);
            Put_Dbg ("15: rmem" & A'Image & B'Image);
         when 16 =>
            A := Value_From_Mem (PC + 1);
            B := Value_From_Mem (PC + 2);
            Value_To_Mem (A, B);
            PC_Inc (3);
            Put_Dbg ("16: wmem" & A'Image & B'Image);
         when 17 =>
            A := Value_From_Mem (PC + 1);
            Stack.Append (UInt16 (PC + 2));
            PC := UInt15 (A);
            Put_Dbg ("17: call" & A'Image);
         when 18 =>
            if Stack.Is_Empty /= True then
               PC := UInt15 (Stack.Last_Element);
               Stack.Delete_Last;
            else
               exit;
            end if;
            Put_Dbg ("18: ret");
         when 19 =>
            A := Value_From_Mem (PC + 1);
            Put (Character'Val (Integer (A)));
            PC_Inc (2);
            Put_Dbg ("19: out");
         when 20 =>
            A := Memory (PC + 1);
            --  Value_To_Mem (A, UInt16 (110));
            declare
               Ch : Character;
            begin
               --  Get (Ch) doesn't pass newlines, so we use this construction.
               Character'Read (Input_Stream, Ch);
               Value_To_Mem (A, UInt16 (Character'Pos (Ch)));
               Put_Dbg ("Got char " & Character'Pos (Ch)'Img);
            end;
            PC_Inc (2);
            Put_Dbg ("20: in" & A'Image);
         when 21 =>
            PC_Inc (1);
            Put_Dbg ("21: noop");
         when others =>
            Put_Dbg ("-- Unknown code --");
            Put_Dbg ("PC value: " & Integer'Image (Integer (PC)));
            Put_Dbg ("PC point: " & Integer'Image (Integer (Memory (PC))));
            exit;
      end case;
   end loop;
end Synacorvm;
