--  with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams.Stream_IO;
with Hardware; use Hardware;
with Ada.Containers;
with Operations;

procedure Synacorvm is
   type Mem is access UInt16;
   --  For temporary storage of operands

   PC_Op : Hardware.UInt16;
   A : Hardware.UInt16;
   B : Hardware.UInt16;
   C : Hardware.UInt16;

   --  Memory-indexed values of A/B/C
   A_Mem : Hardware.UInt16;
   B_Mem : Hardware.UInt16;
   C_Mem : Hardware.UInt16;

   --  Create a stream from the Standard Input.
   Input_File : constant Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Input;
   Input_Stream : constant access Ada.Streams.Root_Stream_Type'Class :=
     Ada.Text_IO.Text_Streams.Stream (File => Input_File);

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
      --  NOTE, COULD CAUSE A BUG WHEN AT THE VERY LAST BYTES OF MEMORY!
      PC_Op := Memory (PC);
      A := Memory (PC + 1);
      B := Memory (PC + 2);
      C := Memory (PC + 3);

      A_Mem := Value_From_Mem (PC + 1);
      B_Mem := Value_From_Mem (PC + 2);
      C_Mem := Value_From_Mem (PC + 3);

      Put_Dbg ("----------");
      Put_Dbg ("PC value: " & Integer'Image (Integer (PC)));
      Put_Dbg ("PC point: " & Integer'Image (Integer (PC_Op)));
      Put_Dbg ("Next instruction:"
               & Operations.Ops (Integer (PC_Op)).Name & ":"
               & PC_Op'Image
               & A'Image
               & B'Image
               & C'Image);
      Put_Dbg ("Stack has "
               & Ada.Containers.Count_Type'Image (Stack.Length)
               & " elements");
      Put_Dbg ("----------");

      case Memory (PC) is
         when 0 =>
            exit;
         when 1 =>
            Registers (A) := B_Mem;
         when 2 =>
            Stack.Append (A_Mem);
         when 3 =>
            if Stack.Is_Empty /= True then
               Value_To_Mem (A, Stack.Last_Element);
               Stack.Delete_Last;
            end if;
         when 4 =>
            Int_To_Mem (A, (if B_Mem = C_Mem then 1 else 0));
         when 5 =>
            Int_To_Mem (A, (if B_Mem > C_Mem then 1 else 0));
         when 6 =>
            PC := UInt15 (A_Mem);
         when 7 =>
            PC_Inc (3);
            if A_Mem /= 0 then
               PC := UInt15 (B_Mem);
            end if;
         when 8 =>
            PC_Inc (3);
            if A_Mem = 0 then
               PC := UInt15 (B_Mem);
            end if;
         when 9 =>
            Int_To_Mem (A, UInt15 (B_Mem) + UInt15 (C_Mem));
         when 10 =>
            Int_To_Mem (A, UInt15 (B_Mem) * UInt15 (C_Mem));
         when 11 =>
            Int_To_Mem (A, UInt15 (B_Mem) mod UInt15 (C_Mem));
         when 12 =>
            Value_To_Mem (A, B_Mem and C_Mem);
         when 13 =>
            Value_To_Mem (A, B_Mem or C_Mem);
         when 14 =>
            --  Has to be 15-bit bitwise, according to spec, unlike other bitwise ops
            Int_To_Mem (A, not UInt15 (B_Mem));
         when 15 =>
            B := Value_From_Mem (UInt15 (Value_From_Mem (PC + 2)));
            Value_To_Mem (A, B);
            Put_Dbg ("15: rmem" & A'Image & B'Image);
         when 16 =>
            Value_To_Mem (A_Mem, B_Mem);
         when 17 =>
            Stack.Append (UInt16 (PC + 2));
            PC := UInt15 (A_Mem);
         when 18 =>
            if Stack.Is_Empty /= True then
               PC := UInt15 (Stack.Last_Element);
               Stack.Delete_Last;
            else
               exit;
            end if;
         when 19 =>
            Put (Character'Val (Integer (A_Mem)));
         when 20 =>
            declare
               Ch : Character;
            begin
               --  Get (Ch) doesn't pass newlines, so we use this construction.
               Character'Read (Input_Stream, Ch);
               Value_To_Mem (A, UInt16 (Character'Pos (Ch)));
               Put_Dbg ("Got char " & Character'Pos (Ch)'Img);
            end;
         when 21 =>
            null;
         when others =>
            Put_Dbg ("-- Unknown code --");
            exit;
      end case;

      --  Put_Line (Operations.Ops (Integer (PC_Op)).Length'Img);
      PC_Inc (Integer (Operations.Ops (Integer (PC_Op)).Length));
   end loop;
end Synacorvm;
