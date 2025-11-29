--  with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Hardware; use Hardware;
with Ada.Containers;

procedure Synacorvm is
   --  For temporary storage of operands
   A : Hardware.UInt16;
   B : Hardware.UInt16;
   C : Hardware.UInt16;
begin
   --  Hardware.Stack.Append (10);
   --  Put_Line ("Vector has " & Ada.Containers.Count_Type'Image (Hardware.Stack.Length) & " elements");

   Put_Line ("Loading disk image into memory.");
   Hardware.Load_Binary;
   Put_Line ("Done, starting machine.");
   Put_Line ("");

   loop

      Put_Line (Standard_Error, "----------");
      Put_Line (Standard_Error, "PC value: " & Integer'Image (Integer (Hardware.PC)));
      Put_Line (Standard_Error, "PC point: " & Integer'Image (Integer (Hardware.Memory (Hardware.PC))));
      Put_Line (Standard_Error, "Next instruction:"
                                & Hardware.Memory (Hardware.PC)'Image
                                & Hardware.Memory (Hardware.PC + 1)'Image
                                & Hardware.Memory (Hardware.PC + 2)'Image);
      Put_Line ("Vector has " & Ada.Containers.Count_Type'Image (Hardware.Stack.Length) & " elements");
      if Hardware.Stack.Is_Empty /= True then
         Put_Line ("Vector has " & Integer (Hardware.Stack.First_Element)'Image);
      end if;
      Put_Line (Standard_Error, "----------");
      case Hardware.Memory (Hardware.PC) is
         when 0 =>
            Put_Line (Standard_Error, "0: exit");
            exit;
         when 1 =>
            -- Here we want the register number, not the value in it
            A := Hardware.Memory (Hardware.PC + 1);
            B := Hardware.Value_From_Mem (Hardware.PC + 2);
            Hardware.Registers (A) := B;
            Hardware.PC_Inc (3);
            Put_Line (Standard_Error, "1: set" & A'Image & B'Image);
         when 2 =>
            A := Hardware.Value_From_Mem (Hardware.PC + 1);
            Hardware.PC_Inc (2);
            Hardware.Stack.Append(A);
            Put_Line (Standard_Error, "2: push" & A'Image);
         when 3 =>
            -- Here we want the memory address / register number, not the value in it
            A := Hardware.Memory (Hardware.PC + 1);
            Hardware.PC_Inc (2);
            if Hardware.Stack.Is_Empty /= True then
               Hardware.Value_To_Mem (A, Hardware.Stack.Last_Element);
               Hardware.Stack.Delete_Last;
            end if;
            Put_Line (Standard_Error, "3: pop" & A'Image);
         when 4 =>
            -- Here we want the memory address / register number, not the value in it
            A := Hardware.Memory (Hardware.PC + 1);
            B := Hardware.Value_From_Mem (Hardware.PC + 2);
            C := Hardware.Value_From_Mem (Hardware.PC + 3);
            Hardware.PC_Inc (4);
            Hardware.Int_To_Mem (A, (if B = C then 1 else 0));
            Put_Line (Standard_Error, "4: eq" & A'Image & B'Image & C'Image);
         when 5 =>
            -- Here we want the memory address / register number, not the value in it
            A := Hardware.Memory (Hardware.PC + 1);
            B := Hardware.Value_From_Mem (Hardware.PC + 2);
            C := Hardware.Value_From_Mem (Hardware.PC + 3);
            Hardware.PC_Inc (4);
            Hardware.Int_To_Mem (A, (if B > C then 1 else 0));
            Put_Line (Standard_Error, "9: add" & A'Image & B'Image & C'Image);
         when 6 =>
            A := Hardware.Value_From_Mem (Hardware.PC + 1);
            Hardware.PC := Hardware.UInt15 (A);
            Put_Line (Standard_Error, "6: jmp" & A'Image);
         when 7 =>
            A := Hardware.Value_From_Mem (Hardware.PC + 1);
            B := Hardware.Value_From_Mem (Hardware.PC + 2);
            Hardware.PC_Inc (3);
            if A /= 0 then
               Hardware.PC := Hardware.UInt15 (B);
            end if;
            Put_Line (Standard_Error, "7: jt" & A'Image & B'Image);
         when 8 =>
            A := Hardware.Value_From_Mem (Hardware.PC + 1);
            B := Hardware.Value_From_Mem (Hardware.PC + 2);
            Hardware.PC_Inc (3);
            if A = 0 then
               Hardware.PC := Hardware.UInt15 (B);
            end if;
            Put_Line (Standard_Error, "8: jf" & A'Image & B'Image);
         when 9 =>
            -- Here we want the memory address / register number, not the value in it
            A := Hardware.Memory (Hardware.PC + 1);
            B := Hardware.Value_From_Mem (Hardware.PC + 2);
            C := Hardware.Value_From_Mem (Hardware.PC + 3);
            Hardware.PC_Inc (4);
            Hardware.Int_To_Mem (A, UInt15(B) + UInt15(C));
            Put_Line (Standard_Error, "9: add" & A'Image & B'Image & C'Image);
         when 12 =>
            -- Here we want the memory address / register number, not the value in it
            A := Hardware.Memory (Hardware.PC + 1);
            B := Hardware.Value_From_Mem (Hardware.PC + 2);
            C := Hardware.Value_From_Mem (Hardware.PC + 3);
            Hardware.PC_Inc (4);
            Hardware.Value_To_Mem (A, B and C);
            Put_Line (Standard_Error, "12: and" & A'Image & B'Image & C'Image);
         when 13 =>
            -- Here we want the memory address / register number, not the value in it
            A := Hardware.Memory (Hardware.PC + 1);
            B := Hardware.Value_From_Mem (Hardware.PC + 2);
            C := Hardware.Value_From_Mem (Hardware.PC + 3);
            Hardware.PC_Inc (4);
            Hardware.Value_To_Mem (A, B or C);
            Put_Line (Standard_Error, "13: or" & A'Image & B'Image & C'Image);
         when 14 =>
            -- Here we want the memory address / register number, not the value in it
            A := Hardware.Memory (Hardware.PC + 1);
            B := Hardware.Value_From_Mem (Hardware.PC + 2);
            Hardware.PC_Inc (3);
            -- Has to be 15-bit bitwise, according to spec, unlike other bitwise ops
            Hardware.Int_To_Mem (A, not Hardware.UInt15 (B));
            Put_Line (Standard_Error, "14: not" & A'Image & B'Image);
         when 17 =>
            A := Hardware.Value_From_Mem (Hardware.PC + 1);
            B := Hardware.Memory (Hardware.PC + 2);
            Hardware.Stack.Append(Hardware.UInt16(Hardware.PC + 2));
            Hardware.PC := Hardware.UInt15(A);
            Put_Line (Standard_Error, "17: call" & A'Image);
         when 19 =>
            A := Hardware.Value_From_Mem (Hardware.PC + 1);
            Put (Character'Val (Integer (A)));
            Hardware.PC_Inc (2);
         when 21 =>
            Hardware.PC_Inc (1);
            Put_Line (Standard_Error, "21: noop");
         when others =>
            Put_Line (Standard_Error, "-- Unknown code --");
            Put_Line (Standard_Error, "PC value: " & Integer'Image (Integer (Hardware.PC)));
            Put_Line (Standard_Error, "PC point: " & Integer'Image (Integer (Hardware.Memory (Hardware.PC))));
            exit;
      end case;
   end loop;
end Synacorvm;
