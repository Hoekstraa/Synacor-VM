--  with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Hardware; use Hardware;

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
      -- Put_Line (Standard_Error, "Next instruction:"
      --                           & Hardware.Memory (Hardware.PC)'Image
      --                           & Hardware.Memory (Hardware.PC + 1)'Image
      --                           & Hardware.Memory (Hardware.PC + 2)'Image);
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
            Hardware.Int_To_Mem (A, UInt15(B + C));
            Put_Line (Standard_Error, "9: add" & A'Image & B'Image & C'Image);
         when 19 =>
            A := Hardware.Value_From_Mem (Hardware.PC + 1);
            Put (Character'Val (Integer (A)));
            Hardware.PC_Inc (2);
         when 21 =>
            Put_Line (Standard_Error, "21: noop");
            Hardware.PC_Inc (1);
         when others =>
            Put_Line (Standard_Error, "-- Unknown code --");
            Put_Line (Standard_Error, "PC value: " & Integer'Image (Integer (Hardware.PC)));
            Put_Line (Standard_Error, "PC point: " & Integer'Image (Integer (Hardware.Memory (Hardware.PC))));
            exit;
      end case;
   end loop;
end Synacorvm;
