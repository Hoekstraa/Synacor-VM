with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
with Hardware;

procedure Synacorvm is
begin
   --  Hardware.Stack.Append (10);
   --  Put_Line ("Vector has " & Ada.Containers.Count_Type'Image (Hardware.Stack.Length) & " elements");

   Put_Line ("Loading disk image into memory.");
   Hardware.Load_Binary;
   Put_Line ("Done, starting machine.");
   Put_Line ("");

   loop
      case Hardware.Memory (Hardware.PC) is
         when 0 =>
            Put_Line (Standard_Error, "0: exit");
            exit;
         when 19 =>
            Put ("TODO");
            exit;
            Hardware.PC_Inc (2);
         when 21 =>
            Put_Line (Standard_Error, "21: noop");
            Hardware.PC_Inc (1);
         when others =>
            Put_Line (Standard_Error, "Unknown code");
            Put_Line (Standard_Error, "PC value: " & Integer'Image (Integer (Hardware.PC)));
            Put_Line (Standard_Error, "PC point: " & Integer'Image (Integer (Hardware.Memory (Hardware.PC))));
      end case;
   end loop;
end Synacorvm;
