with Ada.Sequential_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Hardware is

   procedure Load_Binary is
      package Binary_IO is
      new Ada.Sequential_IO (UInt16);
      use Binary_IO;

      F : Binary_IO.File_Type;
      File_Name : constant String := "./resources/challenge.bin";

      Memory_Array_Index : UInt15 := 0;
   begin
      declare
         Value : UInt16;
      begin
         Open (F, In_File, File_Name);
         while not End_Of_File (F) loop
            Read (F, Value);
            Memory (Memory_Array_Index) := Value;
            -- Ada.Text_IO.Put_Line (Integer'Image (Integer (Value)));
            Memory_Array_Index := Memory_Array_Index + 1;
         end loop;
         Close (F);
      end;
   end Load_Binary;

   procedure PC_Inc(amount : Integer := 1) is
   begin
      PC := PC + UInt15(amount);
   end PC_Inc;


   function Value_From_Mem(address : UInt16) return UInt16 is
   begin
      case address is
         when 0 .. 2 ** 15-1 =>
            return Memory (UInt15(address));
         when RegisterInt =>
            return Registers (address);
         when others =>
            Put_Line (Standard_Error, "Invalid address, stopping");
            raise Constraint_Error with "Invalid address";
      end case;
   end Value_From_Mem;

   function Value_From_Mem(address : UInt15) return UInt16 is
   begin
      return Value_From_Mem(UInt16(address));
   end Value_From_Mem;

end Hardware;
