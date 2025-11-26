with Ada.Sequential_IO;
with Ada.Text_IO;

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
end Hardware;
