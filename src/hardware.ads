with Ada.Containers.Vectors;

package Hardware is
   type UInt16 is mod 2 ** 16;
   type UInt15 is mod 2 ** 15;
   --  All math is modulo this number, as numbers are 15-bit
   Math_Modulus : constant Integer := 2 ** 15;

   --  Programcounter
   PC : UInt15 := 0;

   subtype RegisterInt is UInt16 range 32768 .. 32775;
   type RegistersT is array (RegisterInt) of UInt16;
   Registers : RegistersT := (others => 0);

   type MemoryT is array (UInt15) of UInt16;
   Memory : MemoryT := (others => 0);

   package UInt16_Vectors is new
     Ada.Containers.Vectors
       (Index_Type => Natural,
        Element_Type => UInt16);
   Stack : UInt16_Vectors.Vector;

   procedure Load_Binary;
   procedure PC_Inc (amount : Integer := 1);
   function Value_From_Mem(address : UInt15) return UInt16;
   procedure Value_To_Mem(address : UInt16;
                          value : UInt16);
   procedure Int_To_Mem(address : UInt16;
                        value : UInt15);

end Hardware;
