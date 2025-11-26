with Ada.Containers.Vectors;

package Hardware is
   type UInt16 is mod 2 ** 16;
   type UInt15 is mod 2 ** 15;
   --  All math is modulo this number, as numbers are 15-bit
   Math_Modulus : Integer := 2 ** 15;

   --  Programcounter
   PC : UInt15 := 0;

   subtype RegisterInt is UInt16 range 32768 .. 32775;
   type Register is (R0, R1, R2, R3, R4, R5, R6, R7);
   for Register use (R0 => 32768,
                     R1 => 32769,
                     R2 => 32770,
                     R3 => 32771,
                     R4 => 32772,
                     R5 => 32773,
                     R6 => 32774,
                     R7 => 32775);

   type MemoryT is array (UInt15) of UInt16;
   Memory : MemoryT := (others => 0);

   package UInt16_Vectors is new
     Ada.Containers.Vectors
       (Index_Type => Natural,
        Element_Type => UInt16);
   Stack : UInt16_Vectors.Vector;

   procedure Load_Binary;
   procedure PC_Inc (amount : Integer := 1);
end Hardware;
