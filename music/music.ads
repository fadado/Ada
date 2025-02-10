with Ada.Containers.Generic_Array_Sort;

package Music is
   ---------------------------------------
   -- Encoded pitch-class and intervals --
   ---------------------------------------

   type    PITCH_CLASS is mod 12;
   subtype PC_INTERVAL is PITCH_CLASS;
   type    INTERVAL_CLASS is range 0..6;

   -- PC_INTERVAL properties
   -------------------------
   -- Identity element:
   --    0
   -- Combination operation:
   --    i + j
   -- Complement operation:
   --    -i
   -- Laws:
   --    i + 0 = i
   --    i + -i = 0
   --    (i+j)+k = i+(j+k)
   --    i + j = j + i

   -- Laws for Interval (D), Transposition (T) and Inversion (I)
   -------------------------------------------------------------
   --    forall x,y,z in PITCH_CLASS:
   --       D(x,y) + D(y,z) = D(x,z)
   --    forall x,y   in PITCH_CLASS:
   --       D(x,y) = -D(y,x)
   --    forall i in PC_INTERVAL, forall x in PITCH_CLASS:
   --       D(x,T(i,x)) = i
   --    forall i in PC_INTERVAL, forall x in PITCH_CLASS:
   --       I(i,x) = T(i,-x)

   -- Ordered pitch-class interval
   function Interval(x, y: PITCH_CLASS) return PC_INTERVAL
      is (y - x) with Inline;

   function Transposition(i: PC_INTERVAL; x: PITCH_CLASS) return PITCH_CLASS
      is (x + i) with Inline;

   function Inversion(i: PC_INTERVAL; x: PITCH_CLASS) return PITCH_CLASS
      is (i - x) with Inline;

   -- Unordered pitch-class interval
   function "abs"(i: PC_INTERVAL) return INTERVAL_CLASS
      is (INTERVAL_CLASS(if i < 7 then i else -i)) with Inline;

   -- Expression examples
   ----------------------
   -- Get_PC(x: PITCH): PITCH_CLASS
   --    PITCH_CLASS'Mod(x)
   -- Congruent(x,y: PITCH): BOOLEAN
   --    PITCH_CLASS'Mod(x) = PITCH_CLASS'Mod(y)
   -- Member(x: PITCH; pc: PITCH_CLASS): BOOLEAN
   --    PITCH_CLASS'Mod(x) = pc
   -- Invert(x: PITCH_CLASS): PITCH_CLASS
   --    -x
   -- Member(i: PC_INTERVAL; ic: INTERVAL_CLASS): BOOLEAN
   --   abs i = ic

   ---------------------
   -- pitch-class set --
   ---------------------

   type PC_SET is mod 2**PITCH_CLASS'Modulus;
   type SET_COUNT is range 0..PITCH_CLASS'Modulus;

   VOID : constant PC_SET := 16#000#;
   FULL : constant PC_SET := 16#FFF#;

   BitSet : constant array (PITCH_CLASS) of PC_SET :=
      (2048,1024,512,256,128,64,32,16,8,4,2,1);

   function Cardinality(s: PC_SET) return SET_COUNT;

   function Member(x: PITCH_CLASS; s: PC_SET) return BOOLEAN is
      ((BitSet(x) and s) /= VOID) with Inline;

   function Transposition(i: PC_INTERVAL; s: PC_SET) return PC_SET;
   function Inversion(i: PC_INTERVAL; s: PC_SET) return PC_SET;

   function Transpositions(s: PC_SET) return SET_COUNT;

   -- More algebra of sets
   -----------------------
   -- Add(x: PITCH_CLASS; s: PC_SET): BitSet(x) or s
   -- Remove(x: PITCH_CLASS; s: PC_SET): not BitSet(x) and s
   -- Complement(s: PC_SET): not s and FULL
   -- Union(s,t: PC_SET): s or t
   -- Intersection(s,t: PC_SET): s and t
   -- Difference(s,t: PC_SET): s and not t
   -- Symm_Difference(s,t: PC_SET): s xor t
   -- Is_Subset(s,t: PC_SET): (s and t) = s
   -- Are_Disjoin(s,t: PC_SET): (s and t) = VOID

   -----------------------------
   -- pitch-class ordered set --
   -----------------------------

   type INDEX is range 1..12;
   type ORDER is array (INDEX range <>) of PITCH_CLASS;

   procedure Sort is new 
      Ada.Containers.Generic_Array_Sort (
         INDEX, PITCH_CLASS, ORDER
   );

   function Cardinality(s: ORDER) return SET_COUNT
      is (s'Length) with Inline;

   function Member(x: PITCH_CLASS; s: ORDER) return BOOLEAN
      is (for some k in s'Range => x = s(k)) with Inline;

   function Transposition(i: PC_INTERVAL; s: ORDER) return ORDER;
   function Inversion(i: PC_INTERVAL; s: ORDER) return ORDER;

 -- TODO:
 --function Position(i: PC_INTERVAL; s: ORDER) return INDEX;
 --function Retrograde(s: ORDER) return ORDER;
 --function Rotate(s: ORDER; n: INDEX:=1) return ORDER;

   -----------------
   -- Set <=> Seq --
   -----------------

   function Seq(s: PC_SET) return ORDER;
   function Set(s: ORDER)  return PC_SET;

   ----------------------
   -- Interval pattern --
   ----------------------

   type INTERVAL_PATTERN is array (INDEX range <>) of PC_INTERVAL;

   function Pattern(s: ORDER) return INTERVAL_PATTERN;

end Music;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
