with Generics;

package Music is

   ---------------------------------------
   -- Encoded pitch-class and intervals --
   ---------------------------------------

   type PITCH_CLASS is mod 12;

   subtype PC_INTERVAL is PITCH_CLASS;

   type INTERVAL_CLASS is range 0..6;

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
   function Interval
     (x : PITCH_CLASS;
      y : PITCH_CLASS) return PC_INTERVAL
   is (y - x) with Inline;

   function Transposition
     (i : PC_INTERVAL;
      x : PITCH_CLASS) return PITCH_CLASS
   is (x + i) with Inline;

   function Inversion
     (i : PC_INTERVAL;
      x : PITCH_CLASS) return PITCH_CLASS
   is (i - x) with Inline;

   function Inversion
     (x : PITCH_CLASS) return PITCH_CLASS
   is (Inversion(0, x)) with Inline;

   -- Unordered pitch-class interval
   function "abs"
     (i : PC_INTERVAL) return INTERVAL_CLASS
   is (INTERVAL_CLASS(PC_INTERVAL'Min(i, -i)))  with Inline;

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

   type PC_SET is mod 2**PITCH_CLASS'Modulus
      with Default_Value => 16#000#;

   type SET_COUNT is range 0..PITCH_CLASS'Modulus
      with Default_Value => 0;

   BitSet : constant array (PITCH_CLASS) of PC_SET :=
     (2048,1024,512,256,128,64,32,16,8,4,2,1);

   function Cardinality
     (s : PC_SET) return SET_COUNT;

   function Member
     (x : PITCH_CLASS;
      s : PC_SET) return BOOLEAN
   is ((BitSet(x) and s) /= 16#000#) with Inline;

   function Transposition
     (i : PC_INTERVAL;
      s : PC_SET) return PC_SET;

   function Inversion
     (i : PC_INTERVAL;
      s : PC_SET) return PC_SET;

   function Inversion
     (s : PC_SET) return PC_SET
   is (Inversion(0, s)) with Inline;

   function Transpositions
     (s : PC_SET) return SET_COUNT;

   -- More algebra of sets
   -----------------------
   -- Add(x: PITCH_CLASS; s: PC_SET): BitSet(x) or s
   -- Remove(x: PITCH_CLASS; s: PC_SET): not BitSet(x) and s
   -- Complement(s: PC_SET): not s and 16#FFF#
   -- Union(s,t: PC_SET): s or t
   -- Intersection(s,t: PC_SET): s and t
   -- Difference(s,t: PC_SET): s and not t
   -- Symm_Difference(s,t: PC_SET): s xor t
   -- Is_Subset(s,t: PC_SET): (s and t) = s
   -- Are_Disjoin(s,t: PC_SET): (s and t) = 16#000#

   -----------------------------
   -- pitch-class ordered set --
   -----------------------------

   type TUPLE_INDEX is range 1..12; -- from single to dodecatuple

   type PC_TUPLE is array (TUPLE_INDEX range <>) of PITCH_CLASS;

   package PC_Tuple_Signature is
      new Generics.Tuple_Signature (
         Index_Type   => TUPLE_INDEX,
         Element_Type => PITCH_CLASS,
         Array_Type   => PC_TUPLE
   );

   package PC_Tuples is
      new Generics.Tuples (PC_Tuple_Signature);

   function Member
     (x : PITCH_CLASS;
      s : PC_TUPLE) return BOOLEAN
   renames PC_Tuples.Member;

   function Retrograde
     (s : PC_TUPLE) return PC_TUPLE
   renames PC_Tuples.Reversed;

   procedure Sort
     (s: in out PC_TUPLE)
   renames PC_Tuples.Sort;

   function Sorted
     (s : PC_TUPLE) return PC_TUPLE
   renames PC_Tuples.Sorted;

   function Rotate
     (n : TUPLE_INDEX;
      s : PC_TUPLE) return PC_TUPLE
   renames PC_Tuples.Rotated;

   function Rotate
     (s : PC_TUPLE) return PC_TUPLE
   is (Rotate(1, s)) with Inline;

   function Cardinality
     (s : PC_TUPLE) return SET_COUNT
   is (s'Length) with Inline;

   function Transposition
     (i : PC_INTERVAL;
      s : PC_TUPLE) return PC_TUPLE;

   function Inversion
     (i : PC_INTERVAL;
      s : PC_TUPLE) return PC_TUPLE;

   function Inversion
     (s : PC_TUPLE) return PC_TUPLE
   is (Inversion(0, s)) with Inline;

   -------------------
   -- Set <=> Tuple --
   -------------------

   function Tuple
     (s : PC_SET) return PC_TUPLE;

   function Set
     (s : PC_TUPLE) return PC_SET;

   ----------------------
   -- Interval pattern --
   ----------------------

   type INTERVAL_PATTERN is array (TUPLE_INDEX range <>) of PC_INTERVAL;

   function Pattern 
     (s : PC_TUPLE) return INTERVAL_PATTERN;

   --------------
   -- Generate --
   --------------

   function Generate
     (origin : PITCH_CLASS;
      d      : PC_INTERVAL;
      g      : PC_INTERVAL) return PC_SET;

end Music;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
