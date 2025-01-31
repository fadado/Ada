package Music is
   ---------------------------------
   -- Encoded pitch and intervals --
   ---------------------------------

   type    PITCH is range 0..127;
   type    PITCH_INTERVAL is range -127..+127;
   subtype UNORDERED_INTERVAL is PITCH_INTERVAL range 0..127;

   -- Ordered pitch interval
   function Distance(x, y: PITCH) return PITCH_INTERVAL
      is (PITCH_INTERVAL(y - x)) with Inline;

   -- Pich transposition
   function Transposition(i: PITCH_INTERVAL; x: PITCH) return PITCH
      is (x + PITCH'Base(i)) with Inline;

   -- Unordered pitch interval
   -- "abs"(i: PITCH_INTERVAL): UNORDERED_INTERVAL
   --     abs Distance(x, y)

   ---------------------------------------
   -- Encoded pitch-class and intervals --
   ---------------------------------------

   type    PITCH_CLASS is mod 12;
   subtype PC_INTERVAL is PITCH_CLASS;
   type    INTERVAL_CLASS is range 0..6;

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

   -- PC_INTERVAL properties
   -------------------------
   -- Identity:
   --    0
   -- Combination:
   --    i + j
   -- Complement:
   --    -i
   -- Laws:
   --    i + 0 = i
   --    i + -i = 0
   --    (i+j)+k = i+(j+k)
   --    i + j = j + i

   -- Ordered pitch-class interval ("directed" interval)
   function Distance(x, y: PITCH_CLASS) return PC_INTERVAL
      is (y - x) with Inline;
   -- forall x,y,z in PITCH_CLASS: D(x,y) + D(y,z) = D(x,z)
   -- forall x,y   in PITCH_CLASS: D(x,y) = -D(y,x)

   function Transposition(i: PC_INTERVAL; x: PITCH_CLASS) return PITCH_CLASS
      is (x + i) with Inline;
   -- forall i in PC_INTERVAL, forall x in PITCH_CLASS: D(x,T(i,x)) = i

   function Inversion(i: PC_INTERVAL; x: PITCH_CLASS) return PITCH_CLASS
      is (i - x) with Inline;
   -- forall i in PC_INTERVAL, forall x in PITCH_CLASS: I(i,x) = T(i,-x)

   -- Unordered pitch-class interval
   function "abs"(i: PC_INTERVAL) return INTERVAL_CLASS
      is (INTERVAL_CLASS(if i < 7 then i else -i)) with Inline;

   ---------------------
   -- pitch-class set --
   ---------------------

   type PC_SET is mod 2**PITCH_CLASS'Modulus;
   type SET_COUNT is range 0..PITCH_CLASS'Modulus;

   VOID : constant PC_SET := 16#000#;
   FULL : constant PC_SET := 16#FFF#;

   BitSet : constant array (PITCH_CLASS) of PC_SET :=
      (1,2,4,8,16,32,64,128,256,512,1024,2048);

   function Cardinality(s: PC_SET) return SET_COUNT;

   function Member(x: PITCH_CLASS; s: PC_SET) return BOOLEAN is
      ((BitSet(x) and s) /= VOID) with Inline;

   function Transposition(i: PC_INTERVAL; s: PC_SET) return PC_SET;
   function Inversion(i: PC_INTERVAL; s: PC_SET) return PC_SET;

   -- Add(x: PITCH_CLASS; s: PC_SET): BitSet(x) or s
   -- Remove(x: PITCH_CLASS; s: PC_SET): not BitSet(x) and s
   -- Complement(s: PC_SET): not s and FULL
   -- Union(s,t: PC_SET): s or t
   -- Intersection(s,t: PC_SET): s and t
   -- Difference(s,t: PC_SET): s and not t
   -- Symm_Difference(s,t: PC_SET): s xor t
   -- Is_Subset(s,t: PC_SET): (s and t) = s
   -- Are_Disjoin(s,t: PC_SET): (s and t) = VOID

end Music;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
