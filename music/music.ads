package Music is
   ---------------------------------
   -- Encoded pitch and intervals --
   ---------------------------------

   type    PITCH is range 0..127;
   type    PITCH_INTERVAL is range -127..+127;
   subtype UNORDERED_INTERVAL is PITCH_INTERVAL range 0..127;

   -- Ordered pitch interval
   function Distance(x, y: PITCH) return PITCH_INTERVAL
      with Inline;

   -- Unordered pitch interval:
   --     abs Distance(x, y)
   -- function "abs"(i: PITCH_INTERVAL) return UNORDERED_INTERVAL;

   -- Pich transposition
   function Transposition(x: PITCH; i: PITCH_INTERVAL) return PITCH
      with Inline;

   ---------------------------------------
   -- Encoded pitch-class and intervals --
   ---------------------------------------

   type    PITCH_CLASS is mod 12;
   subtype PC_INTERVAL is PITCH_CLASS;
   type    INTERVAL_CLASS is mod 7;

   -- Obtain pitch-class from a pich
   --   PITCH_CLASS'Mod(x)

   -- Congruent(x,y: PITCH):
   --   PITCH_CLASS'Mod(x) = PITCH_CLASS'Mod(y)

   -- Member(x: PITCH; pc: PITCH_CLASS):
   --   PITCH_CLASS'Mod(x) = pc

   -- Equivalent(i,j: PC_INTERVAL):
   --   abs i = abs j

   -- Complement(i: PC_INTERVAL):
   --   -i

   -- Member(i: PC_INTERVAL; ic: INTERVAL_CLASS):
   --   abs i = ic

   -- Ordered pitch-class interval ("directed" interval)
   function Distance(x, y: PITCH_CLASS) return PC_INTERVAL
      with Inline;

   function Transposition(x: PITCH_CLASS; i: PC_INTERVAL) return PITCH_CLASS
      with Inline;

   function Inversion(x: PITCH_CLASS; i: PC_INTERVAL) return PITCH_CLASS
      with Inline;

   -- Unordered pitch-class interval
   function "abs"(i: PC_INTERVAL) return INTERVAL_CLASS
      with Inline;

end Music;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
