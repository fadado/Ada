package body Music is

   ---------------------
   -- pitch-class set --
   ---------------------

   function Cardinality(s: PC_SET) return SET_COUNT is
   begin
      return a : SET_COUNT := 0 do
         if s = VOID then
            null;
         elsif s = FULL then
            a := SET_COUNT'Last;
         else
            for t of BitSet loop
               if (t and s) /= VOID then
                  a := a+1;
               end if;
            end loop;
         end if;
      end return;
   end Cardinality;

   function map(s: PC_SET;
                f: access function (x: PITCH_CLASS) return PITCH_CLASS)
     return PC_SET is
   begin
      return t : PC_SET := VOID do
         if s = VOID then
            null;
         elsif s = FULL then
            -- t := FULL; ???
            for x in PITCH_CLASS loop
               t := BitSet(f(x)) or t;
            end loop;
         else
            for x in PITCH_CLASS loop
               if (BitSet(x) and s) /= VOID then
                  t := BitSet(f(x)) or t;
               end if;
            end loop;
         end if;
      end return;
   end map;

   function Transposition(i: PC_INTERVAL; s: PC_SET) return PC_SET
   is
      function f(x: PITCH_CLASS) return PITCH_CLASS is (x + i);
   begin
      return map(s, f'Access);
   end Transposition;

   function Inversion(i: PC_INTERVAL; s: PC_SET) return PC_SET
   is
      function f(x: PITCH_CLASS) return PITCH_CLASS is (i - x);
   begin
      return map(s, f'Access);
   end Inversion;

   -----------------------------
   -- pitch-class ordered set --
   -----------------------------

   function invariant_no_dups(s: ORDER) return BOOLEAN
      is (s'Length < 2 or else
            (for all k in s'First..s'Last-1 =>
               (for all j in k+1..s'Last => s(j) /= s(k))))
      with Inline;

   function invariant_sorted(s: ORDER) return BOOLEAN
      is (s'Length < 2 or else
            (for all k in s'First..s'Last-1 => s(k) < s(k+1)))
      with Inline;

   function map(s: ORDER;
                f: access function (x: PITCH_CLASS) return PITCH_CLASS)
     return ORDER is
   begin
      pragma Assert(s'Length > 0);
      return t : ORDER(s'Range) do
         for k in s'Range loop
            t(k) := f(s(k));
         end loop;
      end return;
   end map;

   function Transposition(i: PC_INTERVAL; s: ORDER) return ORDER
   is
      function f(x: PITCH_CLASS) return PITCH_CLASS is (x + i);
   begin
      return map(s, f'Access);
   end Transposition;

   function Inversion(i: PC_INTERVAL; s: ORDER) return ORDER
   is
      function f(x: PITCH_CLASS) return PITCH_CLASS is (i - x);
   begin
      return map(s, f'Access);
   end Inversion;

   -----------------
   -- Set <=> Seq --
   -----------------

   function Seq(s: PC_SET) return ORDER
   is
      k : INDEX := INDEX'First;
   begin
      return t : ORDER(k..k+INDEX(Cardinality(s))-1) do
         if s = VOID then
            null;
         elsif s = FULL then
            t := (0,1,2,3,4,5,6,7,8,9,10,11);
         else
            for x in PITCH_CLASS loop
               if (BitSet(x) and s) /= VOID then
                  t(k) := x;
                  exit when k = INDEX'Last;
                  k := k+1;
               end if;
            end loop;
         end if;
         pragma Assert(invariant_sorted(t));
      end return;
   end Seq;

   function Set(s: ORDER) return PC_SET is
   begin
      return t : PC_SET := VOID do
         for k in s'Range loop
            t := BitSet(s(k)) or t;
         end loop;
      end return;
   end Set;

   ----------------------
   -- Interval pattern --
   ----------------------

   function invariant_octave(p: INTERVAL_PATTERN) return BOOLEAN
   is
      a : NATURAL := 0;
   begin
      for i of p loop
         a := a + NATURAL(i);
      end loop;
      return a = 12;
   end invariant_octave;

   function Pattern(s: ORDER) return INTERVAL_PATTERN is
   begin
      pragma Assert(s'Length > 1);
      return p : INTERVAL_PATTERN(s'Range) do
         for k in s'First+1..s'Last loop
            p(k-1) := Distance(s(k-1), s(k));
         end loop;
         p(s'Last) := Distance(s(s'Last), s(s'First));
         pragma Assert(invariant_octave(p));
      end return;
   end Pattern;

end Music;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
