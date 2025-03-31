package body Music is

   ---------------------
   -- pitch-class set --
   ---------------------

   function Cardinality
     (s : PC_SET) return SET_COUNT
   is
   begin
      return count : SET_COUNT := 0 do
         case s is
            when 16#000# => null;
            when 16#FFF# => count := SET_COUNT'Last;
            when others =>
               for t of BitSet loop
                  if (t and s) /= 16#000# then
                     count := count+1;
                  end if;
               end loop;
         end case;
      end return;
   end Cardinality;

   function map_set
     (s : PC_SET;
      f : access function (x: PITCH_CLASS) return PITCH_CLASS)
     return PC_SET
   is
   begin
      return t : PC_SET := 16#000# do
         for x in PITCH_CLASS loop
            if (BitSet(x) and s) /= 16#000# then
               t := BitSet(f(x)) or t;
            end if;
         end loop;
      end return;
   end map_set;

   function Transposition
     (i : PC_INTERVAL;
      s : PC_SET) return PC_SET
   is
      function f (x: PITCH_CLASS) return PITCH_CLASS
      is (Transposition(i, x));
   begin
      return map_set(s, f'Access);
   end Transposition;

   function Inversion
     (i : PC_INTERVAL;
      s : PC_SET) return PC_SET
   is
      function f (x: PITCH_CLASS) return PITCH_CLASS
      is (Inversion(i, x));
   begin
      return map_set(s, f'Access);
   end Inversion;

   function Transpositions
     (s : PC_SET) return SET_COUNT
   is
      function gcd (m, n: SET_COUNT) return SET_COUNT
      is
         a, b, c : SET_COUNT;
      begin
         a := m; b := n;
         while b > 0 loop
            c := a; a := b; b := (c mod b);
         end loop;
         return a;
      end gcd;

      Z : constant SET_COUNT := SET_COUNT'Last;
   begin
      return Z / gcd(Z, Cardinality(s));
   end Transpositions;

   function Generate
     (origin : PITCH_CLASS;
      d      : PC_INTERVAL;
      g      : PC_INTERVAL) return PC_SET
   is
      t : PC_SET;
   begin
      return s : PC_SET := 16#000# do
         for i in PC_INTERVAL'First..PC_INTERVAL'First+d-1 loop
            t := BitSet(origin + i * g);
            exit when (s and t) /= 16#000#; -- cycle detected
            s := s or t;
         end loop;
      end return;
   end Generate;

   -----------------------------
   -- pitch-class ordered set --
   -----------------------------

   function Transposition
     (i : PC_INTERVAL;
      s : PC_TUPLE) return PC_TUPLE
   is
      function f (x: PITCH_CLASS) return PITCH_CLASS
      is (Transposition(i, x)) with Inline;

      function apply is
         new PC_Tuple_Place.Mapper (f);
   begin
      return apply(s);
   end Transposition;

   function Inversion
     (i : PC_INTERVAL;
      s : PC_TUPLE) return PC_TUPLE
   is
      function f (x: PITCH_CLASS) return PITCH_CLASS
      is (Inversion(i, x)) with Inline;

      function apply is
         new PC_Tuple_Place.Mapper (f);
   begin
      return apply(s);
   end Inversion;

   -------------------
   -- Set <=> Tuple --
   -------------------

   function Set
     (s : PC_TUPLE) return PC_SET
   is
   begin
      return t : PC_SET := 16#000# do
         for x of s loop
            t := BitSet(x) or t;
         end loop;
      end return;
   end Set;

   function Tuple
     (s : PC_SET) return PC_TUPLE
   is
      k : TUPLE_INDEX := TUPLE_INDEX'First;
      h : constant TUPLE_INDEX := k + TUPLE_INDEX(Cardinality(s)) - 1;
   begin
      return t : PC_TUPLE(k..h) do
         for x in PITCH_CLASS loop
            if (BitSet(x) and s) /= 16#000# then
               t(k) := x;
               exit when k = TUPLE_INDEX'Last;
               k := k+1;
            end if;
         end loop;
      end return;
   end Tuple;

   ----------------------
   -- Interval pattern --
   ----------------------

   function invariant_octave
     (intervals : INTERVAL_PATTERN) return BOOLEAN
   is
      function sum return INTEGER
      is
         a : NATURAL := 0;
      begin
         for n of intervals loop
            a := a + NATURAL(n);
         end loop;
         return a;
      end sum;

   begin
      return sum = 12;
   end invariant_octave;

   function Pattern
     (s : PC_TUPLE) return INTERVAL_PATTERN
   is
   begin
      return intervals : INTERVAL_PATTERN(s'Range) do
         for k in s'First+1..s'Last loop
            intervals(k-1) := Interval(s(k-1), s(k));
         end loop;
         intervals(s'Last) := Interval(s(s'Last), s(s'First));
      end return;
   end Pattern;

end Music;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
