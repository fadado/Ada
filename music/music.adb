pragma Assertion_Policy(Check); -- Check / Ignore

package body Music is
   ---------------------
   -- pitch-class set --
   ---------------------

   function Cardinality(s: PITCH_CLASS_SET) return SET_COUNT is
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

   function map(s: PITCH_CLASS_SET;
                f: access function (x: PITCH_CLASS) return PITCH_CLASS)
     return PITCH_CLASS_SET is
   begin
      return t : PITCH_CLASS_SET := 16#000# do
         for x in PITCH_CLASS loop
            if (BitSet(x) and s) /= 16#000# then
               t := BitSet(f(x)) or t;
            end if;
         end loop;
      end return;
   end map;

   function Transposition(i: PITCH_CLASS_INTERVAL; s: PITCH_CLASS_SET) return PITCH_CLASS_SET
   is
      function f(x: PITCH_CLASS) return PITCH_CLASS is (x + i);
   begin
      return map(s, f'Access);
   end Transposition;

   function Inversion(i: PITCH_CLASS_INTERVAL; s: PITCH_CLASS_SET) return PITCH_CLASS_SET
   is
      function f(x: PITCH_CLASS) return PITCH_CLASS is (i - x);
   begin
      return map(s, f'Access);
   end Inversion;

   function Transpositions(s: PITCH_CLASS_SET) return SET_COUNT
   is
      function gcd(m, n: SET_COUNT) return SET_COUNT is
         a, b, c : SET_COUNT;
      begin
         a := m; b := n;
         while b /= 0 loop
            c := a; a := b; b := (c mod b);
         end loop;
         return a;
      end gcd;

      Z : constant SET_COUNT := SET_COUNT'Last;
   begin
      return Z / gcd(Z, Cardinality(s));
   end Transpositions;

   -----------------------------
   -- pitch-class ordered set --
   -----------------------------

   function invariant_no_dups(s: PITCH_CLASS_TUPLE) return BOOLEAN
      is (s'Length < 2 or else
            (for all k in s'First..s'Last-1 =>
               (for all h in k+1..s'Last => s(h) /= s(k))))
      with Inline;

   function invariant_sorted(s: PITCH_CLASS_TUPLE) return BOOLEAN
      is (s'Length < 2 or else
            (for all k in s'First..s'Last-1 => s(k) < s(k+1)))
      with Inline;

   function map(s: PITCH_CLASS_TUPLE;
                f: access function (x: PITCH_CLASS) return PITCH_CLASS)
     return PITCH_CLASS_TUPLE is
   begin
      pragma Assert(s'Length > 0);

      return t : PITCH_CLASS_TUPLE(s'Range) do
         for k in s'Range loop
            t(k) := f(s(k));
         end loop;
      end return;
   end map;

   function Transposition(i: PITCH_CLASS_INTERVAL; s: PITCH_CLASS_TUPLE) return PITCH_CLASS_TUPLE
   is
      function f(x: PITCH_CLASS) return PITCH_CLASS is (x + i);
   begin
      return map(s, f'Access);
   end Transposition;

   function Inversion(i: PITCH_CLASS_INTERVAL; s: PITCH_CLASS_TUPLE) return PITCH_CLASS_TUPLE
   is
      function f(x: PITCH_CLASS) return PITCH_CLASS is (i - x);
   begin
      return map(s, f'Access);
   end Inversion;

   function Position(i: PITCH_CLASS_INTERVAL; s: PITCH_CLASS_TUPLE) return TUPLE_INDEX is
   begin
      for k in s'Range loop
         if s(k) = i then
            return k;
         end if;
      end loop;
      raise Constraint_Error;
   end Position;

   function Retrograde(s: PITCH_CLASS_TUPLE) return PITCH_CLASS_TUPLE
   is
      k : TUPLE_INDEX := TUPLE_INDEX'First;
   begin
      return t : PITCH_CLASS_TUPLE(s'Range) do
         for x of reverse s loop
            t(k) := x;
            k := k+1;
         end loop;
      end return;
   end Retrograde;

   function Rotate(n: TUPLE_INDEX; s: PITCH_CLASS_TUPLE) return PITCH_CLASS_TUPLE is
   begin
      return t : PITCH_CLASS_TUPLE(s'Range) do
         t(s'First    .. s'Last-n) := s(s'First+n .. s'Last);
         t(s'Last-n+1 .. s'Last)   := s(s'First   .. s'First+n-1);
      end return;
   end Rotate;

   -----------------
   -- Set <=> Seq --
   -----------------

   function Seq(s: PITCH_CLASS_SET) return PITCH_CLASS_TUPLE
   is
      k : TUPLE_INDEX := TUPLE_INDEX'First;
      h : constant TUPLE_INDEX :=
            TUPLE_INDEX'First + TUPLE_INDEX(Cardinality(s)) - 1;
   begin
      return t : PITCH_CLASS_TUPLE(k..h) do
         for x in PITCH_CLASS loop
            if (BitSet(x) and s) /= 16#000# then
               t(k) := x;
               exit when k = TUPLE_INDEX'Last;
               k := k+1;
            end if;
         end loop;

         pragma Assert(invariant_sorted(t));
      end return;
   end Seq;

   function Set(s: PITCH_CLASS_TUPLE) return PITCH_CLASS_SET is
   begin
      return t : PITCH_CLASS_SET := 16#000# do
         for x of s loop
            t := BitSet(x) or t;
         end loop;
      end return;
   end Set;

   ----------------------
   -- Interval pattern --
   ----------------------

   function invariant_octave(intervals: INTERVAL_PATTERN) return BOOLEAN
   is
      a : NATURAL := 0;
   begin
      for i of intervals loop
         a := a + NATURAL(i);
      end loop;
      return a = 12;
   end invariant_octave;

   function Pattern(s: PITCH_CLASS_TUPLE) return INTERVAL_PATTERN is
   begin
      pragma Assert(s'Length > 1);

      return intervals : INTERVAL_PATTERN(s'Range) do
         for k in s'First+1..s'Last loop
            intervals(k-1) := Interval(s(k-1), s(k));
         end loop;
         intervals(s'Last) := Interval(s(s'Last), s(s'First));

         pragma Assert(invariant_octave(intervals));
      end return;
   end Pattern;

   --------------
   -- Generate --
   --------------

   function Generate(origin: PITCH_CLASS; d, g: PITCH_CLASS_INTERVAL) return PITCH_CLASS_SET
   is
      t : PITCH_CLASS_SET;
   begin
      return s : PITCH_CLASS_SET := 16#000# do
         for i in PITCH_CLASS_INTERVAL'First..PITCH_CLASS_INTERVAL'First+d-1 loop
            t := BitSet(origin + i * g);
            exit when (s and t) /= 16#000#; -- cycle detected
            s := s or t;
         end loop;
      end return;
   end Generate;

end Music;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
