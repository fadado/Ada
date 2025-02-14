pragma Assertion_Policy(Check); -- Check / Ignore

package body Music is
   ---------------------
   -- pitch-class set --
   ---------------------

   function Cardinality(s: PC_SET) return SET_COUNT is
   begin
      return count : SET_COUNT := 0 do
         if s = FULL then
            count := SET_COUNT'Last;
         elsif s /= VOID then
            for t of BitSet loop
               if (t and s) /= VOID then
                  count := count + 1;
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
         if s /= VOID then
            for x in PITCH_CLASS'Range loop
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

   function Transpositions(s: PC_SET) return SET_COUNT
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

   function invariant_no_dups(s: ORDER) return BOOLEAN
      is (s'Length < 2 or else
            (for all k in s'First..s'Last-1 =>
               (for all h in k+1..s'Last => s(h) /= s(k))))
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

   function Position(i: PC_INTERVAL; s: ORDER) return INDEX is
   begin
      for k in s'Range loop
         if s(k) = i then
            return k;
         end if;
      end loop;
      raise Constraint_Error;
   end Position;

   function Retrograde(s: ORDER) return ORDER
   is
      k : INDEX := INDEX'First;
   begin
      return t : ORDER(s'Range) do
         for x of reverse s loop
            t(k) := x;
            k := k + 1;
         end loop;
      end return;
   end Retrograde;

 --function Rotate(s: ORDER; n: INDEX:=1) return ORDER;

   -----------------
   -- Set <=> Seq --
   -----------------

   function Seq(s: PC_SET) return ORDER
   is
      k : INDEX := INDEX'First;
      h : constant INDEX := INDEX'First + INDEX(Cardinality(s)) - 1;
   begin
      return t : ORDER(k..h) do
         if s /= VOID then
            for x in PITCH_CLASS'Range loop
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
         for x of s loop
            t := BitSet(x) or t;
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
            p(k-1) := Interval(s(k-1), s(k));
         end loop;
         p(s'Last) := Interval(s(s'Last), s(s'First));

         pragma Assert(invariant_octave(p));
      end return;
   end Pattern;

   --------------
   -- Generate --
   --------------

   function Generate(origin: PITCH_CLASS; d, g: PC_INTERVAL) return PC_SET
   is
      t : PC_SET;
   begin
      return s : PC_SET := VOID do
         for i in PC_INTERVAL'First..PC_INTERVAL'First+d-1 loop
            t := BitSet(origin + i * g);
            exit when (s and t) /= VOID; -- cycle detected
            s := s or t;
         end loop;
      end return;
   end Generate;

end Music;
-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
