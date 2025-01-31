package body Music is

   function Map(s: PC_SET;
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
   end Map;

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

   function Transposition(i: PC_INTERVAL; s: PC_SET) return PC_SET
   is
      function f(x: PITCH_CLASS) return PITCH_CLASS is (x + i);
   begin
      return Map(s, f'Access);
   end Transposition;

   function Inversion(i: PC_INTERVAL; s: PC_SET) return PC_SET
   is
      function f(x: PITCH_CLASS) return PITCH_CLASS is (i - x);
   begin
      return Map(s, f'Access);
   end Inversion;

end Music;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
