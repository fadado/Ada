------------------------------------------------------------------------------
--  Junctions implementation
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

package body Control . Junctions is

   --------------
   -- Junction --
   --------------

   procedure Junction
      (generator : in out GENERATOR_TYPE;
       collector : in out COLLECTOR_TYPE)
   is
   begin
      for x of generator loop
         collector.Resume(x);
      end loop;

      collector.Close;
   end Junction;

   ------------
   -- Filter --
   ------------

   procedure Filter
      (generator : in out GENERATOR_TYPE;
       collector : in out COLLECTOR_TYPE)
   is
   begin
      for x of generator loop
         collector.Resume(Map(x));
      end loop;

      collector.Close;
   end Filter;

end Control . Junctions;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
