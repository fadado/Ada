------------------------------------------------------------------------
--  Junctions implementation
------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

package body Control . Junctions is

   ------------
   -- Joint --
   ------------

   procedure Joint
     (generator : in out GENERATOR_TYPE;
      collector : in out COLLECTOR_TYPE)
   is
   begin
      for x of generator loop
         collector.Resume(x);
      end loop;

      collector.Close;
   end Joint;

   ------------------
   -- Joint_Filter --
   ------------------

   procedure Joint_Filter
     (generator : in out GENERATOR_TYPE;
      collector : in out COLLECTOR_TYPE;
      filter    : access function (x: IO_TYPE) return BOOLEAN)
   is
   begin
      for x of generator loop
         if not filter(x) then
            collector.Resume(x);
         end if;
      end loop;

      collector.Close;
   end Joint_Filter;

   ---------------
   -- Joint_Map --
   ---------------

   procedure Joint_Map
     (generator : in out GENERATOR_TYPE;
      collector : in out COLLECTOR_TYPE;
      map       : access function (x: INPUT_TYPE) return OUTPUT_TYPE)
   is
   begin
      for x of generator loop
         collector.Resume(map(x));
      end loop;

      collector.Close;
   end Joint_Map;

end Control . Junctions;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
