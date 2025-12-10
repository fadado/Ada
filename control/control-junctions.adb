------------------------------------------------------------------------
--  Junctions implementation
------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

package body Control . Junctions is

   ----------------
   -- Joint_Pass --
   ----------------

   procedure Joint_Pass
     (generator : in out GENERATOR_TYPE;
      collector : in out COLLECTOR_TYPE)
   is
   begin
      for element of generator loop
         collector.Resume(element);
      end loop;

      collector.Close;
   end Joint_Pass;

   ------------------
   -- Joint_Filter --
   ------------------

   procedure Joint_Filter
     (generator : in out GENERATOR_TYPE;
      collector : in out COLLECTOR_TYPE;
      filter    : access function (element: ELEMENT_TYPE) return BOOLEAN)
   is
   begin
      for element of generator loop
         if not filter(element) then
            collector.Resume(element);
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
      map       : access function (element: SOURCE_TYPE) return TARGET_TYPE)
   is
   begin
      for element of generator loop
         collector.Resume(map(element));
      end loop;

      collector.Close;
   end Joint_Map;

   ----------------
   -- Joint_Tube --
   ----------------

   procedure Joint_Tube
     (generator : in out GENERATOR_TYPE;
      functor   : in out FUNCTOR_TYPE;
      collector : in out COLLECTOR_TYPE)
   is
   begin
      for element of generator loop
         collector.Resume(functor.Resume(element));
      end loop;

      functor.Close;
      collector.Close;
   end Joint_Tube;

end Control . Junctions;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
