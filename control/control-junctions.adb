------------------------------------------------------------------------
--  Junctions implementation
------------------------------------------------------------------------

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

   ---------------------------------------------------------------------
   -- Closure Wrapper
   ---------------------------------------------------------------------

   package body Closure_Wrapper is

      ------------------------------------------------------------------
      --  ITERATOR_TYPE
      ------------------------------------------------------------------

      type ITERATOR_TYPE (
         source : CLOSURE_FUNCTION
      ) is limited new ITERATOR_INTERFACE with null record;

      -----------
      -- First --
      -----------

      overriding function First
        (iterator : in ITERATOR_TYPE) return CURSOR_TYPE
      is (source => iterator.source) with Inline;

      ----------
      -- Next --
      ----------

      overriding function Next
        (iterator : in ITERATOR_TYPE;
         cursor   : in CURSOR_TYPE) return CURSOR_TYPE
      is (cursor) with Inline;

      -------------
      -- Iterate --
      -------------

      function Iterate
        (closure : in out CLOSURE_TYPE) return ITERATOR_INTERFACE'Class
      is (ITERATOR_TYPE'(source => closure.source));

   end Closure_Wrapper;

end Control . Junctions;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
