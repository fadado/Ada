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

      type ITERATOR_TYPE is limited new ITERATOR_INTERFACE with 
         record
            source : CLOSURE_FUNCTION;
         end record;

      overriding function First
        (iterator : in ITERATOR_TYPE) return CURSOR_TYPE
      with Inline;

      overriding function Next
        (iterator : in ITERATOR_TYPE;
         cursor   : in CURSOR_TYPE) return CURSOR_TYPE
      with Inline;

      -----------
      -- First --
      -----------

      function First
        (iterator : in ITERATOR_TYPE) return CURSOR_TYPE
      is
      begin
         return (source => iterator.source);
      end First;

      ----------
      -- Next --
      ----------

      function Next
        (iterator : in ITERATOR_TYPE;
         cursor   : in CURSOR_TYPE) return CURSOR_TYPE
      is
      begin
         pragma Assert (iterator.source = cursor.source);
         return cursor;
      end Next;

      -------------------
      -- Element_Value --
      -------------------

      function Element_Value
        (closure : in out CLOSURE_TYPE;
         cursor  : in CURSOR_TYPE) return OUTPUT_TYPE
      is
      begin
         pragma Assert (closure.source = cursor.source);
         return closure.source.all;
      end Element_Value;

      -------------
      -- Iterate --
      -------------

      function Iterate
        (closure : in out CLOSURE_TYPE) return ITERATOR_INTERFACE'Class
      is
      begin
         return ITERATOR_TYPE'(source => closure.source);
      end Iterate;

   end Closure_Wrapper;

end Control . Junctions;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
