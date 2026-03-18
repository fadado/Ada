------------------------------------------------------------------------------
--  Iterables specification
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Iterator_Interfaces;

package Iterables is
   ---------------------
   -- Closure Wrapper --
   ---------------------

   generic
      type ELEMENT_TYPE is private;

   package Closure_Wrapper is

      type ITERABLE_FUNCTION is
         not null access function return ELEMENT_TYPE;

      type CURSOR_TYPE is null record;

      function Has_Element
        (cursor : in CURSOR_TYPE) return BOOLEAN
      is (cursor=cursor) with Inline;

      package Closure_IIP is  -- Closure Iterator Interfaces Package
         new Ada.Iterator_Interfaces (CURSOR_TYPE, Has_Element);

      subtype ITERATOR_INTERFACE is Closure_IIP.Forward_Iterator;

      type ITERABLE_TYPE (
         flux : ITERABLE_FUNCTION
      ) is limited new ITERATOR_INTERFACE with null record
      with
         Constant_Indexing => Call_Closure,
         Default_Iterator  => Cast_Iterator,
         Iterator_Element  => ELEMENT_TYPE;

      subtype ITERATOR_TYPE is ITERABLE_TYPE;

      overriding function First
        (iterator : in ITERATOR_TYPE) return CURSOR_TYPE
      is ((null record)) with Inline;

      overriding function Next
        (iterator : in ITERATOR_TYPE;
         cursor   : in CURSOR_TYPE) return CURSOR_TYPE
      is (cursor) with Inline;

      function Call_Closure
        (closure : in ITERABLE_TYPE;
         cursor  : in CURSOR_TYPE) return ELEMENT_TYPE
      is (closure.flux.all) with Inline;

      function Cast_Iterator
        (closure : in ITERABLE_TYPE) return ITERATOR_INTERFACE'Class
      is (ITERATOR_TYPE'(closure)) with Inline;

   end Closure_Wrapper;

end Iterables;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
