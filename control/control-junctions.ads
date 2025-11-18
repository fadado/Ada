------------------------------------------------------------------------------
--  Junctions specification
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Iterator_Interfaces;

with Control.Generators;
with Control.Collectors;

package Control . Junctions is
   ---------------------------------------------------------------------------
   --  Signatures
   ---------------------------------------------------------------------------

   ---------------------
   -- Joint_Signature --
   ---------------------

   generic
      type IO_TYPE is private;
      type INPUT_CONTEXT  (<>) is limited private;
      type OUTPUT_CONTEXT (<>) is limited private;
   package Joint_Signature is private end;

   -------------------------
   -- Joint_Map_Signature --
   -------------------------

   generic
      type INPUT_TYPE  is private;
      type OUTPUT_TYPE is private;
      type INPUT_CONTEXT  (<>) is limited private;
      type OUTPUT_CONTEXT (<>) is limited private;
   package Joint_Map_Signature is private end;

   --------------------------
   -- Joint_Pipe_Signature --
   --------------------------

   generic
      type INPUT_TYPE  is private;
      type OUTPUT_TYPE is private;
      type HEAD_CONTEXT (<>) is limited private;
      type BODY_CONTEXT (<>) is limited private;
      type TAIL_CONTEXT (<>) is limited private;
   package Joint_Pipe_Signature is private end;

   ---------------------------------------------------------------------------
   --  Junctions
   ---------------------------------------------------------------------------

   --------------
   -- Joint --
   --------------

   generic
      with package Joint_Package is new Joint_Signature (<>);
      use Joint_Package;

      with package Input_Package is new Generators (
         Output_Type  => IO_TYPE,
         Context_Type => INPUT_CONTEXT
      );
      use Input_Package;

      with package Ouput_Package is new Collectors (
         Input_Type   => IO_TYPE,
         Context_Type => OUTPUT_CONTEXT
      );
      use Ouput_Package;

   procedure Joint
     (generator : in out GENERATOR_TYPE;
      collector : in out COLLECTOR_TYPE);

   ------------------
   -- Joint_Filter --
   ------------------

   generic
      with package Joint_Filter_Package is new Joint_Signature (<>);
      use Joint_Filter_Package;

      with package Input_Package is new Generators (
         Output_Type  => IO_TYPE,
         Context_Type => INPUT_CONTEXT
      );
      use Input_Package;

      with package Ouput_Package is new Collectors (
         Input_Type   => IO_TYPE,
         Context_Type => OUTPUT_CONTEXT
      );
      use Ouput_Package;

   procedure Joint_Filter
     (generator : in out GENERATOR_TYPE;
      collector : in out COLLECTOR_TYPE;
      filter    : access function (x: IO_TYPE) return BOOLEAN);

   ---------------
   -- Joint_Map --
   ---------------

   generic
      with package Joint_Map_Package is new Joint_Map_Signature (<>);
      use Joint_Map_Package;

      with package Input_Package is new Generators (
         Output_Type  => INPUT_TYPE,
         Context_Type => INPUT_CONTEXT
      );
      use Input_Package;

      with package Ouput_Package is new Collectors (
         Input_Type   => OUTPUT_TYPE,
         Context_Type => OUTPUT_CONTEXT
      );
      use Ouput_Package;

   procedure Joint_Map
     (generator : in out GENERATOR_TYPE;
      collector : in out COLLECTOR_TYPE;
      map       : access function (x: INPUT_TYPE) return OUTPUT_TYPE);

   ---------------------------------------------------------------------------
   --  Wrappers
   ---------------------------------------------------------------------------

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
      is (TRUE) with Inline;

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

end Control . Junctions;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
