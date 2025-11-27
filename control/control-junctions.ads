------------------------------------------------------------------------------
--  Junctions specification
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Iterator_Interfaces;

with Control.Generators;
with Control.Functors;
with Control.Collectors;

package Control . Junctions is
   ---------------------------------------------------------------------------
   --  Signatures
   ---------------------------------------------------------------------------

   ---------------------
   -- Joint_Signature --
   ---------------------

   generic
      type ELEMENT_TYPE is private;
      type SOURCE_CONTEXT (<>) is limited private;
      type TARGET_CONTEXT (<>) is limited private;
   package Joint_Signature is private end;

   -------------------------
   -- Joint_Map_Signature --
   -------------------------

   generic
      type SOURCE_TYPE is private;
      type TARGET_TYPE is private;
      type SOURCE_CONTEXT (<>) is limited private;
      type TARGET_CONTEXT (<>) is limited private;
   package Joint_Map_Signature is private end;

   --------------------------
   -- Joint_Pipe_Signature --
   --------------------------

   generic
      type SOURCE_TYPE is private;
      type TARGET_TYPE is private;
      type SOURCE_CONTEXT (<>) is limited private;
      type MIDDLE_CONTEXT (<>) is limited private;
      type TARGET_CONTEXT (<>) is limited private;
   package Joint_Pipe_Signature is private end;

   ---------------------------------------------------------------------------
   --  Junctions
   ---------------------------------------------------------------------------

   -----------
   -- Joint --
   -----------

   generic
      with package J_Package is new Joint_Signature (<>);
      use J_Package;

      with package G_Package is new Generators (
         Element_Type => ELEMENT_TYPE,
         Context_Type => SOURCE_CONTEXT
      );
      use G_Package;

      with package C_Package is new Collectors (
         Element_Type => ELEMENT_TYPE,
         Context_Type => TARGET_CONTEXT
      );
      use C_Package;

   procedure Joint
     (generator : in out GENERATOR_TYPE;
      collector : in out COLLECTOR_TYPE);

   ------------------
   -- Joint_Filter --
   ------------------

   generic
      with package J_Package is new Joint_Signature (<>);
      use J_Package;

      with package G_Package is new Generators (
         Element_Type => ELEMENT_TYPE,
         Context_Type => SOURCE_CONTEXT
      );
      use G_Package;

      with package C_Package is new Collectors (
         Element_Type => ELEMENT_TYPE,
         Context_Type => TARGET_CONTEXT
      );
      use C_Package;

   procedure Joint_Filter
     (generator : in out GENERATOR_TYPE;
      collector : in out COLLECTOR_TYPE;
      filter    : access function (element: ELEMENT_TYPE) return BOOLEAN);

   ---------------
   -- Joint_Map --
   ---------------

   generic
      with package M_Package is new Joint_Map_Signature (<>);
      use M_Package;

      with package G_Package is new Generators (
         Element_Type => SOURCE_TYPE,
         Context_Type => SOURCE_CONTEXT
      );
      use G_Package;

      with package C_Package is new Collectors (
         Element_Type => TARGET_TYPE,
         Context_Type => TARGET_CONTEXT
      );
      use C_Package;

   procedure Joint_Map
     (generator : in out GENERATOR_TYPE;
      collector : in out COLLECTOR_TYPE;
      map       : access function (element: SOURCE_TYPE) return TARGET_TYPE);

   ----------------
   -- Joint_Pipe --
   ----------------

   generic
      with package P_Package is new Joint_Pipe_Signature (<>);
      use P_Package;

      with package G_Package is new Generators (
         Element_Type => SOURCE_TYPE,
         Context_Type => SOURCE_CONTEXT
      );
      use G_Package;

      with package F_Package is new Functors (
         Input_Type   => SOURCE_TYPE,
         Output_Type  => TARGET_TYPE,
         Context_Type => MIDDLE_CONTEXT
      );
      use F_Package;

      with package C_Package is new Collectors (
         Element_Type => TARGET_TYPE,
         Context_Type => TARGET_CONTEXT
      );
      use C_Package;

   procedure Joint_Pipe
     (generator : in out GENERATOR_TYPE;
      functor   : in out FUNCTOR_TYPE;
      collector : in out COLLECTOR_TYPE);

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
