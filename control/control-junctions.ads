------------------------------------------------------------------------------
--  Junctions specification
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Iterator_Interfaces;

with Control.Generators;
with Control.Collectors;

package Control . Junctions is
   ---------------------
   -- Joint_Signature --
   ---------------------

   generic
      type IO_TYPE is private;
      type INPUT_CONTEXT  (<>) is limited private;
      type OUTPUT_CONTEXT (<>) is limited private;
   package Joint_Signature is private end;

   ----------------------
   -- Filter_Signature --
   ----------------------

   generic
      type INPUT_TYPE  is private;
      type OUTPUT_TYPE is private;
      type INPUT_CONTEXT  (<>) is limited private;
      type OUTPUT_CONTEXT (<>) is limited private;
      with function Map(x: in INPUT_TYPE) return OUTPUT_TYPE;
   package Filter_Signature is private end;

   --------------
   -- Junction --
   --------------

   generic
      with package Joint_Package is new Joint_Signature (<>);

      use Joint_Package;

      with package Input_Package is new Generators (
         Output_Type  => IO_TYPE,
         Context_Type => INPUT_CONTEXT
      );

      with package Ouput_Package is new Collectors (
         Input_Type   => IO_TYPE,
         Context_Type => OUTPUT_CONTEXT
      );

      use Input_Package;
      use Ouput_Package;
   procedure Junction
      (generator : in out GENERATOR_TYPE;
       collector : in out COLLECTOR_TYPE);

   ------------
   -- Filter --
   ------------

   generic
      with package Filter_Package is new Filter_Signature (<>);

      use Filter_Package;

      with package Input_Package is new Generators (
         Output_Type  => INPUT_TYPE,
         Context_Type => INPUT_CONTEXT
      );

      with package Ouput_Package is new Collectors (
         Input_Type   => OUTPUT_TYPE,
         Context_Type => OUTPUT_CONTEXT
      );

      use Input_Package;
      use Ouput_Package;
   procedure Filter
      (generator : in out GENERATOR_TYPE;
       collector : in out COLLECTOR_TYPE);

   ---------------------------------------------------------------------
   -- Closure Wrapper
   ---------------------------------------------------------------------

   generic
      type OUTPUT_TYPE is private;

   package Closure_Wrapper is

      type CLOSURE_FUNCTION is
         not null access function return OUTPUT_TYPE;

      type CLOSURE_TYPE (
         source : CLOSURE_FUNCTION
      ) is tagged limited private
      with
         Constant_Indexing => Element_Value,
         Default_Iterator  => Iterate,
         Iterator_Element  => OUTPUT_TYPE;

      type CURSOR_TYPE is private;

      function Has_Element
        (cursor : in CURSOR_TYPE) return BOOLEAN
      is (TRUE) with Inline;

      function Element_Value
        (closure : in out CLOSURE_TYPE;
         cursor  : in CURSOR_TYPE) return OUTPUT_TYPE
      with Inline;

      package Closure_IIP is  -- Closure Iterator Interfaces Package
         new Ada.Iterator_Interfaces (CURSOR_TYPE, Has_Element);

      subtype ITERATOR_INTERFACE is Closure_IIP.Forward_Iterator;

      function Iterate
        (closure : in out CLOSURE_TYPE) return ITERATOR_INTERFACE'Class
      with Inline;

   private

      type CLOSURE_TYPE (
            source : CLOSURE_FUNCTION
      ) is tagged limited null record;

      type CURSOR_TYPE is
         record
            source : CLOSURE_FUNCTION;
         end record;

   end Closure_Wrapper;

end Control . Junctions;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
