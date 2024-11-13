------------------------------------------------------------------------------
--  Generic Control . Generators (specification)
------------------------------------------------------------------------------

with Ada.Iterator_Interfaces;

generic
   type CONTEXT_TYPE is private;
   --  Data to provide an environment for the program

   type ELEMENT_TYPE is private;
   --  Type for `Yield` generated values

package Control . Generators is
   ---------------------------------------------------------------------------
   --  GENERATOR_TYPE coroutine methods and auxiliar types
   ---------------------------------------------------------------------------

   type CONTEXT_ACCESS is access all CONTEXT_TYPE;
   --  Optional data for the program

   type GENERATOR_TYPE;
   type GENERATOR_ACCESS is access all GENERATOR_TYPE;
   --  Forward declarations

   type PROGRAM_ACCESS is not null access procedure (self: GENERATOR_ACCESS);
   --  Procedure type for the main program

   type GENERATOR_TYPE (main: PROGRAM_ACCESS; context: CONTEXT_ACCESS) is
      tagged limited private;
   --  Coroutine type with only transfer of control

   procedure Resume(self: in out GENERATOR_TYPE; value: out ELEMENT_TYPE);
   --  Resume `self` and raises `Stop_Iterator` when dead

   procedure Yield(self: in out GENERATOR_TYPE; value: ELEMENT_TYPE);
   --  Yields control only

   procedure Close(self: in out GENERATOR_TYPE);
   --  Force `self` to exit

   ---------------------------------------------------------------------------
   --  CURSOR_TYPE methods and constants
   ---------------------------------------------------------------------------

   type CURSOR_TYPE is private;
   --  Ada 2005 style iteration

   No_Element : constant CURSOR_TYPE;
   --  `No_Element` represents a cursor that designates no element.

   function Has_Element(cursor: CURSOR_TYPE) return BOOLEAN is
      (cursor /= No_Element);
   --  Returns `TRUE` if `cursor` designates an element, and returns `FALSE`
   --  otherwise.

   function Next(cursor: CURSOR_TYPE) return CURSOR_TYPE;
   --  If `cursor` equals `No_Element` or the generator is exhaust returns
   --  the value `No_Element`.  Otherwise advances `cursor` one element.

   procedure Next(cursor: in out CURSOR_TYPE);
   --  Equivalent to cursor := Next(cursor).

   function Element(cursor: CURSOR_TYPE) return ELEMENT_TYPE;
   --  If `cursor` equals `No_Element`, then `Constraint_Error` is
   --  propagated.  Otherwise, `Element` returns the element designated by
   --  `cursor`.

   ---------------------------------------------------------------------------
   --  GENERATOR_TYPE as Ada 2005 iterator methods
   ---------------------------------------------------------------------------

   function First(generator: in out GENERATOR_TYPE) return CURSOR_TYPE;
   --  If `generator` is empty, `First` returns `No_Element`. Otherwise, it
   --  returns a cursor that designates the first element in `generator`.

   procedure Iterate (
      generator : in out GENERATOR_TYPE;
      process   : not null access procedure(value: ELEMENT_TYPE));
   --  Invokes `process.all` with value for each element in `generator`.
   --  Consumes `generator`until exhaustion.  Any exception raised by `process`
   --  is propagated.

   ---------------------------------------------------------------------------
   --  GENERATOR_TYPE as Ada 2012 iterator methods
   ---------------------------------------------------------------------------

   package IIGenerator is
      new Ada.Iterator_Interfaces (CURSOR_TYPE, Has_Element);

   subtype ITERATOR is IIGenerator.Forward_Iterator;
   subtype ITERABLE is IIGenerator.Forward_Iterator'Class;

 --function Iterate(generator: GENERATOR_TYPE) return ITERABLE;

 --function First
 --  (Object : Forward_Iterator) return Cursor is abstract;
 --function Next
 --  (Object   : Forward_Iterator;
 --   Position : Cursor) return Cursor is abstract;

   ---------------------------------------------------------------------------
   --  Wrapper for a program with optional context
   ---------------------------------------------------------------------------

   generic
      Main    : PROGRAM_ACCESS;
      Context : CONTEXT_ACCESS := NULL;
   package Wrap is
      procedure Call(value: out ELEMENT_TYPE);
      --  Resume `main`; propagate exceptions after cleanup
   end Wrap;

private
   task type Run_Method (self: not null GENERATOR_ACCESS);

   type GENERATOR_TYPE (main: PROGRAM_ACCESS; context: CONTEXT_ACCESS) is
      limited new ASYMMETRIC_CONTROLLER
            --and IIGenerator.Forward_Iterator
      with record
         head   : ASYMMETRIC_CONTROLLER;
         runner : Run_Method (GENERATOR_TYPE'Unchecked_Access);
         value  : ELEMENT_TYPE;
      end record;

   type CURSOR_TYPE is access all GENERATOR_TYPE;

   No_Element : constant CURSOR_TYPE := NULL;

end Control . Generators;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- im:fileformat=dos:fileencoding=latin1:syntax=ada
