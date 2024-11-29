------------------------------------------------------------------------------
--  Control . Generators specification (generic)
------------------------------------------------------------------------------

with Ada.Iterator_Interfaces;

generic
   type ELEMENT_TYPE is private;
   --  Type for `Yield` generated values

   type CONTEXT_TYPE is private;
   --  Data to provide an environment for the program

package Control . Generators is
   ---------------------------------------------------------------------------
   --  GENERATOR_TYPE coroutine methods and auxiliar types
   ---------------------------------------------------------------------------

   type CONTEXT_ACCESS is access all CONTEXT_TYPE;

   type GENERATOR_TYPE;
   type GENERATOR_ACCESS is access all GENERATOR_TYPE;

   type GENERATOR_FUNCTION is
      not null access procedure (generator: not null GENERATOR_ACCESS);
   --  Procedure type for the main program

   type GENERATOR_TYPE (main: GENERATOR_FUNCTION; context: CONTEXT_ACCESS) is
      tagged limited private
   with
      Constant_Indexing => Element_C_I,
      Default_Iterator  => Iterate,
      Iterator_Element  => ELEMENT_TYPE;
   --  Coroutine type with iterable capabilities

   function Resume(generator: in out GENERATOR_TYPE) return ELEMENT_TYPE;
   --  Resume `generator` and raises `Stop_Iteration` when dead

   procedure Yield(generator: in out GENERATOR_TYPE; value: ELEMENT_TYPE);
   --  Yields control and a value

   procedure Close(generator: in out GENERATOR_TYPE);
   --  Force `generator` to exit

   ---------------------------------------------------------------------------
   --  CURSOR_TYPE methods and constants
   ---------------------------------------------------------------------------

   type CURSOR_TYPE is private;
   --  Cursor based iteration style

   No_Element : constant CURSOR_TYPE;
   --  `No_Element` represents a cursor that designates no element.

   function First(generator: in out GENERATOR_TYPE) return CURSOR_TYPE;
   --  If `generator` is empty, `First` returns `No_Element`. Otherwise, it
   --  returns a cursor that designates the first element in `generator`.

   function Next(cursor: CURSOR_TYPE) return CURSOR_TYPE;
   --  If `cursor` equals `No_Element` or the generator is exhaust returns
   --  the value `No_Element`.  Otherwise advances `cursor` one element.

   function Has_Element(cursor: CURSOR_TYPE) return BOOLEAN with Inline;
   --  Returns `TRUE` if `cursor` designates an element, and returns `FALSE`
   --  otherwise.

   function Element(cursor: CURSOR_TYPE) return ELEMENT_TYPE;
   --  If `cursor` equals `No_Element`, then `Constraint_Error` is
   --  propagated.  Otherwise, `Element` returns the element designated by
   --  `cursor`.

   procedure For_Each (
      generator : in out GENERATOR_TYPE;
      callback  : not null access procedure(value: ELEMENT_TYPE));
   --  Invokes `callback.all` with value for each element in `generator`.
   --  Consumes `generator`until exhaustion.  Any exception raised by
   --  `callback` is propagated.

   ---------------------------------------------------------------------------
   --  Ada 2012 generalized iterator infrastructure
   ---------------------------------------------------------------------------

   package GII is
      new Ada.Iterator_Interfaces (CURSOR_TYPE, Has_Element);

   function Iterate(generator: in out GENERATOR_TYPE)
      return GII.Forward_Iterator'Class with Inline;
   --  For use in the construct `for cursor in G.Iterate loop...`

   function Element_C_I(g: in out GENERATOR_TYPE'Class; c: CURSOR_TYPE)
      return ELEMENT_TYPE with Inline;
   --  Used only in the `Constant_Indexing` aspect.

private
   ---------------------------------------------------------------------------
   --  Full view for private types
   ---------------------------------------------------------------------------

   task type Run_Method (generator: not null GENERATOR_ACCESS)
      is private end;

   type GENERATOR_TYPE (main: GENERATOR_FUNCTION; context: CONTEXT_ACCESS) is
      limited new ASYMMETRIC_CONTROLLER with 
      record
         head   : ASYMMETRIC_CONTROLLER;
         runner : Run_Method (GENERATOR_TYPE'Unchecked_Access);
         value  : ELEMENT_TYPE;
      end record;

   type CURSOR_TYPE is
      record
         source : GENERATOR_ACCESS;
      end record;

   No_Element : constant CURSOR_TYPE := (source => NULL);

end Control . Generators;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
