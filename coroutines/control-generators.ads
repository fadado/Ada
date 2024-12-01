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
      Default_Iterator  => Generator_D_I,
      Constant_Indexing => Generator_C_I,
      Iterator_Element  => ELEMENT_TYPE;
   --  Coroutine type with iterator capabilities

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
   --  ITERATOR_TYPE (Ada 2012 generalized iterator infrastructure)
   ---------------------------------------------------------------------------

   package GII is new Ada.Iterator_Interfaces (CURSOR_TYPE, Has_Element);

   type ITERATOR_TYPE is
      limited new GII.Forward_Iterator with private
   with
      Default_Iterator  => Iterator_D_I,
      Constant_Indexing => Iterator_C_I,
      Iterator_Element  => ELEMENT_TYPE;

   overriding function First(iterator: ITERATOR_TYPE)
      return CURSOR_TYPE with Inline;

   overriding function Next(iterator: ITERATOR_TYPE; cursor: CURSOR_TYPE)
      return CURSOR_TYPE with Inline;

   function Iterate(generator: in out GENERATOR_TYPE'Class)
      return ITERATOR_TYPE with Inline;

   --  Used in aspects declarations

   function Generator_C_I(g: in out GENERATOR_TYPE'Class; c: CURSOR_TYPE)
      return ELEMENT_TYPE with Inline;

   function Iterator_C_I(i: in out ITERATOR_TYPE'Class; c: CURSOR_TYPE)
      return ELEMENT_TYPE with Inline;

   function Generator_D_I(g: in out GENERATOR_TYPE'Class)
      return GII.Forward_Iterator'Class with Inline;

   function Iterator_D_I(i: in out ITERATOR_TYPE'Class)
      return GII.Forward_Iterator'Class with Inline;

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

   type ITERATOR_TYPE is 
      limited new GII.Forward_Iterator with
      record
         source : not null GENERATOR_ACCESS;
      end record;

end Control . Generators;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
