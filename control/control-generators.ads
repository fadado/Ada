------------------------------------------------------------------------------
--  Control . Generators specification (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy(Check); -- Check / Ignore

with Ada.Iterator_Interfaces;

generic
   type OUTPUT_TYPE is private;
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
      not null access procedure (generator: in not null GENERATOR_ACCESS);
   --  Procedure type for the main program

   type GENERATOR_TYPE (
      main    : GENERATOR_FUNCTION;
      context : CONTEXT_ACCESS
   ) is tagged limited private
   with
      Constant_Indexing => Generator_C_I,
      Default_Iterator  => Iterate,
      Iterator_Element  => OUTPUT_TYPE;
   --  Coroutine type with iterable capabilities

   function Resume
     (generator : in out GENERATOR_TYPE) return OUTPUT_TYPE;
   --  Resume `generator` and raises `Stop_Iteration` when dead

   procedure Yield
     (generator : in out GENERATOR_TYPE;
      value     : in OUTPUT_TYPE);
   --  Yields control and a value

   procedure Close
     (generator : in out GENERATOR_TYPE);
   --  Force `generator` to exit

   ---------------------------------------------------------------------------
   --  CURSOR_TYPE methods and constants
   ---------------------------------------------------------------------------

   type CURSOR_TYPE is private;
   --  Cursor based iteration style

   No_Element : constant CURSOR_TYPE;
   --  Represents a cursor that designates no element

   function First
     (generator : in out GENERATOR_TYPE) return CURSOR_TYPE;
   --  If `generator` is empty returns `No_Element`; otherwise returns a
   --  cursor that designates the first element in `generator`

   function Next
     (cursor : CURSOR_TYPE) return CURSOR_TYPE;
   --  If `cursor` equals `No_Element` or the generator is exhaust returns
   --  the value `No_Element`;  otherwise advances `cursor` one element

   function Has_Element
     (cursor : CURSOR_TYPE) return BOOLEAN with Inline;
   --  Returns `TRUE` if `cursor` designates an element, and returns `FALSE`
   --  otherwise

   function Element
     (cursor : CURSOR_TYPE) return OUTPUT_TYPE;
   --  If `cursor` equals `No_Element`, then `Constraint_Error` is
   --  propagated;  otherwise, `Element` returns the element designated by
   --  `cursor`

   procedure For_Each (
      generator : in out GENERATOR_TYPE;
      callback  : not null access procedure (value: in OUTPUT_TYPE));
   --  Invokes `callback.all` with a `value` for each element in `generator`,
   --  consuming `generator`until exhaustion

   ---------------------------------------------------------------------------
   --  Ada 2012 generalized iterator infrastructure
   ---------------------------------------------------------------------------

   package GII is
      new Ada.Iterator_Interfaces (CURSOR_TYPE, Has_Element);

   function Generator_C_I
     (g : in out GENERATOR_TYPE'Class;
      c : CURSOR_TYPE) return OUTPUT_TYPE
   with Inline;
   --  Ignore: used only in the `Constant_Indexing` aspect

   function Iterate
     (generator : in out GENERATOR_TYPE) return GII.Forward_Iterator'Class
   with Inline;
   --  For use in the construct `for cursor in G.Iterate loop...`

private
   ---------------------------------------------------------------------------
   --  Full view for private types
   ---------------------------------------------------------------------------

   task type Generator_Runner (generator: not null GENERATOR_ACCESS);

   type GENERATOR_TYPE (
         main    : GENERATOR_FUNCTION;
         context : CONTEXT_ACCESS
   ) is limited new CONTROLLER_TYPE with 
      record
         master  : CONTROLLER_TYPE;
         runner  : Generator_Runner (GENERATOR_TYPE'Unchecked_Access);
         output  : OUTPUT_TYPE;
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
