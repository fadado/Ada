------------------------------------------------------------------------------
--  Control . Generators specification (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Iterator_Interfaces;

generic
   type ELEMENT_TYPE is private;
   --  Type for `Yield` generated values

   type CONTEXT_TYPE (<>) is limited private;
   --  Data to provide an environment for the generator procedure

package Control . Generators is
   ---------------------------------------------------------------------------
   --  GENERATOR_TYPE methods and auxiliar types
   ---------------------------------------------------------------------------

   type GENERATOR_INTERFACE is limited interface;

   procedure Yield
     (generator : in out GENERATOR_INTERFACE;
      output    : in ELEMENT_TYPE)
   is abstract;
   -- To restrict the generator procedure to call only `Yield`

   type GENERATOR_PROCEDURE is not null access procedure
      (generator : in out GENERATOR_INTERFACE'Class;
       context   : access CONTEXT_TYPE);
   --  Procedure type for the generator procedure

   type GENERATOR_TYPE (
      main    : GENERATOR_PROCEDURE;
      context : access CONTEXT_TYPE
   ) is limited new GENERATOR_INTERFACE with private
   with
      Constant_Indexing => Element_Value,
      Default_Iterator  => Iterate,
      Iterator_Element  => ELEMENT_TYPE;
   --  Coroutine type with iterable capabilities

   function Resume
     (generator : in out GENERATOR_TYPE) return ELEMENT_TYPE
   with Inline;
   --  Resume `generator` and raises `Stop_Iteration` when dead

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
     (cursor : in CURSOR_TYPE) return CURSOR_TYPE;
   --  If `cursor` equals `No_Element` or the generator is exhaust returns
   --  the value `No_Element`;  otherwise advances `cursor` one element

   function Has_Element
     (cursor : in CURSOR_TYPE) return BOOLEAN with Inline;
   --  Returns `TRUE` if `cursor` designates an element, and returns `FALSE`
   --  otherwise

   function Element
     (cursor : in CURSOR_TYPE) return ELEMENT_TYPE;
   --  If `cursor` equals `No_Element`, then `Control_Error` is
   --  propagated;  otherwise, `Element` returns the element designated by
   --  `cursor`

   procedure For_Each (
      generator : in out GENERATOR_TYPE;
      callback  : not null access procedure (value: in ELEMENT_TYPE));
   --  Call `callback.all` with a `value` for each element in `generator`,
   --  consuming `generator` until exhaustion

   function Element_Value
     (generator : in out GENERATOR_TYPE;
      cursor    : in CURSOR_TYPE) return ELEMENT_TYPE
   with Inline;
   --  Used only in the `Constant_Indexing` aspect

   ---------------------------------------------------------------------------
   --  ITERATOR_TYPE methods and constants
   --  Ada 2012 generalized iterator infrastructure
   ---------------------------------------------------------------------------

   package Generator_IIP is  -- Generator Iterator Interfaces Package
      new Ada.Iterator_Interfaces (CURSOR_TYPE, Has_Element);

   subtype ITERATOR_INTERFACE is Generator_IIP.Forward_Iterator;

   function Iterate
     (generator : in out GENERATOR_TYPE) return ITERATOR_INTERFACE'Class
   with Inline;
   --  For use in the construct `for cursor in G.Iterate loop...`

private

   overriding procedure Yield
     (generator : in out GENERATOR_TYPE;
      value     : in ELEMENT_TYPE)
   with Inline;
   --  Yields control and a value

   ---------------------------------------------------------------------------
   --  Full view for private types
   ---------------------------------------------------------------------------

   task type Generator_Runner (generator: not null access GENERATOR_TYPE);

   type GENERATOR_TYPE (
         main       : GENERATOR_PROCEDURE;
         context    : access CONTEXT_TYPE
   ) is limited new SEMI_CONTROLLER_TYPE and GENERATOR_INTERFACE with 
      record
         dispatcher : DISPATCHER_TYPE;
         runner     : Generator_Runner (GENERATOR_TYPE'Access);
         output     : ELEMENT_TYPE;
      end record;

   type CURSOR_TYPE is
      record
         source : access GENERATOR_TYPE;
      end record;

   No_Element : constant CURSOR_TYPE := (source => NULL);

end Control . Generators;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
