------------------------------------------------------------------------------
--  Control . Functors specification (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

generic
   type INPUT_TYPE  is private;
   type OUTPUT_TYPE is private;
   --  Type for `Yield` generated values

   type CONTEXT_TYPE (<>) is limited private;
   --  Data to provide an environment for the functor procedure

package Control . Functors is
   ---------------------------------------------------------------------------
   --  FUNCTOR_TYPE methods and auxiliar types
   ---------------------------------------------------------------------------

   type FUNCTOR_INTERFACE is limited interface;

   procedure Yield
     (functor : in out FUNCTOR_INTERFACE;
      map     : not null access function (x: INPUT_TYPE) return OUTPUT_TYPE)
   is abstract;
   -- To restrict the functor procedure to call only `Yield`

   type FUNCTOR_PROCEDURE is not null access procedure
      (functor : in out FUNCTOR_INTERFACE'Class;
       context : access CONTEXT_TYPE);
   --  Procedure type for the functor procedure

   type FUNCTOR_TYPE (
      main    : FUNCTOR_PROCEDURE;
      context : access CONTEXT_TYPE
   ) is limited new FUNCTOR_INTERFACE with private;
   --  Coroutine type with functor capabilities

   function Resume
     (functor : in out FUNCTOR_TYPE;
      input   : in INPUT_TYPE) return OUTPUT_TYPE;
   --  Resume `functor` with a new `input` value

   procedure Close
     (functor : in out FUNCTOR_TYPE);
   --  Force `functor` to exit

private

   procedure Yield
     (functor : in out FUNCTOR_TYPE;
      map     : not null access function (x: INPUT_TYPE) return OUTPUT_TYPE);
   --  Yields control after returning a value

   ---------------------------------------------------------------------------
   --  Full view for private types
   ---------------------------------------------------------------------------

   task type Functor_Runner (functor: not null access FUNCTOR_TYPE);

   type FUNCTOR_TYPE (
         main       : FUNCTOR_PROCEDURE;
         context    : access CONTEXT_TYPE
   ) is limited new SEMI_CONTROLLER_TYPE and FUNCTOR_INTERFACE with 
      record
         dispatcher : DISPATCHER_TYPE;
         runner     : Functor_Runner (FUNCTOR_TYPE'Access);
         input      : INPUT_TYPE;
         output     : OUTPUT_TYPE;
         inaugural  : BOOLEAN := TRUE;
      end record;

end Control . Functors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
