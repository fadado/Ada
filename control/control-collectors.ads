------------------------------------------------------------------------------
--  Control . Collectors specification (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

generic
   type ELEMENT_TYPE is private;
   --  Type for `Yield` generated values

   type CONTEXT_TYPE (<>) is limited private;
   --  Data to provide an environment for the collector procedure

package Control . Collectors is
   ---------------------------------------------------------------------------
   --  COLLECTOR_TYPE methods and auxiliar types
   ---------------------------------------------------------------------------

   type COLLECTOR_INTERFACE is limited interface;

   function Yield
     (collector : in out COLLECTOR_INTERFACE) return ELEMENT_TYPE
   is abstract;
   -- To restrict the collector procedure to call only `Yield`

   type COLLECTOR_PROCEDURE is not null access procedure
      (collector : in out COLLECTOR_INTERFACE'Class;
       context   : access CONTEXT_TYPE);
   --  Procedure type for the collector procedure

   type COLLECTOR_TYPE (
      main    : COLLECTOR_PROCEDURE;
      context : access CONTEXT_TYPE
   ) is limited new COLLECTOR_INTERFACE with private;
   --  Coroutine type with collector capabilities

   procedure Resume
     (collector : in out COLLECTOR_TYPE;
      input     : in ELEMENT_TYPE);
   --  Resume `collector` with a new `input` value

   procedure Close
     (collector : in out COLLECTOR_TYPE);
   --  Force `collector` to exit

private
   overriding function Yield
     (collector : in out COLLECTOR_TYPE) return ELEMENT_TYPE;
   --  Yields control after returning a value

   ---------------------------------------------------------------------------
   --  Full view for private types
   ---------------------------------------------------------------------------

   task type Collector_Runner (collector: not null access COLLECTOR_TYPE);

   type COLLECTOR_TYPE (
         main       : COLLECTOR_PROCEDURE;
         context    : access CONTEXT_TYPE
   ) is limited new SEMI_CONTROLLER_TYPE and COLLECTOR_INTERFACE with 
      record
         dispatcher : DISPATCHER_TYPE;
         runner     : Collector_Runner (COLLECTOR_TYPE'Access);
         input      : ELEMENT_TYPE;
         inaugural  : BOOLEAN := TRUE;
      end record;

end Control . Collectors;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
