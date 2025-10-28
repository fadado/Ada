------------------------------------------------------------------------------
--  Control . CoRoutines . Semi specification (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Control.CoRoutines.Implement;

generic
   type CONTEXT_TYPE (<>) is limited private;
   --  Data to provide an environment for the coroutine procedure

package Control . CoRoutines . Semi is

   package SemiCoroImpl is new Implement
     (Base_Controller => SEMI_CONTROLLER_TYPE,
      Context_Type    => CONTEXT_TYPE);

   subtype COROUTINE_TYPE is SemiCoroImpl.COROUTINE_TYPE;
   subtype CONTEXT_ACCESS is SemiCoroImpl.CONTEXT_ACCESS;

end Control . CoRoutines . Semi;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
