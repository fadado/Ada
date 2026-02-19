------------------------------------------------------------------------------
--  Control . CoRoutines . Semi specification (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Control.CoRoutines.Implement;

generic
   type CONTEXT_TYPE (<>) is limited private;
   --  Data to provide an environment for the coroutine procedure

package Control . CoRoutines . Semi is

   package SemiCoroImplement is new Implement
     (Base_Controller => SEMI_CONTROLLER_TYPE,
      Context_Type    => CONTEXT_TYPE);

   subtype COROUTINE_TYPE is SemiCoroImplement.COROUTINE_TYPE;

end Control . CoRoutines . Semi;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
