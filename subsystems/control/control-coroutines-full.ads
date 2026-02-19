------------------------------------------------------------------------------
--  Control . CoRoutines . Full specification (generic)
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Control.CoRoutines.Implement;

generic
   type CONTEXT_TYPE (<>) is limited private;
   --  Data to provide an environment for the coroutine procedure

package Control . CoRoutines . Full is

   package FullCoroImplement is new Implement
     (Base_Controller => FULL_CONTROLLER_TYPE,
      Context_Type    => CONTEXT_TYPE);

   subtype COROUTINE_TYPE is FullCoroImplement.COROUTINE_TYPE;

end Control . CoRoutines . Full;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
