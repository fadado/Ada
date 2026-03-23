------------------------------------------------------------------------------
--  Control . CoRoutines specification
------------------------------------------------------------------------------

package Control . CoRoutines is

   pragma Assertion_Policy (Check); -- Check / Ignore

   type COROUTINE_INTERFACE is limited interface;

   procedure Yield
     (self : in out COROUTINE_INTERFACE)
   is abstract;

   procedure Resume
     (self    : in out COROUTINE_INTERFACE;
      invoker : in out COROUTINE_INTERFACE)
   is abstract;

end Control . CoRoutines;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
