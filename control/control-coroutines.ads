------------------------------------------------------------------------------
--  Control . CoRoutines specification
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

package Control . CoRoutines is

   type COROUTINE_INTERFACE is limited interface;

   procedure Yield
     (self : in out COROUTINE_INTERFACE)
   is abstract;

   procedure Resume
     (self    : in out COROUTINE_INTERFACE;
      invoker : in out COROUTINE_INTERFACE)
   is abstract;

end Control . CoRoutines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
