------------------------------------------------------------------------------
--  Control . CoRoutines specification
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

package Control . CoRoutines is

   type COROUTINE_INTERFACE is limited interface;

   procedure Yield
     (routine  : in out COROUTINE_INTERFACE) is abstract;

   procedure Resume
     (routine  : in out COROUTINE_INTERFACE;
      invoker  : in out COROUTINE_INTERFACE) is abstract;
   -- To restrict the coroutine procedure to call only this API

 --procedure Close
 --  (routine  : in out COROUTINE_INTERFACE) is abstract;

end Control . CoRoutines;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
