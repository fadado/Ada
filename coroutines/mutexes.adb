-- mutexes.adb

package body Mutexes is

   -- Make a new semaphore initizalized to `True`
   function Make_Mutex return MUTEX is
      use Ada.Synchronous_Task_Control;
   begin
      return S:MUTEX do
         -- suspension objects are by default set to `False`
         Set_True(S);
      end return;
   end Make_Mutex;

end Mutexes;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
