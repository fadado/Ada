-- mutexes.ads

with Ada.Synchronous_Task_Control;

package Mutexes is

   -- Private semaphore like; works only for *two* tasks
   subtype MUTEX is Ada.Synchronous_Task_Control.SUSPENSION_OBJECT;

   -- Operations for `True` initialized semaphores (mutexes)
   procedure Seize(S: in out MUTEX)
      renames Ada.Synchronous_Task_Control.Suspend_Until_True;

   procedure Release(S: in out MUTEX)
      renames Ada.Synchronous_Task_Control.Set_True;

   function Make_Mutex return MUTEX;

end Mutexes;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
