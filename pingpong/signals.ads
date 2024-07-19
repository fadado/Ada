-- signals.ads

with Ada.Synchronous_Task_Control;

package Signals is

   -- Private semaphore like; works only for *two* tasks
   subtype SIGNAL is Ada.Synchronous_Task_Control.Suspension_Object;

   -- Operations for `False` initialized semaphores (signals)
   procedure Wait(S: in out SIGNAL)
      renames Ada.Synchronous_Task_Control.Suspend_Until_True;

   procedure Notify(S: in out SIGNAL)
      renames Ada.Synchronous_Task_Control.Set_True;

   function Busy(S: in SIGNAL) return BOOLEAN
      renames Ada.Synchronous_Task_Control.Current_State;

end Signals;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
