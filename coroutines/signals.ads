------------------------------------------------------------------------------
--  A "renaming" layer on top of `Ada.Synchronous_Task_Control`
------------------------------------------------------------------------------

with Ada.Synchronous_Task_Control;

package Signals is

   subtype SIGNAL is Ada.Synchronous_Task_Control.SUSPENSION_OBJECT;
   --  `FALSE` initialized semaphores for *two* tasks only

   procedure Wait(S: in out SIGNAL)
      renames Ada.Synchronous_Task_Control.Suspend_Until_True;

   procedure Notify(S: in out SIGNAL)
      renames Ada.Synchronous_Task_Control.Set_True;

   procedure Clear(S: in out SIGNAL)
      renames Ada.Synchronous_Task_Control.Set_False;

   function Is_Set(S: in SIGNAL) return BOOLEAN
      renames Ada.Synchronous_Task_Control.Current_State;

   function Is_Cleared(S: in SIGNAL) return BOOLEAN
      is (not Is_Set(S)) with Inline;

end Signals;

-- �ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
