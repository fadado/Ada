-- pingpong.adb

with Ada.Text_IO;
with Ada.Synchronous_Task_Control;

procedure pingpong is
   ---------------------------------------------------------------------
   -- Private semaphore; works only for *two* tasks
   ---------------------------------------------------------------------

   subtype Semaphore is Ada.Synchronous_Task_Control.Suspension_Object;

   -- Operations with `False` initialized semaphores (signals)
   procedure Wait(S: in out Semaphore)
      renames Ada.Synchronous_Task_Control.Suspend_Until_True;

   procedure Signal(S: in out Semaphore)
      renames Ada.Synchronous_Task_Control.Set_True;

   -- Operations with `True` initialized semaphores (mutexes)
   procedure Seize(S: in out Semaphore)
      renames Ada.Synchronous_Task_Control.Suspend_Until_True;

   procedure Release(S: in out Semaphore)
      renames Ada.Synchronous_Task_Control.Set_True;

   -- Make a new semaphore initizalized to `True`
   function Make_Mutex return Semaphore is
      use Ada.Synchronous_Task_Control;
   begin
      return S:Semaphore do
         -- by default suspension objects are set to `False`
         Set_True(S);
      end return;
   end Make_Mutex;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Ping_Routine(This, That: access Semaphore);
   task type Pong_Routine(This, That: access Semaphore);

   task body Ping_Routine is
      use Ada.Text_IO;
   begin
      Wait (This.all);

      for i in 1..10 loop
         Put("PING!  ");

         Signal (That.all);
         if i /= 10 then
            Wait (This.all);
         end if;
      end loop;
   end Ping_Routine;

   task body Pong_Routine is
      use Ada.Text_IO;
   begin
      Wait (This.all);

      for i in 1..10 loop
         Put_Line("PONG!");

         if i /= 10 then
            Signal (That.all);
            Wait   (This.all);
         end if;
      end loop;
   end Pong_Routine;

begin
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   declare
      ping_flag   : aliased Semaphore;
      pong_flag   : aliased Semaphore;

      ping_player : Ping_Routine (ping_flag'Access, pong_flag'Access);
      pong_player : Pong_Routine (pong_flag'Access, ping_flag'Access);
   begin
      Signal (ping_flag);
   end;
end pingpong;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
