-- pingpong.adb

with Ada.Text_IO;
with Ada.Synchronous_Task_Control;

procedure pingpong is
   ---------------------------------------------------------------------
   -- Private semaphore; works only for *two* tasks
   ---------------------------------------------------------------------

   subtype SIGNAL is Ada.Synchronous_Task_Control.Suspension_Object;

   -- Operations with `False` initialized semaphores (SIGNALs)
   procedure Wait(S: in out SIGNAL)
      renames Ada.Synchronous_Task_Control.Suspend_Until_True;

   procedure Notify(S: in out SIGNAL)
      renames Ada.Synchronous_Task_Control.Set_True;

   -- Operations with `True` initialized semaphores (mutexes)
   procedure Seize(S: in out SIGNAL)
      renames Ada.Synchronous_Task_Control.Suspend_Until_True;

   procedure Release(S: in out SIGNAL)
      renames Ada.Synchronous_Task_Control.Set_True;

   -- Make a new semaphore initizalized to `True`
   function Make_Mutex return SIGNAL is
      use Ada.Synchronous_Task_Control;
   begin
      return S:SIGNAL do
         -- by default suspension objects are set to `False`
         Set_True(S);
      end return;
   end Make_Mutex;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   Conveyor_Error : exception;

   type CONVEYOR is
      record
         here: aliased SIGNAL; -- defaults to false
         back: access  SIGNAL; -- defaults to null
      end record;

   procedure suspend(self: in out CONVEYOR) is
   begin
      Wait(self.here);
   end suspend;

   procedure call(self: in out CONVEYOR) is
   begin
      Notify(self.here);
   end call;

   procedure resume(self, other: in out CONVEYOR) is
   begin
      if self.back /= null then
         other.back := self.back;
      else
         other.back := self.here'Unchecked_Access;
      end if;
      Notify(other.here);
      Wait(self.here);
   end resume;

   procedure yield(self: in out CONVEYOR) is
   begin
      if self.back = null then
         raise Conveyor_Error;
      end if;
      Notify(self.back.all);
      Wait(self.here);
   end yield;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Ping_Task(This, That: access SIGNAL);
   task type Pong_Task(This, That: access SIGNAL);

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task body Ping_Task is
      use Ada.Text_IO;

      procedure suspend with Inline is
      begin
         Wait(This.all);
      end;

      procedure resume(S: access SIGNAL; W: BOOLEAN := True) with Inline is
      begin
         Notify(S.all);
         if W then
            Wait(This.all);
         end if;
      end;

   begin
      suspend;

      for i in 1..10 loop
         Put("PING!  ");
         resume(That, i /= 10);
      end loop;
   end Ping_Task;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task body Pong_Task is
      use Ada.Text_IO;

      procedure suspend with Inline is
      begin
         Wait(This.all);
      end;

      procedure resume(S: access SIGNAL) with Inline is
      begin
         Notify(S.all);
         Wait(This.all);
      end;

   begin
      suspend;

      for i in 1..10 loop
         Put_Line("PONG!");
         if i /= 10 then
            resume(That);
         end if;
      end loop;
   end Pong_Task;

begin
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   declare
      ping : aliased SIGNAL;
      pong : aliased SIGNAL;
      ping_player : Ping_Task (ping'Access, pong'Access);
      pong_player : Pong_Task (pong'Access, ping'Access);
   begin
      Notify(ping);
   end;
end pingpong;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
