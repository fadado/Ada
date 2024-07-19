-- pingpong.adb

pragma Restrictions (No_Select_Statements);

with Ada.Dispatching;
with Ada.Text_IO;

with Conveyors; use  Conveyors;

procedure pingpong is
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Hello_Task(This: access CONVEYOR);

   task body Hello_Task is
      use Ada.Text_IO;
   begin
      This.Suspend;

      Put_Line("Hello, world! The players are ready...");
   end Hello_Task;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Ping_Task(This, That: access CONVEYOR);
   task type Pong_Task(This, That: access CONVEYOR);

   task body Ping_Task is
      use Ada.Text_IO;
   begin
      This.Suspend;

      for i in 1..10 loop
         Put("PING!  ");
         if i < 10 then
            This.Resume(That);
         else
            That.Run;
         end if;
      end loop;
   end Ping_Task;

   task body Pong_Task is
      use Ada.Text_IO;
   begin
      This.Suspend;

      for i in 1..10 loop
         Put_Line("PONG!");
         exit when i = 10;
         This.Resume(That);
      end loop;
   end Pong_Task;

begin
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   declare
      ping : aliased CONVEYOR;
      pong : aliased CONVEYOR;
      ping_thread : Ping_Task (ping'Access, pong'Access);
      pong_thread : Pong_Task (pong'Access, ping'Access);

      hello : aliased CONVEYOR;
      hello_thread : Hello_Task (hello'Access);
   begin
      hello.Run;
      while not hello_thread'Terminated loop Ada.Dispatching.Yield; end loop;

      ping.Run;
   end;

end pingpong;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
