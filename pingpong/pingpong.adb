-- pingpong.adb

pragma Restrictions (No_Select_Statements);

with Ada.Text_IO; use Ada.Text_IO;
with Conveyors; use Conveyors;

procedure pingpong is
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Hello_Task(This: access CONVEYOR);

   task body Hello_Task is
   begin
      This.Suspend;

      Put_Line("Hello, world!");

      This.YieldX;
   exception
      when others => Put_Line(Standard_Error, "Oops at Hello_Task!");
      raise;
   end Hello_Task;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Ping_Task(This, That: access CONVEYOR);
   task type Pong_Task(This, That: access CONVEYOR);

   task body Ping_Task is
   begin
      This.Suspend;

      for i in 1..10 loop
         Put("PING!  ");
         if i < 10 then
            This.Resume(That);
         end if;
      end loop;

      That.Continue;
   exception
      when others => Put_Line(Standard_Error, "Oops at Ping_Task!");
      raise;
   end Ping_Task;

   task body Pong_Task is
   begin
      This.Suspend;

      for i in 1..10 loop
         Put_Line("PONG!");
         if i < 10 then
            This.Resume(That);
         end if;
      end loop;

      This.YieldX;
   exception
      when others => Put_Line(Standard_Error, "Oops at Pong_Task!");
      raise;
   end Pong_Task;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   Conveyer : CONVEYOR;

begin
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   declare
      hello : aliased CONVEYOR;
      hello_thread : Hello_Task (hello'Access);
   begin
      Conveyer.Resume(hello);
      Conveyer.Reset;
   end;

   Put_Line("The players are ready...");
   New_Line;

   declare
      ping : aliased CONVEYOR;
      pong : aliased CONVEYOR;
      ping_thread : Ping_Task (ping'Access, pong'Access);
      pong_thread : Pong_Task (pong'Access, ping'Access);
   begin
      Conveyer.Resume(ping);
   end;

   New_Line;
   Put_Line("Game Over");
end pingpong;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
