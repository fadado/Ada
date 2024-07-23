-- pingpong.adb

pragma Restrictions (No_Select_Statements);

with Ada.Text_IO; use Ada.Text_IO;
with Conveyors; use Conveyors;

procedure pingpong is
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Hello_Task_1(This: access CONVEYOR);

   task body Hello_Task_1 is
   begin
      This.Suspend;

      Put_Line("1-Hello, world!");

      This.YieldX;
   exception
      when others => Put_Line(Standard_Error, "Oops at Hello_Task_1!");
      raise;
   end Hello_Task_1;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Hello_Task_2(This: access CONVEYOR);

   task body Hello_Task_2 is
   begin
      This.Suspend;

      Put_Line("2-Hello, world!");
   exception
      when others => Put_Line(Standard_Error, "Oops at Hello_Task_2!");
      raise;
   end Hello_Task_2;

   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------

   task type Hello_Task_3(This: access CONVEYOR);

   task body Hello_Task_3 is
   begin
      This.Suspend;

      Put("3-");
      This.Yield;
      Put("Hello");
      This.Yield;
      Put(", world");
      This.Yield;
      Put_Line("!");
      This.YieldX;
   exception
      when others => Put_Line(Standard_Error, "Oops at Hello_Task_3!");
      raise;
   end Hello_Task_3;

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

      That.Resume;
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

begin
   ---------------------------------------------------------------------
   --
   ---------------------------------------------------------------------
   declare
      Conveyer : CONVEYOR;
      hello : aliased CONVEYOR;
      hello_thread : Hello_Task_1 (hello'Access);
   begin
      Conveyer.Resume(hello);
   end;

   declare
      hello : aliased CONVEYOR;
      hello_thread : Hello_Task_2 (hello'Access);
   begin
      hello.Resume;
   end;

   declare
      Conveyer : CONVEYOR;
      hello : aliased CONVEYOR;
      hello_thread : Hello_Task_3 (hello'Access);
   begin
      Conveyer.Resume(hello);
      Conveyer.Resume(hello);
      Conveyer.Resume(hello);
      Conveyer.Resume(hello);
   end;

   declare
      Conveyer : CONVEYOR;
      ping : aliased CONVEYOR;
      pong : aliased CONVEYOR;
      ping_thread : Ping_Task (ping'Access, pong'Access);
      pong_thread : Pong_Task (pong'Access, ping'Access);
   begin
      New_Line;
      Put_Line("The players are ready...");
      New_Line;
      Conveyer.Resume(ping);
      New_Line;
      Put_Line("Game Over");
   end;

end pingpong;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
