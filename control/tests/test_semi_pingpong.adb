------------------------------------------------------------------------------
--  Symmetric ping/pong
------------------------------------------------------------------------------

pragma Assertion_Policy (Check); -- Check / Ignore

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Control; use Control;

with Gotcha;

procedure test_semi_pingpong
is
   Game_Over : BOOLEAN := FALSE;
   Turns : constant := 10;

   task type Ping_Run(This, That: not null SEMI_CONTROLLER_ACCESS);
   task type Pong_Run(This, That: not null SEMI_CONTROLLER_ACCESS);

   --------------
   -- Ping_Run --
   --------------

   task body Ping_Run
   is
      Ping : SEMI_CONTROLLER_TYPE renames This.all;
      Pong : SEMI_CONTROLLER_TYPE renames That.all;

      procedure strike is begin Put("PING!  "); end;
   begin
      Ping.Commence;

      while not Game_Over loop
         strike;
         Pong.Resume(Ping); -- resume Pong from Ping
      end loop;

      Ping.Quit;
   exception
      when X: others => Ping.Quit(X); raise;
   end Ping_Run;

   --------------
   -- Pong_Run --
   --------------

   task body Pong_Run
   is
      Ping : SEMI_CONTROLLER_TYPE renames This.all; -- not used
      Pong : SEMI_CONTROLLER_TYPE renames That.all;

      procedure strike is begin Put_Line("PONG!"); end;
   begin
      Pong.Commence;

      for i in 1..Turns-1 loop
         strike;
         Pong.Yield; -- yield to Ping
      end loop;
      strike;

      Game_Over := TRUE;

      Pong.Quit; -- yield to Ping
   exception
      when X: others => Pong.Quit(X); raise;
   end Pong_Run;

begin
   -- MAIN

   Gotcha.Set_Handlers;

   declare
      dispatcher : DISPATCHER_TYPE;

      ping_control : aliased SEMI_CONTROLLER_TYPE;
      pong_control : aliased SEMI_CONTROLLER_TYPE;
      ping_runner  : Ping_Run (ping_control'Unchecked_Access,
                               pong_control'Unchecked_Access);
      pong_runner  : Pong_Run (ping_control'Unchecked_Access,
                               pong_control'Unchecked_Access);
   begin
      Put_Line("The players are ready...");

      dispatcher.Dispatch(ping_control);

      --TODO: bug: wait end of players
      while not ping_runner'Terminated loop null; end loop;
      while not pong_runner'Terminated loop null; end loop;

      Put_Line("Game Over");
      New_Line;
   exception
      when X: others =>
         Gotcha.Report_Exception(X, "Oops at MASTER TASK!!");
   end;

end test_semi_pingpong;

-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=UTF8:syntax=ada
