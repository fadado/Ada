-- sieve.adb

with Ada.Text_IO;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Bounded_Synchronized_Queues;
with Ada.Exceptions;
with GNAT.OS_Lib;

use Ada.Text_IO;
use Ada.Containers;
use Ada.Exceptions;

procedure sieve is
   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   LIMIT : constant := 2000;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   package SQI is
      new Synchronized_Queue_Interfaces (
         Element_Type => POSITIVE
      );
   package BSQ is
      new Bounded_Synchronized_Queues (
         Queue_Interfaces => SQI,
         Default_Capacity => 7
      );

   subtype QUEUE is BSQ.Queue;
   type access_QUEUE is access all QUEUE;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   task type Prime_Filter(Input_Channel: access_QUEUE);
   type access_FILTER is access Prime_Filter;

   function New_Filter(Channel: access_QUEUE)
     return access_FILTER
     with Inline is
   begin
      return F : access_FILTER do
         F := new Prime_Filter(Channel);
      end return;
   end New_Filter;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   task Odds_Generator;
   task body Odds_Generator is
      odds : aliased QUEUE;
      candidate : POSITIVE := 2;
   begin
      -- print first prime (2)
      Put_Line(candidate'Image);

      -- generate candidates and send them to odds
      candidate := 3;
      declare
         F : access_FILTER := New_Filter(odds'Unchecked_Access);
      begin null; end;
      --
      while candidate <= LIMIT loop
         odds.Enqueue(candidate);
         candidate := candidate + 2;
      end loop;
      --
      delay 1.1111; -- TODO!
      GNAT.OS_Lib.OS_Exit(0);
   exception
      when X : others =>
         Put_Line(Exception_Name(X));
         Put_Line(Exception_Message(X));
         Put_Line(Exception_Information(X));
         raise;
   end Odds_Generator;

   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   task body Prime_Filter is
      Output_Channel : aliased QUEUE;
      prime, candidate : POSITIVE;
   begin
      -- get and print a prime
      Input_Channel.Dequeue(prime);
      Put_Line(prime'Image);

      -- search next primes
      declare
         F : access_FILTER := New_Filter(Output_Channel'Unchecked_Access);
      begin null; end;
      --
      loop
         Input_Channel.Dequeue(candidate);
         if (candidate rem prime) /= 0 then
            Output_Channel.Enqueue(candidate);
         end if;
      end loop;
   exception
      when X : others =>
         Put_Line(Exception_Name(X));
         Put_Line(Exception_Message(X));
         Put_Line(Exception_Information(X));
         raise;
   end Prime_Filter;

begin
   ------------------------------------------------------------
   --
   ------------------------------------------------------------
   null;

end sieve;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
