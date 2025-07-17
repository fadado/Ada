-- sieve.adb

with Ada.Command_Line;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Bounded_Synchronized_Queues;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Exceptions;
with Ada.Task_Identification;

use Ada;

procedure Sieve is

   procedure Halt
     (X: Exceptions.EXCEPTION_OCCURRENCE)
   is
      use Text_IO;
      use Task_Identification;
   begin
      New_Line(Standard_Error);
      Put_Line(Standard_Error, Exceptions.Exception_Information(X));

      Abort_Task(Environment_Task);
   end Halt;

   ------------------------------------------------------------
   -- Set of numbers to search for primes
   ------------------------------------------------------------

   subtype NUMBER is POSITIVE;
   -- We use 1 as a token to signal task's ending;
   -- primes are >= 2

   ------------------------------------------------------------
   -- Channels between tasks
   ------------------------------------------------------------

   package Synchronized_Queue_Interface is
      new Containers.Synchronized_Queue_Interfaces (
         Element_Type => NUMBER
      );

   package Bounded_Synchronized_Queue is
      new Containers.Bounded_Synchronized_Queues (
         Queue_Interfaces => Synchronized_Queue_Interface,
         Default_Capacity => 2   -- > 2 is not faster
      );

   subtype NUMERIC_CHANNEL is Bounded_Synchronized_Queue.QUEUE;

   type CHANNEL is access NUMERIC_CHANNEL;

   ------------------------------------------------------------
   -- Task to generate odd numbers starting at 3
   ------------------------------------------------------------

   task type ODD_NUMBERS_GENERATOR (
      Output : CHANNEL;
      Limit  : NUMBER
   );

   type GENERATOR is access ODD_NUMBERS_GENERATOR;

   procedure Launch (X: GENERATOR) is null with Inline;

   task body ODD_NUMBERS_GENERATOR
   is
      odd : NUMBER := 3;
   begin
      while odd <= Limit loop
         Output.Enqueue(odd);
         odd := odd + 2;
      end loop;

      Output.Enqueue(1);

   exception
      when X : others => Halt(X);
   end ODD_NUMBERS_GENERATOR;

   ------------------------------------------------------------
   -- Task to reject (or pass) prime cadidates
   ------------------------------------------------------------

   task type FILTER_PRIME_MULTIPLES (
      Input  : CHANNEL;
      Output : CHANNEL;
      Prime  : NUMBER
   );

   type FILTER is access FILTER_PRIME_MULTIPLES;

   procedure Launch (X: FILTER) is null with Inline;

   task body FILTER_PRIME_MULTIPLES
   is
      function Is_Multiple 
        (n : NUMBER) return BOOLEAN
      is (n rem Prime = 0)
      with Inline;

      candidate : NUMBER;
   begin
      loop
         Input.Dequeue(candidate);
         exit when candidate = 1;

         if Is_Multiple(candidate) then
            null; -- reject candidate
         else
            Output.Enqueue(candidate);
         end if;
      end loop;

      Output.Enqueue(1);

   exception
      when X : others => Halt(X);
   end FILTER_PRIME_MULTIPLES;

   ------------------------------------------------------------
   -- Build the sieve as a chain of filters
   ------------------------------------------------------------

   task type SIEVE_GENERATOR (
      Output : CHANNEL;
      Limit  : NUMBER
   );

   type SIEVE is access SIEVE_GENERATOR;

   procedure Launch (X: SIEVE) is null with Inline;

   task body SIEVE_GENERATOR
   is
      prime  : NUMBER;
      source : CHANNEL;
      result : CHANNEL;
   begin
      Output.Enqueue(2); -- the only even prime

      result := new NUMERIC_CHANNEL;
      Launch (
         new ODD_NUMBERS_GENERATOR (result, Limit)
      );

      loop
         source := result;

         source.Dequeue(prime);
         exit when prime = 1;
         Output.Enqueue(prime);

         result := new NUMERIC_CHANNEL;
         Launch (
            new FILTER_PRIME_MULTIPLES (source, result, prime)
         );
      end loop;

      Output.Enqueue(1);

   exception
      when X : others => Halt(X);
   end SIEVE_GENERATOR;

------------------------------------------------------------------------
-- Manage command line and start and consume the sieve
------------------------------------------------------------------------

   procedure Display_Sieve 
     (limit: NUMBER)
   is
      use Text_IO;
      use Integer_Text_IO;

      count : NATURAL := 0;

      procedure Print
        (n: NUMBER)
      with Inline
      is
         Field_Size : constant := 7;
         Columns    : constant := 10;
      begin
         count := count + 1;
         Put(n, Width => Field_Size);
         if count = Columns then
            count := 0;
            New_Line;
         end if;
      end;

      primes : CHANNEL;
      n      : NUMBER;
   begin
      primes := new NUMERIC_CHANNEL;
      Launch (
         new SIEVE_GENERATOR (primes, limit)
      );

      loop
         primes.Dequeue(n);
         exit when n = 1;
         Print(n);
      end loop;

      New_Line;
   end Display_Sieve;

begin
   declare
      use Command_Line;

      procedure Usage
      is
         use Text_IO;
      begin
         Put_Line(Standard_Error, "Usage: ./sieve [LIMIT]");
         Put_Line(Standard_Error, "LIMIT must by a number greater than 1");
      end;

      limit : NUMBER := 541; -- default limit: the first 100 primes
   begin
      Set_Exit_Status(Failure);

      if Argument_Count > 0 then
         begin
            limit := NUMBER'Value(Argument(1));
            -- this conversion can raise Constraint_Error

            if limit = 1 then raise Constraint_Error; end if;
            -- extra constraint
         exception
            when Constraint_Error =>
               Usage;
               return;
         end;
      end if;

      Display_Sieve(limit);

      Set_Exit_Status(Success);
   end;
end Sieve;

-- ¡ISO-8859-1!
-- vim:tabstop=3:shiftwidth=3:expandtab:autoindent
-- vim:fileformat=dos:fileencoding=latin1:syntax=ada
