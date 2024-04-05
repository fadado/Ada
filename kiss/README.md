# Keep It Structured Simply

Exploring better abstractions on top of `Ada.Containers`.

## Instantiation example

```ada
   declare
      package Container is new Ada.Containers.Vectors
         (Index_Type   => POSITIVE, 
          Element_Type => CHARACTER);
      use Container;

      package Stack_Signature is new Kiss.Signatures.Stack
         (Data_Type    => VECTOR,
          Element_Type => CHARACTER);

      package Character_Stack is new Kiss.Functors.Stack
         (Stack_Signature);

      the_stack: Character_Stack.T;
   begin
      the_stack.Push('Z'); 
      the_stack.Push('A'); 
      if the_stack.Pop /= 'A' then raise Tests.Error; end if;
      if the_stack.Pop /= 'Z' then raise Tests.Error; end if;
      if not the_stack.Void then raise Tests.Error; end if;
   end;
```
