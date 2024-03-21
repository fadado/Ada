# Keep It Structured Simply

Exploring better abstractions on top of `Ada.Containers`.

## Instantiation example

```ada
-- tester.adb
   package Structure is
      new Ada.Containers.Vectors
        (Index_Type   => POSITIVE, 
         Element_Type => CHARACTER);
   use Structure;
   package Signature is
      new Kiss.Signatures.Stack
        (Data_Type    => VECTOR,
         Element_Type => CHARACTER);
   procedure run_test is
      new Tests.Stack (Signature);

-- tests-stack.ads
   with package Signature is
      new Kiss.Signatures.Stack
        (Data_Type    => <>,
         Element_Type => CHARACTER,
         others       => <>);

-- tests-stack.adb
   package Character_Stack is
      new Kiss.Functors.Stack (Signature);

-- kiss.functors.stack.ads
   with package Signature is
      new Signatures.Stack (<>);
   use Signature;
```
