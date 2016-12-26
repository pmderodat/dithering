with Ada.Finalization;

--  Provide a generic helper for scope-based resources management

generic
   type T is private;
   --  Resource to manage. This helper is useful for types that are not
   --  controlled.

   with procedure Finalize (Object : in out T) is <>;
   --  Procedure that will be called when it is time to deallocate the
   --  resource.

package Resource_Holders is

   type Holder_Type is limited new Ada.Finalization.Limited_Controlled
   with record
      Value_Present : Boolean := False;
      --  Whether there is an actual Value to manage in this holder

      Value : T;
      --  The value that is held
   end record;
   --  Holder for values of type T

   function Create (Value : T) return Holder_Type;
   --  Create a new holder for Value

   procedure Set_Present (Holder : in out Holder_Type);
   --  Tag the value as being present in Holder. This is useful to use
   --  Holder.Value as the out parameter.

   function Move (Holder : in out Holder_Type) return T;
   --  Return the value in Holder and erase the

private

   overriding procedure Finalize (Holder : in out Holder_Type);

end Resource_Holders;
