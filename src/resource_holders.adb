package body Resource_Holders is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Holder : in out Holder_Type) is
   begin
      if Holder.Value_Present then
         Finalize (Holder.Value);
      end if;
   end Finalize;

   ------------
   -- Create --
   ------------

   function Create (Value : T) return Holder_Type is
   begin
      return (Ada.Finalization.Limited_Controlled with
              Value_Present => True,
              Value => Value);
   end Create;

   ----------
   -- Move --
   ----------

   function Move (Holder : in out Holder_Type) return T is
      Result : constant T := Holder.Value;
   begin
      Holder.Value_Present := False;
      return Result;
   end Move;

end Resource_Holders;
