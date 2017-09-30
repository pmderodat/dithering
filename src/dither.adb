procedure Dither
  (Input  : HAL.Bitmap.Bitmap_Buffer'Class;
   Output : in out HAL.Bitmap.Bitmap_Buffer'Class)
is
   subtype Height_Type is Natural range 0 .. Output.Height - 1;
   subtype Width_Type is Natural range 0 .. Output.Width - 1;

   type Luminance_Row is array (Width_Type) of Integer;
   type Luminance_Row_Index is mod 2;
   type Luminance_Buffer is array (Luminance_Row_Index) of Luminance_Row;

   function Row (Y : Natural) return Luminance_Row_Index is
      (Luminance_Row_Index'Mod (Y));

   type Error_Destination is record
      DX       : Integer;
      Next_Row : Boolean;
      Ratio    : Natural;
   end record;

   procedure Compute_Luminance (Y : Natural; Lum : out Luminance_Row);
   --  Compute the luminance corresponding to row Y in Output and store it in
   --  Lum.

   -----------------------
   -- Compute_Luminance --
   -----------------------

   procedure Compute_Luminance (Y : Natural; Lum : out Luminance_Row) is
   begin
      for X in Width_Type'Range loop
         declare
            Input_Color : constant HAL.Bitmap.Bitmap_Color :=
              Input.Pixel ((X * Input.Width / Output.Width,
                            Y * Input.Height / Output.Height));
         begin
            Lum (X) := (Natural (Input_Color.Red) * 299
                        + Natural (Input_Color.Green) * 587
                        + Natural (Input_Color.Blue) * 114) / 1000;
         end;
      end loop;
   end Compute_Luminance;

   Lum : Luminance_Buffer;
   Dests : constant array (1 .. 4) of Error_Destination :=
     ((1,  False, 7),
      (-1, True, 3),
      (0,  True, 5),
      (1,  True, 1));

begin
   --  Compute luminance for the first row so that we are ready to process it

   Compute_Luminance (0, Lum (0));

   --  Now for each row...

   for Y in Height_Type'Range loop
      declare
         Lum_Current_Row : Luminance_Row renames
           Lum (Row (Y));
         Lum_Next_Row    : Luminance_Row renames
           Lum (Row (Y + 1));
         Has_Next_Row    : constant Boolean := Y < Height_Type'Last;
      begin
         --  Compute the luminance for the next row (if any)

         if Has_Next_Row then
            Compute_Luminance (Y + 1, Lum_Next_Row);
         end if;

         --  Then convert the current row to black and white directly on the
         --  output bitmap, propagating error towards right and bottom pixels
         --  in the luminance buffer.

         for X in Width_Type'Range loop
            declare
               Old_Lum : constant Integer := Lum_Current_Row (X);
               New_Lum : constant HAL.UInt8 :=
                 (if Old_Lum < 128 then 0 else 255);
               Error   : constant Integer := Old_Lum - Integer (New_Lum);

            begin
               Output.Set_Pixel ((X, Y), (255, New_Lum, New_Lum, New_Lum));

               for D of Dests loop
                  declare
                     DX : constant Integer := X + D.DX;
                  begin
                     if DX in Width_Type'Range
                       and then (not D.Next_Row or else Has_Next_Row)
                     then
                        declare
                           Dest_Lum_Index : constant Luminance_Row_Index :=
                             Row (if D.Next_Row then Y + 1 else Y);
                           L : Integer renames Lum (Dest_Lum_Index) (DX);
                        begin
                           L := L + Error * D.Ratio / 16;
                        end;
                     end if;
                  end;
               end loop;
            end;
         end loop;
      end;
   end loop;
end Dither;
