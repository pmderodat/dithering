with Ada.Unchecked_Deallocation;

procedure Dither (Input, Output : HAL.Bitmap.Bitmap_Buffer) is

   type Luminance_Array is
     array (Natural range <>, Natural range <>) of Integer;
   type Luminance_Array_Access is access Luminance_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Luminance_Array, Luminance_Array_Access);

   Lum : Luminance_Array_Access :=
     new Luminance_Array (0 .. Input.Width - 1,
                          0 .. Input.Height - 1);

   type Error_Destination is record
      DX, DY : Integer;
      Ratio  : Natural;
   end record;

   Dests : constant array (1 .. 4) of Error_Destination :=
     ((1,  0, 7),
      (-1, 1, 3),
      (0,  1, 5),
      (1,  1, 1));

begin
   --  Compute luminance for all pixels

   for Y in Lum'Range (2) loop
      for X in  Lum'Range (1) loop
         declare
            Input_Color : constant HAL.Bitmap.Bitmap_Color :=
              Input.Get_Pixel (X, Y);
         begin
            Lum (X, Y) := (Natural (Input_Color.Red) * 299
                           + Natural (Input_Color.Green) * 587
                           + Natural (Input_Color.Blue) * 114) / 1000;
         end;
      end loop;
   end loop;

   --  Then convert to black and white, propagating error towards right and
   --  bottom pixels.

   for Y in Lum'Range (2) loop
      for X in  Lum'Range (1) loop
         declare
            Old_Lum : constant Integer := Lum (X, Y);
            New_Lum : constant Integer :=
              (if Old_Lum < 128 then 0 else 255);
            Error   : constant Integer := Old_Lum - New_Lum;

         begin
            Lum (X, Y) := New_Lum;

            for D of Dests loop
               declare
                  DX : constant Integer := X + D.DX;
                  DY : constant Integer := Y + D.DY;
               begin
                  if DX in Lum'Range (1) and then DY in Lum'Range (2) then
                     declare
                        L : Integer renames Lum (X + D.DX, Y + D.DY);
                     begin
                        L := L + Error * D.Ratio / 16;
                     end;
                  end if;
               end;
            end loop;
         end;
      end loop;
   end loop;

   --  And now turn the luminances into the final image

   for Y in Lum'Range (2) loop
      for X in  Lum'Range (1) loop
         declare
            L : constant Integer := Lum (X, Y);
            V : constant HAL.Byte := HAL.Byte (if L < 128 then 0 else 255);
         begin
            Output.Set_Pixel (X, Y, (255, V, V, V));
         end;
      end loop;
   end loop;

   Free (Lum);
end Dither;
