with Ada.Unchecked_Deallocation;

with System;

with Resource_Holders;

package body BMP is

   type RGB24_Pixel is record
      Blue, Green, Red : Unsigned_8;
   end record
     with Size => 24;
   --  Representation of a pixel in a 24-bit BI_RGB BMP file

   for RGB24_Pixel use record
      Blue  at 0 range 0 .. 7;
      Green at 1 range 0 .. 7;
      Red   at 2 range 0 .. 7;
   end record;

   function "+" (Pixel : RGB24_Pixel) return Bitmap_Color
   is (Alpha => 255,
       Red   => Pixel.Red,
       Green => Pixel.Green,
       Blue  => Pixel.Blue);

   function "+" (Pixel : Bitmap_Color) return RGB24_Pixel
   is (Blue  => Pixel.Blue,
       Green => Pixel.Green,
       Red   => Pixel.Red);

   type RGB24_Array is array (Natural range <>) of RGB24_Pixel
     with Pack => True;

   procedure Free is new Ada.Unchecked_Deallocation
     (Byte_Array, Byte_Array_Access);

   package Bitmap_Allocation_Holders is new Resource_Holders
     (Bitmap_Allocation, Destroy);

   package Byte_Array_Allocation_Holders is new Resource_Holders
     (Byte_Array_Access, Free);

   function Pad (Size, Step : Natural) return Natural is
     ((Size + Step - 1) / Step * Step);
   --  Return Size or the next natural that is a multiple of Step

   function RGB24_Row_Size (Width : Natural) return Natural
   is (Pad (Width * RGB24_Pixel'Size / System.Storage_Unit, 4));
   --  Given the width of an image, determine how large a padded row will be,
   --  in bytes.

   ------------
   -- Create --
   ------------

   function Create
     (Mode          : Bitmap_Color_Mode;
      Width, Height : Natural) return Bitmap_Allocation
   is
      Result : Bitmap_Allocation;

      Pixel_Count : constant Natural := Width * Height;
      Byte_Count  : constant Natural :=
        Bits_Per_Pixel (Mode) * Pixel_Count;
   begin
      Result.Buffer := new Byte_Array (1 .. Byte_Count);

      Result.Bitmap.Addr := Result.Buffer.all'Address;
      Result.Bitmap.Width := Width;
      Result.Bitmap.Height := Height;
      Result.Bitmap.Color_Mode := Mode;

      return Result;
   end Create;

   --------------
   -- Load_BMP --
   --------------

   function Load (File : in out File_Handle'Class) return Bitmap_Allocation is
      Result : Bitmap_Allocation_Holders.Holder_Type :=
        Bitmap_Allocation_Holders.Create ((Bitmap => <>, Buffer => null));
      BM     : Bitmap_Buffer renames Result.Value.Bitmap;

      --  Buffers used to read headers from File

      BMP_Hdr_Buffer : Byte_Array (1 .. Natural (BMP_Header_Byte_Size));
      DIB_Hdr_Buffer : Byte_Array (1 .. Natural (DIB_Header_Byte_Size));

      --  Overlays to decode these headers

      BMP_Hdr : BMP_Header;
      for BMP_Hdr'Address use BMP_Hdr_Buffer'Address;

      DIB_Hdr : DIB_Header;
      for DIB_Hdr'Address use DIB_Hdr_Buffer'Address;

   begin
      --  Read the two headers

      if File.Read (BMP_Hdr_Buffer) /= Status_Ok then
         raise Read_Error;
      elsif BMP_Hdr.Magic /= BMP_Magic then
         raise Invalid_Data with "unknown magic";
      end if;

      if File.Read (DIB_Hdr_Buffer) /= Status_Ok then
         raise Read_Error;
      end if;

      --  Make sure we can handle this file format

      if DIB_Hdr.Color_Planes /= 1 then
         raise Invalid_Data with "unexpected number of color planes";
      end if;

      if not DIB_Hdr.Compression_Method'Valid then
         raise Invalid_Data with "unknown compression method";
      elsif DIB_Hdr.Compression_Method /= BI_RGB then
         raise Invalid_Data with "unhandled compression method";
      end if;

      if DIB_Hdr.Bits_Per_Pixel /= 24 then
         raise Invalid_Data with "unhandled number of bits per pixel";
      end if;

      --  Now allocate the buffer to hold image data

      Result.Value := Create
        (ARGB_8888, Natural (DIB_Hdr.Width), Natural (DIB_Hdr.Height));

      --  ... and finally decode data from the file!

      if File.Seek (IO_Count (BMP_Hdr.Data_Offset)) /= Status_Ok then
         raise Read_Error;
      end if;

      declare
         Width           : constant Natural := Natural (DIB_Hdr.Width);
         Padded_Row_Size : constant Natural := RGB24_Row_Size (Width);

         Read_Buffer : constant Byte_Array_Allocation_Holders.Holder_Type :=
           Byte_Array_Allocation_Holders.Create
             (new Byte_Array (1 .. Padded_Row_Size));
         Data        : RGB24_Array (1 .. Width)
           with Address => Read_Buffer.Value.all'Address;
      begin
         --  Data is organized in rows, and the greatest Y indexes come first:
         --  read them in file order (reverse Y indexes).

         for Row_I in reverse 1 .. Natural (DIB_Hdr.Height) loop
            if File.Read (Read_Buffer.Value.all) /= Status_Ok then
               raise Read_Error;
            end if;

            for Cell_I in Data'Range loop
               BM.Set_Pixel
                 (Cell_I - 1, Row_I - 1, +Data (Cell_I));
            end loop;
         end loop;
      end;

      return Bitmap_Allocation_Holders.Move (Result);
   end Load;

   --------------
   -- Save_BMP --
   --------------

   procedure Save (File : in out File_Handle'Class; BM : Bitmap_Buffer'Class)
   is
      --  First thing to do: compute at which offsets the various parts of the
      --  file will appear.

      Padded_Row_Size : constant Natural := RGB24_Row_Size (BM.Width);
      Write_Buffer    : constant Byte_Array_Allocation_Holders.Holder_Type :=
        Byte_Array_Allocation_Holders.Create
          (new Byte_Array (1 .. Padded_Row_Size));
      Data            : RGB24_Array (1 .. BM.Width)
        with Address => Write_Buffer.Value.all'Address;

      Data_Offset : constant Natural :=
        BMP_Header_Byte_Size + DIB_Header_Byte_Size;
      Data_Size   : constant Natural :=
        Padded_Row_Size * BM.Height;
      File_Size : constant Natural := Data_Offset + Data_Size;
   begin

      --  Write headers

      declare
         BMP_Hdr : constant BMP_Header :=
           (Magic       => BMP_Magic,
            Size        => Unsigned_32 (File_Size),
            Reserved_1  => 0,
            Reserved_2  => 0,
            Data_Offset => Unsigned_32 (Data_Offset));

         DIB_Hdr : constant DIB_Header :=
           (Size                  => DIB_Header_Byte_Size,
            Width                 => Unsigned_32 (BM.Width),
            Height                => Unsigned_32 (BM.Height),
            Color_Planes          => 1,
            Bits_Per_Pixel        => 24,
            Compression_Method    => BI_RGB,
            Data_Size             => Unsigned_32 (Data_Size),
            Horizontal_Resolution => 1,
            Vertical_Resolution   => 1,
            Color_Palette_Size    => 0,
            Important_Color_Count => 0);

         --  Overlays to write the above headers to File

         BMP_Hdr_Buffer : Byte_Array (1 .. Natural (BMP_Header_Byte_Size));
         for BMP_Hdr_Buffer'Address use BMP_Hdr'Address;

         DIB_Hdr_Buffer : Byte_Array (1 .. Natural (DIB_Header_Byte_Size));
         for DIB_Hdr_Buffer'Address use DIB_Hdr'Address;

      begin
         if File.Write (BMP_Hdr_Buffer) /= Status_Ok
           or else File.Write (DIB_Hdr_Buffer) /= Status_Ok
         then
            raise Write_Error;
         end if;
      end;

      --  Then write image data

      for Row_I in reverse 1 .. BM.Height loop
         for Cell_I in 1 .. BM.Width loop
            Data (Cell_I) := +BM.Get_Pixel (Cell_I - 1, Row_I - 1);
         end loop;
         if File.Write (Write_Buffer.Value.all) /= Status_Ok then
            raise Write_Error;
         end if;
      end loop;
   end Save;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (BM_Alloc : in out Bitmap_Allocation) is
   begin
      Free (BM_Alloc.Buffer);
   end Destroy;

end BMP;
