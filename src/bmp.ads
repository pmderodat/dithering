with Interfaces; use Interfaces;

with HAL;            use HAL;
with HAL.Bitmap;     use HAL.Bitmap;
with HAL.Filesystem; use HAL.Filesystem;

package BMP is

   --  TODO??? Handle the file format on big-endian architectures. For now, we
   --  assume this runs on little-endian host.

   BMP_Magic : constant String := "BM";

   type Compression_Method_Type is
     (BI_RGB,
      BI_LRE8,
      BI_RLE4,
      BI_BITFIELDS,
      BI_JPEG,
      BI_PNG,
      BI_ALPHABITFIELDS,
      BI_CMYK,
      BI_CMYKRLE8,
      BI_CMYKRLE4)
     with Size => 32;
   for Compression_Method_Type use
     (BI_RGB            => 0,
      BI_LRE8           => 1,
      BI_RLE4           => 2,
      BI_BITFIELDS      => 3,
      BI_JPEG           => 4,
      BI_PNG            => 5,
      BI_ALPHABITFIELDS => 6,
      BI_CMYK           => 11,
      BI_CMYKRLE8       => 12,
      BI_CMYKRLE4       => 13);

   BMP_Header_Byte_Size : constant := 14;
   DIB_Header_Byte_Size : constant := 40;

   type BMP_Header is record
      Magic                  : String (1 .. 2);
      Size                   : Unsigned_32;
      Reserved_1, Reserved_2 : Unsigned_16;
      Data_Offset            : Unsigned_32;
   end record
     with Size => BMP_Header_Byte_Size * 8;

   for BMP_Header use record
      Magic       at 0  range 0 .. 15;
      Size        at 2  range 0 .. 31;
      Reserved_1  at 6  range 0 .. 15;
      Reserved_2  at 8  range 0 .. 15;
      Data_Offset at 10 range 0 .. 31;
   end record;

   type DIB_Header is record
      Size                  : Unsigned_32;
      Width                 : Unsigned_32;
      Height                : Unsigned_32;
      Color_Planes          : Unsigned_16;
      Bits_Per_Pixel        : Unsigned_16;
      Compression_Method    : Compression_Method_Type;
      Data_Size             : Unsigned_32;
      Horizontal_Resolution : Unsigned_32;
      Vertical_Resolution   : Unsigned_32;
      Color_Palette_Size    : Unsigned_32;
      Important_Color_Count : Unsigned_32;
   end record
     with Size => DIB_Header_Byte_Size * 8;

   for DIB_Header use record
      Size                  at 0  range 0 .. 31;
      Width                 at 4  range 0 .. 31;
      Height                at 8  range 0 .. 31;
      Color_Planes          at 12 range 0 .. 15;
      Bits_Per_Pixel        at 14 range 0 .. 15;
      Compression_Method    at 16 range 0 .. 31;
      Data_Size             at 20 range 0 .. 31;
      Horizontal_Resolution at 24 range 0 .. 31;
      Vertical_Resolution   at 28 range 0 .. 31;
      Color_Palette_Size    at 32 range 0 .. 31;
      Important_Color_Count at 36 range 0 .. 31;
   end record;

   Read_Error   : exception;
   Write_Error  : exception;
   Invalid_Data : exception;

   type Byte_Array_Access is private;
   --  Access type used to hold the dynamically allocated buffer in a bitmap

   type Bitmap_Allocation is record
      Bitmap : Bitmap_Buffer;
      --  Holder for an allocated bitmap

      Buffer : Byte_Array_Access;
      --  The memory that is dynamically allocated
   end record;

   function Create
     (Mode          : Bitmap_Color_Mode;
      Width, Height : Natural) return Bitmap_Allocation;
   --  Allocate resources to hold a bitmap for the given mode, width and
   --  height. When done with the result, one must call Destroy on it.

   procedure Destroy (BM_Alloc : in out Bitmap_Allocation);
   --  Free the allocated resources in BM_Alloc

   function Load (File : in out File_Handle'Class) return Bitmap_Allocation;
   --  Load the image encoded in BMP file and create a bitmap buffer out of it

   procedure Save (File : in out File_Handle'Class; BM : Bitmap_Buffer'Class);
   --  Write a BMP file with the content of the BM bitmap buffer

private

   type Byte_Array_Access is access Byte_Array;

end BMP;
