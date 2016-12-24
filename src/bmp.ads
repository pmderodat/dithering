with HAL.Bitmap;     use HAL.Bitmap;
with HAL.Filesystem; use HAL.Filesystem;

package BMP is

   function Load (File : in out File_Handle'Class) return Bitmap_Buffer;
   --  Load the image encoded in BMP file and create a bitmap buffer out of it

   procedure Save (File : in out File_Handle'Class; BM : Bitmap_Buffer'Class);
   --  Write a BMP file with the content of the BM bitmap buffer

end BMP;
