package body BMP is

   --------------
   -- Load_BMP --
   --------------

   function Load (File : in out File_Handle'Class) return Bitmap_Buffer is
   begin
      raise Program_Error;
      return (others => <>);
   end Load;

   --------------
   -- Save_BMP --
   --------------

   procedure Save (File : in out File_Handle'Class; BM : Bitmap_Buffer'Class)
   is
   begin
      raise Program_Error;
   end Save;

end BMP;
