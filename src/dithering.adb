with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;      use Ada.Text_IO;

with HAL.Bitmap;        use HAL.Bitmap;
with HAL.Filesystem;    use HAL.Filesystem;
with Native.Filesystem; use Native.Filesystem;

with BMP;

procedure Dithering is

   Exit_With_Failure : exception;
   --  Exception to be raised after a fatal error. Top-level will handle it and
   --  turn it into a proper process exit.

   procedure Main;
   --  This runs the whole program

   procedure Process_File (Handle : Any_File_Handle);

   ----------
   -- Main --
   ----------

   procedure Main is
      FS : Native_FS_Driver;
   begin
      if Argument_Count /= 1 then
         Put_Line ("Wrong number of arguments.");
         New_Line;
         Put_Line ("Usage: " & Command_Name & " BMP-FILE");
         raise Exit_With_Failure;
      end if;

      if Create (FS, Ada.Directories.Current_Directory) /= Status_Ok then
         Put_Line ("Could not mount current directory");
         raise Exit_With_Failure;
      end if;

      declare
         Filename : constant String := Argument (1);
         Handle   : Any_File_Handle;
      begin
         if FS.Open (Filename, Read_Only, Handle) /= Status_Ok then
            Put_Line ("Could not open " & Filename);
            raise Exit_With_Failure;
         end if;

         Process_File (Handle);

         if Handle.Close /= Status_Ok then
            Put_Line ("Could not close " & Filename);
            raise Exit_With_Failure;
         end if;
      end;
   end Main;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File (Handle : Any_File_Handle) is
      BM : Bitmap_Buffer := BMP.Load (Handle.all);

      pragma Unreferenced (BM);
   begin
      null;
   end Process_File;

begin
   Main;
exception
   when Exit_With_Failure =>
      Set_Exit_Status (Failure);
end Dithering;
