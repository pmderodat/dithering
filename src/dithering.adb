with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;      use Ada.Text_IO;

with HAL.Filesystem;    use HAL.Filesystem;
with Native.Filesystem; use Native.Filesystem;

with BMP;

procedure Dithering is

   Exit_With_Failure : exception;
   --  Exception to be raised after a fatal error. Top-level will handle it and
   --  turn it into a proper process exit.

   procedure Main;
   --  This runs the whole program

   procedure Process_File (Read_Handle, Write_Handle : Any_File_Handle);

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
         Read_Filename             : constant String := Argument (1);
         Write_Filename            : constant String := Read_Filename & ".new";
         Read_Handle, Write_Handle : Any_File_Handle;
      begin
         if FS.Open (Read_Filename, Read_Only, Read_Handle) /= Status_Ok then
            Put_Line ("Could not open " & Read_Filename);
            raise Exit_With_Failure;
         end if;
         if FS.Create_Node (Write_Filename, Regular_File) /= Status_Ok
           or else FS.Open
             (Write_Filename, Write_Only, Write_Handle) /= Status_Ok
         then
            Put_Line ("Could not open " & Write_Filename);
            raise Exit_With_Failure;
         end if;

         Process_File (Read_Handle, Write_Handle);

         if Read_Handle.Close /= Status_Ok then
            Put_Line ("Could not close " & Read_Filename);
            raise Exit_With_Failure;
         end if;
         if Write_Handle.Close /= Status_Ok then
            Put_Line ("Could not close " & Write_Filename);
            raise Exit_With_Failure;
         end if;
      end;
   end Main;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File (Read_Handle, Write_Handle : Any_File_Handle) is
      BM : BMP.Bitmap_Allocation := BMP.Load (Read_Handle.all);
   begin
      BMP.Save (Write_Handle.all, BM.Bitmap);
      BMP.Destroy (BM);
   end Process_File;

begin
   Main;
exception
   when Exit_With_Failure =>
      Set_Exit_Status (Failure);
end Dithering;
