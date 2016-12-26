with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;      use Ada.Text_IO;

with HAL.Filesystem;    use HAL.Filesystem;
with Native.Filesystem; use Native.Filesystem;

with BMP;
with Resource_Holders;

procedure Dithering is

   Exit_With_Failure : exception;
   --  Exception to be raised after a fatal error. Top-level will handle it and
   --  turn it into a proper process exit.

   procedure Main;
   --  This runs the whole program

   procedure Process_File (Read_Handle, Write_Handle : Any_File_Handle);

   procedure Close (Handle : in out Any_File_Handle);

   package File_Handle_Holders is new Resource_Holders
     (Any_File_Handle, Close);

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
         Read_Handle, Write_Handle : File_Handle_Holders.Holder_Type;
      begin
         if FS.Open (Read_Filename, Read_Only, Read_Handle.Value) /= Status_Ok
         then
            Put_Line ("Could not open " & Read_Filename);
            raise Exit_With_Failure;
         end if;
         File_Handle_Holders.Set_Present (Read_Handle);

         if FS.Create_Node (Write_Filename, Regular_File) /= Status_Ok
           or else FS.Open
             (Write_Filename, Write_Only, Write_Handle.Value) /= Status_Ok
         then
            Put_Line ("Could not open " & Write_Filename);
            raise Exit_With_Failure;
         end if;
         File_Handle_Holders.Set_Present (Write_Handle);

         Process_File (Read_Handle.Value, Write_Handle.Value);
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

   -----------
   -- Close --
   -----------

   procedure Close (Handle : in out Any_File_Handle) is
   begin
      if Handle.Close /= Status_Ok then
         Put_Line ("Could not close a file");
      end if;
      Handle := null;
   end Close;

begin
   Main;
exception
   when Exit_With_Failure =>
      Set_Exit_Status (Failure);
end Dithering;
