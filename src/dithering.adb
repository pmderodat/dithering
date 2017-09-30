with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;      use Ada.Text_IO;

with HAL.Bitmap;
with HAL.Filesystem;    use HAL.Filesystem;
with Native.Filesystem; use Native.Filesystem;

with BMP;
with Dither;
with Resource_Holders;

procedure Dithering is

   Exit_With_Failure : exception;
   --  Exception to be raised after a fatal error. Top-level will handle it and
   --  turn it into a proper process exit.

   procedure Main;
   --  This runs the whole program

   procedure Parse_Geometry (S : String; Width, Height : out Natural);
   --  Parse S, a string supposed to be of the form "WIDTHxHEIGHT" (two
   --  non-negative numbers separated by the "x" character). If S does not
   --  comply, complain on the standard output and raise an Exit_With_Failure
   --  exception.

   procedure Process_File
     (Read_Handle, Write_Handle : Any_File_Handle;
      Width, Height             : Natural);

   procedure Close (Handle : in out Any_File_Handle);

   package FSD_Holders is new Resource_Holders (Native_FS_Driver_Ref, Destroy);
   package File_Handle_Holders is new Resource_Holders
     (Any_File_Handle, Close);

   ----------
   -- Main --
   ----------

   procedure Main is
      FS_Holder : constant FSD_Holders.Holder_Type :=
         FSD_Holders.Create (new Native_FS_Driver);
      FS : Native_FS_Driver renames FS_Holder.Value.all;
   begin
      if Argument_Count not in 1 | 2 then
         Put_Line ("Wrong number of arguments.");
         New_Line;
         Put_Line ("Usage: " & Command_Name & " [WIDTHxHEIGHT] BMP-FILE");
         raise Exit_With_Failure;
      end if;

      if Create (FS, Ada.Directories.Current_Directory) /= Status_Ok then
         Put_Line ("Could not mount current directory");
         raise Exit_With_Failure;
      end if;

      declare
         Read_Filename             : constant String :=
           Argument (Argument_Count);
         Write_Filename            : constant String := Read_Filename & ".new";
         Width, Height             : Natural := 0;
         Read_Handle, Write_Handle : File_Handle_Holders.Holder_Type;
      begin
         if Argument_Count > 1 then
            Parse_Geometry (Argument (1), Width, Height);
         end if;

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

         Process_File (Read_Handle.Value, Write_Handle.Value, Width, Height);
      end;
   end Main;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File
     (Read_Handle, Write_Handle : Any_File_Handle;
      Width, Height             : Natural)
   is
      Input  : BMP.Bitmap_Allocation := BMP.Load (Read_Handle.all);
      Output : BMP.Bitmap_Allocation :=
        BMP.Create
          (HAL.Bitmap.ARGB_8888,
           (if Width = 0 then Input.Bitmap.Width else Width),
           (if Height = 0 then Input.Bitmap.Height else Height));
   begin
      Dither (Input.Bitmap, Output.Bitmap);
      BMP.Destroy (Input);

      BMP.Save (Write_Handle.all, Output.Bitmap);
      BMP.Destroy (Output);
   end Process_File;

   --------------------
   -- Parse_Geometry --
   --------------------

   procedure Parse_Geometry (S : String; Width, Height : out Natural) is
      X_Index : Natural := 0;
   begin
      --  Look for the 'x' separator

      for I in S'Range loop
         if S (I) = 'x' then
            X_Index := I;
            exit;
         end if;
      end loop;
      if X_Index = 0 then
         Put_Line ("Missing 'x' in """ & S & """");
         raise Exit_With_Failure;
      end if;

      --  Convert both dimensinos

      declare
         Width_Slice : String renames S (S'First .. X_Index - 1);
      begin
         Width := Natural'Value (Width_Slice);
      exception
         when Constraint_Error =>
            Put_Line ("Invalid width: """ & Width_Slice & """");
            raise Exit_With_Failure;
      end;

      declare
         Height_Slice : String renames S (X_Index + 1 .. S'Last);
      begin
         Height := Natural'Value (Height_Slice);
      exception
         when Constraint_Error =>
            Put_Line ("Invalid width: """ & Height_Slice & """");
            raise Exit_With_Failure;
      end;
   end Parse_Geometry;

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
