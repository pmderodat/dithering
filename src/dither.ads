with HAL.Bitmap;

procedure Dither
  (Input  : HAL.Bitmap.Bitmap_Buffer'Class;
   Output : in out HAL.Bitmap.Bitmap_Buffer'Class)
  with Pre => Input.Width = Output.Width
  and then Input.Height = Output.Height;
--  Turn Input into a black-and-white dithered image and store the result in
--  Output. Input and Output must have the same widthes and heights.
