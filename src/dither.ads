with HAL.Bitmap;

procedure Dither (Input, Output : HAL.Bitmap.Bitmap_Buffer)
  with Pre => Input.Width = Output.Width
  and then Input.Height = Output.Height;
--  Turn Input into a black-and-white dithered image and store the result in
--  Output. Input and Output must have the same widthes and heights.
