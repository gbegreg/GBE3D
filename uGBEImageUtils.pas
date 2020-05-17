unit uGBEImageUtils;

interface

uses FMX.Graphics, System.UITypes, System.SysUtils, FMX.Effects, System.UIConsts, System.types;

  function generateDiamondSquare(size, blurLevel : integer; color: TAlphaColor = TAlphaColorRec.White; bordure : boolean = false; colorBordure : TAlphaColor = TAlphaColorRec.Black): TBitmap;
  function tileImage(imageOrigine : TBitmap; nbX, nbY : integer): TBitmap;

implementation

// https://fr.wikipedia.org/wiki/Algorithme_Diamant-Carr%C3%A9
function generateDiamondSquare(size, blurLevel : integer; color: TAlphaColor = TAlphaColorRec.White; bordure : boolean = false; colorBordure : TAlphaColor = TAlphaColorRec.Black): TBitmap;
var
  bmp : TBitmap;
  bitmapData: TBitmapData;
  i, h, x, y, id, decallage, somme, n, min : integer;
  moyenne : single;
  rec : TAlphaColorRec;
  aByte : Byte;
  aR, aG, aB : single;
begin
  bmp := TBitmap.Create;
  result := TBitmap.Create;

  h := size;
  bmp.Width := h;
  bmp.Height := h;

  if bordure then bmp.Clear(colorBordure);

  aR := TAlphaColorRec(color).R / 255;
  aG := TAlphaColorRec(color).G / 255;
  aB := TAlphaColorRec(color).B / 255;

  try
    if bmp.Map(TMapAccess.ReadWrite, bitmapData) then
    begin
      if bordure then
      begin
        bitmapData.SetPixel(1,1, color);
        bitmapData.SetPixel(1,h-2, color);
        bitmapData.SetPixel(h-2,h-2, color);
        bitmapData.SetPixel(h-2,1, color);
        h := h -2;
        i := bmp.Width-2;
        min := 2;
      end
      else
      begin
        bitmapData.SetPixel(0,0, color);
        bitmapData.SetPixel(0,h-1, color);
        bitmapData.SetPixel(h-1,h-1, color);
        bitmapData.SetPixel(h-1,0, color);
        i := bmp.Width-1;
        min := 1;
      end;

      while i > min do
      begin
        id := trunc(i /2);

        // phase diamond
        for x := id to h do
        begin
          for y := id to h do
          begin
            moyenne := (CorrectColor(bitmapData.GetPixel(x - id, y - id)) +
                        CorrectColor(bitmapData.GetPixel(x - id, y + id)) +
                        CorrectColor(bitmapData.GetPixel(x + id, y + id)) +
                        CorrectColor(bitmapData.GetPixel(x + id, y - id))) / 4;

            aByte :=  Round(moyenne + random(id));

            rec.A := $FF;

            rec.R := Round(aByte * aR );
            rec.G := Round(aByte * aG );
            rec.B := Round(aByte * aB );

            bitmapData.SetPixel(x,y, TAlphaColor(rec));
          end;
        end;

        decallage := min-1;
        for x := min-1 to h do
        begin
          if decallage = min-1 then decallage := id
          else decallage := min-1;

          for y := decallage to h do
          begin
            somme := 0;
            n := 0;

            if x >= id then
            begin
              somme := somme + CorrectColor(bitmapData.GetPixel(x - id, y));
              n := n +1;
            end;

            if x +id < h then
            begin
              somme := somme + CorrectColor(bitmapData.GetPixel(x + id, y));
              n := n +1;
            end;

            if y > id then
            begin
              somme := somme + CorrectColor(bitmapData.GetPixel(x, y - id));
              n := n +1;
            end;

            if y + id < h then
            begin
              somme := somme + CorrectColor(bitmapData.GetPixel(x, y + id));
              n := n +1;
            end;

            aByte :=  Round(somme / n + random(id));

            rec.A := $FF;
            rec.R := Round(aByte * aR );
            rec.G := Round(aByte * aG );
            rec.B := Round(aByte * aB );

            bitmapData.SetPixel(x,y, TAlphaColor(rec));
          end;
        end;

        i := id;
      end;

    end;
  finally
    bmp.Unmap(bitmapData);
    blur(bmp.Canvas, bmp,blurLevel);
    result.Width := bmp.Width;
    result.Height := bmp.Height;
    result.CopyFromBitmap(bmp);
    freeAndNil(bmp);
  end;
end;


function tileImage(imageOrigine : TBitmap; nbX, nbY : integer): TBitmap;
var
  X, Y: Integer;
  dX, dY: Integer;
  tileBmp : TBitmap;
begin
  tileBmp := TBitmap.Create;
  tileBmp.Width := imageOrigine.Width * nbX;
  tileBmp.Height := imageOrigine.Height * nbY;

  dX := imageOrigine.Width;
  dY := imageOrigine.Height;

  tileBmp.Canvas.BeginScene;
  Y := 0;
  while Y <= tileBmp.Height do
    begin
      X := 0;
      while X <= tileBmp.Width do
        begin
          tileBmp.Canvas.DrawBitmap(imageOrigine,Rectf(0,0, dx, dy),Rectf(X, Y, x+dx, y+dy),1);
          Inc(X, dX);
        end;
      Inc(Y, dY);
    end;

  tileBmp.Canvas.EndScene;

  result := TBitmap.Create;
  result.Width := tileBmp.Width;
  result.Height := tileBmp.Height;
  result.CopyFromBitmap(tileBmp);
  freeAndNil(tileBmp);
end;

end.
