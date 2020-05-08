unit uHeightmap;

interface

uses FMX.Graphics, System.UITypes, System.SysUtils, FMX.Effects, System.UIConsts;

  function generateDiamondSquare(size, blurLevel : integer): TBitmap;

implementation

// https://fr.wikipedia.org/wiki/Algorithme_Diamant-Carr%C3%A9
function generateDiamondSquare(size, blurLevel : integer): TBitmap;
var
  bmp : TBitmap;
  bitmapData: TBitmapData;
  i, h, x, y, id, decallage, somme, n : integer;
  moyenne : single;
  rec : TAlphaColorRec;
  aByte : Byte;
begin
  bmp := TBitmap.Create;
  result := TBitmap.Create;

  h := size;
  bmp.Width := h;
  bmp.Height := h;

  try
    if bmp.Map(TMapAccess.ReadWrite, bitmapData) then
    begin
      bitmapData.SetPixel(0,0, TAlphaColorRec.Black);
      bitmapData.SetPixel(0,h-1, TAlphaColorRec.Black);
      bitmapData.SetPixel(h-1,h-1, TAlphaColorRec.Black);
      bitmapData.SetPixel(h-1,0, TAlphaColorRec.Black);

      i := bmp.Width-1;

      while i > 1 do
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
            rec.R := aByte;
            rec.G := aByte;
            rec.B := aByte;

            bitmapData.SetPixel(x,y, TAlphaColor(rec));
          end;
        end;

        decallage := 0;
        for x := 0 to h do
        begin
          if decallage = 0 then decallage := id
          else decallage := 0;

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
            rec.R := aByte;
            rec.G := aByte;
            rec.B := aByte;

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

end.
