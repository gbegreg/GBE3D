unit uGBEImageUtils;

interface

uses FMX.Graphics, System.UITypes, System.SysUtils, FMX.Effects, System.UIConsts, System.types;

  function generateDiamondSquare(size, blurLevel : integer; color: TAlphaColor = TAlphaColorRec.White; bordure : boolean = false; colorBordure : TAlphaColor = TAlphaColorRec.Black): TBitmap;
  function tileImage(imageOrigine : TBitmap; nbX, nbY : integer): TBitmap;
  function cropImage(originBitmap : TBitmap; Xpos, Ypos, width, height: integer): TBitmap;
  function MultiTexturing(imgCarte, imgFond, imgCanalRouge, imgCanalVert, imgCanalBleu : TBitmap; tailleCrop : integer): TBitmap;
  function MultiTexturingZone(img : TBitmap): TBitmap;
  function mixerCouleurPixel(cCarte : TAlphaColor; x, y: integer): TAlphaColor;

var
  textureCanalNoir, textureCanalRouge, textureCanalVert, textureCanalBleu : TBitmap;
  bitmapDataCanalNoir, bitmapDataCanalRouge, bitmapDataCanalVert, bitmapDataCanalBleu: TBitmapData;

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

function cropImage(originBitmap : TBitmap; Xpos, Ypos, width, height: integer): TBitmap;
var
  iRect : TRect;
begin
  iRect.Left := Xpos;
  iRect.Top := Ypos;
  iRect.Width := width;
  iRect.Height := height;

  result := TBitmap.Create;
  result.Width := Width;
  result.Height := Height;
  result.CopyFromBitmap(originBitmap, iRect, 0, 0);
end;

function MultiTexturing(imgCarte, imgFond, imgCanalRouge, imgCanalVert, imgCanalBleu : TBitmap; tailleCrop : integer): TBitmap;
var
  bmpSortie, imagecrop : TBitmap;
  x, y : integer;
  iRect : TRect;

begin
  x := 0;
  y := 0;

  textureCanalNoir := TBitmap.Create(imgFond.Width, imgFond.Height);
  textureCanalNoir.Assign(imgFond);
  textureCanalNoir.Map(TMapAccess.Read, bitmapDataCanalNoir);

  textureCanalBleu := TBitmap.Create(imgCanalBleu.Width, imgCanalBleu.Height);
  textureCanalBleu.Assign(imgCanalBleu);
  textureCanalBleu.Map(TMapAccess.Read, bitmapDataCanalBleu);

  textureCanalRouge := TBitmap.Create(imgCanalRouge.Width, imgCanalRouge.Height);
  textureCanalRouge.Assign(imgCanalRouge);
  textureCanalRouge.Map(TMapAccess.Read, bitmapDataCanalRouge);

  textureCanalVert := TBitmap.Create(imgCanalVert.Width, imgCanalVert.Height);
  textureCanalVert.Assign(imgCanalVert);
  textureCanalVert.Map(TMapAccess.Read, bitmapDataCanalVert);

  iRect.Left := 0;
  iRect.Top := 0;
  iRect.Width := tailleCrop;
  iRect.Height := tailleCrop;

  bmpSortie := TBitmap.Create(tailleCrop, tailleCrop);
  imagecrop := TBitmap.Create(tailleCrop, tailleCrop);

  result := TBitmap.Create(imgCarte.Width, imgCarte.Height);

  while y < imgCarte.height do begin
    while x < imgCarte.Width do begin
      imagecrop.CopyFromBitmap(cropImage(imgCarte, x, y, tailleCrop, tailleCrop));
      bmpsortie.CopyFromBitmap(MultiTexturingZone(imagecrop));
      result.CopyFromBitmap(bmpSortie, iRect, x, y);
      x := x + tailleCrop;
    end;
    y := y + tailleCrop;
    x := 0;
  end;

  textureCanalNoir.Unmap(bitmapDataCanalNoir);
  textureCanalBleu.UnMap(bitmapDataCanalBleu);
  textureCanalRouge.UnMap(bitmapDataCanalRouge);
  textureCanalVert.UnMap(bitmapDataCanalVert);
end;

function MultiTexturingZone(img : TBitmap): TBitmap;
var
  bmp : TBitmap;
  bitmapData: TBitmapData;
  i, j : integer;
begin
  try
    bmp := TBitmap.Create(img.Width, img.Height);
    bmp.Assign(img);

    if (bmp.Map(TMapAccess.ReadWrite, bitmapData)) then
    begin
      for I := 0 to bmp.height-1 do begin
        for j := 0 to bmp.Width-1 do begin
          bitmapData.SetPixel(j,i, mixerCouleurPixel(CorrectColor(bitmapData.GetPixel(j,i)), j,i));
        end;
      end;
    end;

    bmp.Unmap(bitmapdata);
    result := TBitmap.Create(bmp.Width, bmp.Height);
    result.CopyFromBitmap(bmp);
  finally
    FreeAndNil(bmp);
  end;
end;

function mixerCouleurPixel(cCarte : TAlphaColor; x, y: integer): TAlphaColor;
var
  rCarte,gCarte,bCarte, rFond, gFond, bFond, rTextureRouge, gTextureRouge, bTextureRouge,
  rTextureVert, gTextureVert, bTextureVert, rTextureBleu, gTextureBleu, bTextureBleu: byte;
  couleurResult : TAlphaColorRec;
  resTemp1, resTemp2 : single;
begin
  rCarte := TAlphaColorRec(cCarte).R;
  gCarte := TAlphaColorRec(cCarte).G;
  bCarte := TAlphaColorRec(cCarte).B;

  rFond := TAlphaColorRec(bitmapDataCanalNoir.GetPixel(x, y)).R;
  gFond := TAlphaColorRec(bitmapDataCanalNoir.GetPixel(x, y)).G;
  bFond := TAlphaColorRec(bitmapDataCanalNoir.GetPixel(x, y)).B;

  rTextureRouge := TAlphaColorRec(bitmapDataCanalRouge.GetPixel(x, y)).R;
  gTextureRouge := TAlphaColorRec(bitmapDataCanalRouge.GetPixel(x, y)).G;
  bTextureRouge := TAlphaColorRec(bitmapDataCanalRouge.GetPixel(x, y)).B;

  rTextureVert := TAlphaColorRec(bitmapDataCanalVert.GetPixel(x, y)).R;
  gTextureVert := TAlphaColorRec(bitmapDataCanalVert.GetPixel(x, y)).G;
  bTextureVert := TAlphaColorRec(bitmapDataCanalVert.GetPixel(x, y)).B;

  rTextureBleu := TAlphaColorRec(bitmapDataCanalBleu.GetPixel(x, y)).R;
  gTextureBleu := TAlphaColorRec(bitmapDataCanalBleu.GetPixel(x, y)).G;
  bTextureBleu := TAlphaColorRec(bitmapDataCanalBleu.GetPixel(x, y)).B;

  couleurResult.R := rFond;
  couleurResult.G := gFond;
  couleurResult.B := bFond;
  couleurResult.A := 255;

  if (rCarte > gCarte) and (rCarte > bCarte) then begin  // Couleur dominante rouge => utilisation de la texture correspondante au rouge
      resTemp1 := (255-rCarte)/255;
      resTemp2 := rCarte/255;
      couleurResult.R := round(resTemp1 * rFond + resTemp2 * rTextureRouge);
      couleurResult.G := round(resTemp1 * gFond + resTemp2 * gTextureRouge);
      couleurResult.B := round(resTemp1 * bFond + resTemp2 * bTextureRouge);
  end else begin
    if (gCarte > rCarte) and (gCarte > bCarte) then begin // Couleur dominante vert => utilisation de la texture correspondante au vert
        resTemp1 := (255-gCarte)/255;
        resTemp2 := gCarte/255;
        couleurResult.R := round(resTemp1 * rFond + resTemp2 * rTextureVert);
        couleurResult.G := round(resTemp1 * gFond + resTemp2 * gTextureVert);
        couleurResult.B := round(resTemp1 * bFond + resTemp2 * bTextureVert);
    end else begin
      if (bCarte > rCarte) and (bCarte > gCarte) then begin // Couleur dominante bleu => utilisation de la texture correspondante au bleu
          resTemp1 := (255-bCarte)/255;
          resTemp2 := bCarte/255;
          couleurResult.R := round(resTemp1 * rFond + resTemp2 * rTextureBleu);
          couleurResult.G := round(resTemp1 * gFond + resTemp2 * gTextureBleu);
          couleurResult.B := round(resTemp1 * bFond + resTemp2 * bTextureBleu);
      end;
    end;
  end;

  result := CorrectColor(TAlphaColor(couleurResult));
end;

end.
