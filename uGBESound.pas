unit uGBESound;

interface
uses FMX.Media;

  procedure PlaySound(mediaplayer: TMediaPlayer; son : string; volume : single = 1);
  procedure RePlaySound(mediaplayer: TMediaPlayer; son : string; volume : single = 1);

implementation

procedure PlaySound(mediaplayer: TMediaPlayer; son : string; volume : single = 1);
begin
  if mediaplayer.State = TMediaState.Playing then mediaplayer.Stop;
  mediaplayer.Volume := volume;
  mediaplayer.FileName := son;
  mediaplayer.Play;
end;

procedure RePlaySound(mediaplayer: TMediaPlayer; son : string; volume : single = 1);
begin
  if mediaplayer.State = TMediaState.Stopped then begin
    mediaplayer.Stop;
    mediaplayer.CurrentTime := 0;
    mediaplayer.Play;
  end;
end;

end.
