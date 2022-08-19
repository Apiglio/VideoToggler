unit form_video_toggler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LazUTF8,
  PasLibVlcPlayerUnit, timeline_toggle;

type

  { TForm_VideoToggler }

  TForm_VideoToggler = class(TForm)
    Button1: TButton;
    Button_play: TButton;
    Edit_VideoFileName: TEdit;
    GroupBox_TimelineToggle: TGroupBox;
    PasLibVlcPlayer_Preview: TPasLibVlcPlayer;
    procedure Button1Click(Sender: TObject);
    procedure Button_playClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PasLibVlcPlayer_PreviewMediaPlayerPositionChanged(
      Sender: TObject; aposition: Single);
  private

  public
    procedure UserChangeCursorPos(ACursorPos:TTimelineTickPos);
  end;

var
  Form_VideoToggler: TForm_VideoToggler;
  TimelineToggle:TTimelineToggle;
  vstream:TMemoryStream;

implementation

{$R *.lfm}

{ TForm_VideoToggler }

procedure TForm_VideoToggler.FormCreate(Sender: TObject);
begin
  TimelineToggle:=TTimelineToggle.Create(GroupBox_TimelineToggle);
  TimelineToggle.Parent:=GroupBox_TimelineToggle;
  TimelineToggle.Align:=alClient;
  TimelineToggle.MinPosition:=0;
  TimelineToggle.MaxPosition:=293000;//7200000;
  TimelineToggle.OnUserChangeCursorPos:=@UserChangeCursorPos;
  vstream:=TMemoryStream.Create;
end;

procedure TForm_VideoToggler.FormDestroy(Sender: TObject);
var tmpState:TPasLibVlcPlayerState;
begin
  tmpState:=PasLibVlcPlayer_Preview.GetState;
  //PasLibVlcPlayer_Preview.Stop;
  PasLibVlcPlayer_Preview.Free;
  vstream.Free;
  TimelineToggle.Free;
end;

procedure TForm_VideoToggler.PasLibVlcPlayer_PreviewMediaPlayerPositionChanged(
  Sender: TObject; aposition: Single);
var ms:int64;
begin
  //Memo1.Clear;
  //Memo1.Lines.Add(FloatToStrF(aposition,ffFixed,3,3));
  //with TimelineToggle do CursorPos:=round(MinPosition+(MaxPosition - MinPosition)*aposition);
  ms:=PasLibVlcPlayer_Preview.GetVideoPosInMs;
  TimelineToggle.CursorPos:=ms;
  ms:=PasLibVlcPlayer_Preview.GetVideoLenInMs;
  if (ms<>0) and (TimelineToggle.MaxPosition<>ms) then TimelineToggle.MaxPosition:=ms;
end;

procedure TForm_VideoToggler.Button1Click(Sender: TObject);
begin
  TimelineToggle.AddTickAtCursorPos;
end;

procedure TForm_VideoToggler.Button_playClick(Sender: TObject);
var ms:int64;
begin
  case PasLibVlcPlayer_Preview.GetState of
    plvPlayer_Playing:PasLibVlcPlayer_Preview.Pause;
    plvPlayer_Ended,plvPlayer_NothingSpecial:PasLibVlcPlayer_Preview.Play(WideString(Utf8ToAnsi(Edit_VideoFileName.Text)));
    plvPlayer_Paused:PasLibVlcPlayer_Preview.Resume;
    else ;
  end;
  //ms:=PasLibVlcPlayer_Preview.GetVideoLenInMs;
  //TimelineToggle.MaxPosition:=ms;
end;

procedure TForm_VideoToggler.UserChangeCursorPos(ACursorPos:TTimelineTickPos);
begin
  //PasLibVlcPlayer_Preview.Pause;
  PasLibVlcPlayer_Preview.SetVideoPosInMs(ACursorPos);
  //这里太快两次会让player卡住
  //PasLibVlcPlayer_Preview.Resume;
end;


end.

