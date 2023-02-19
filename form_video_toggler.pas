unit form_video_toggler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LazUTF8,
  PasLibVlcPlayerUnit, timeline_toggle;

type

  { TForm_VideoToggler }

  TForm_VideoToggler = class(TForm)
    Button_AddTick: TButton;
    Button_Run: TButton;
    Button_play: TButton;
    Edit_VideoOutput: TEdit;
    Edit_VideoFileName: TEdit;
    GroupBox_TimelineToggle: TGroupBox;
    Label_input: TLabel;
    Label_output: TLabel;
    PasLibVlcPlayer_Preview: TPasLibVlcPlayer;
    procedure Button_AddTickClick(Sender: TObject);
    procedure Button_playClick(Sender: TObject);
    procedure Button_RunClick(Sender: TObject);
    procedure Edit_VideoFileNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure PasLibVlcPlayer_PreviewMediaPlayerPositionChanged(
      Sender: TObject; aposition: Single);
  private

  public
    procedure UserChangeCursorPos(ACursorPos:TTimelineTickPos);
  end;

var
  Form_VideoToggler: TForm_VideoToggler;
  TimelineToggle:TTimelineToggle;

implementation

{$R *.lfm}

{ TForm_VideoToggler }

procedure TForm_VideoToggler.FormCreate(Sender: TObject);
begin
  TimelineToggle:=TTimelineToggle.Create(GroupBox_TimelineToggle);
  TimelineToggle.Parent:=GroupBox_TimelineToggle;
  TimelineToggle.Align:=alClient;
  //TimelineToggle.MinPosition:=0;
  //TimelineToggle.MaxPosition:=7200000;
  TimelineToggle.OnUserChangeCursorPos:=@UserChangeCursorPos;
  TimelineToggle.InputName:=UTF8ToWinCP(Edit_VideoFileName.Caption);
end;

procedure TForm_VideoToggler.FormDestroy(Sender: TObject);
begin
  PasLibVlcPlayer_Preview.Stop;
  TimelineToggle.Free;
end;

procedure TForm_VideoToggler.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  if length(FileNames)<>1 then exit;
  Edit_VideoFileName.Caption:=FileNames[0];
  TimelineToggle.Clear;
  TimelineToggle.MinPosition:=0;
  TimelineToggle.MaxPosition:=100;
  TimelineToggle.OnUserChangeCursorPos:=@UserChangeCursorPos;
  case PasLibVlcPlayer_Preview.GetState of
    plvPlayer_Playing:PasLibVlcPlayer_Preview.Stop;
    plvPlayer_Ended,plvPlayer_NothingSpecial:;
    plvPlayer_Paused:PasLibVlcPlayer_Preview.Stop;
    else ;
  end;
  PasLibVlcPlayer_Preview.Play(WideString(Utf8ToAnsi(Edit_VideoFileName.Text)));
end;

procedure TForm_VideoToggler.PasLibVlcPlayer_PreviewMediaPlayerPositionChanged(
  Sender: TObject; aposition: Single);
var ms:int64;
begin
  ms:=PasLibVlcPlayer_Preview.GetVideoPosInMs;
  TimelineToggle.CursorPos:=ms;
  ms:=PasLibVlcPlayer_Preview.GetVideoLenInMs;
  if (ms<>0) and (TimelineToggle.MaxPosition<>ms) then TimelineToggle.MaxPosition:=ms;//MaxPosition为什么一开始为0？
end;

procedure TForm_VideoToggler.Button_AddTickClick(Sender: TObject);
begin
  TimelineToggle.AddTickAtCursorPos;
end;

procedure TForm_VideoToggler.Button_playClick(Sender: TObject);
begin
  case PasLibVlcPlayer_Preview.GetState of
    plvPlayer_Playing:PasLibVlcPlayer_Preview.Pause;
    plvPlayer_Ended,plvPlayer_NothingSpecial:PasLibVlcPlayer_Preview.Play(WideString(Utf8ToAnsi(Edit_VideoFileName.Text)));
    plvPlayer_Paused:PasLibVlcPlayer_Preview.Resume;
    else ;
  end;
end;

procedure TForm_VideoToggler.Button_RunClick(Sender: TObject);
begin
  TimelineToggle.InputName:=UTF8ToWinCP(Edit_VideoFileName.Caption);
  TimelineToggle.OutputName:=UTF8ToWinCP(Edit_VideoOutput.Caption);
  TimelineToggle.Run;
end;

procedure TForm_VideoToggler.Edit_VideoFileNameChange(Sender: TObject);
var filename,fileext:string;
begin
  TimelineToggle.InputName:=UTF8ToWinCP(Edit_VideoFileName.Caption);
  filename:=ExtractFileNameWithoutExt(Edit_VideoFileName.Caption);
  fileext:=ExtractFileExt(Edit_VideoFileName.Caption);
  Edit_VideoOutput.Caption:=filename+'_out'+fileext;
end;

procedure TForm_VideoToggler.UserChangeCursorPos(ACursorPos:TTimelineTickPos);
begin
  PasLibVlcPlayer_Preview.SetVideoPosInMs(ACursorPos);
end;


end.

