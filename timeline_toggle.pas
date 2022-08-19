unit timeline_toggle;

{$mode objfpc}{$H+}
{$inline on}
{$define test_mode}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Graphics;

type

  TTimelineTickPos = Integer;
  TTimelineCursorChangeEvent = procedure(ACursorPos:TTimelineTickPos) of object;

  TTimelineToggleTick = class
  public
    Caption:string;
    Position:TTimelineTickPos;
  end;

  TTimelineToggleSegment = class
    Enabled:boolean;
    Left,Right:TTimelineToggleTick;
  public
    procedure XorEnabled;{$ifndef test_mode}inline;{$endif}
  end;

  TTimelineToggle = class(TPanel)
  private
    FMin,FMax:TTimelineTickPos;
    FTicks:TList;
    FSegments:TList;
    FCursorPos:TTimelineTickPos;

    FBoundary:Integer;//边界像素大小
    FTimeLineTop:Integer;//时间轴区域上缘的坐标
    FRightBound:Integer;//右边额外的空间
    FEnabledColor:TColor;//启用区段的时间轴颜色
    FDisabledColor:TColor;//禁用区段的时间轴颜色
    FTickCaptionStyle:TTextStyle;//时间标记的对齐方式等


  private
    function TimelinePosToPixelPos(Timeline:TTimelineTickPos):Integer;{$ifndef test_mode}inline;{$endif}
    function PixelPosToTimelinePos(PixelPos:Integer):TTimelineTickPos;{$ifndef test_mode}inline;{$endif}
    function SegmentToRect(Segment:TTimelineToggleSegment):TRect;{$ifndef test_mode}inline;{$endif}
    function TimelinePosToSegment(Timeline:TTimelineTickPos):TTimelineToggleSegment;{$ifndef test_mode}inline;{$endif}
  private
    procedure Paint;override;
    procedure MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);override;
    procedure MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
  public
    function AddTick(position:TTimelineTickPos):integer;
    procedure DelTick(position:TTimelineTickPos);

  public
    procedure AddTickAtCursorPos;

  protected
    function GetTick(Index:Integer):TTimelineToggleTick;
    function GetSegment(Index:Integer):TTimelineToggleSegment;
    procedure SetMin(value:TTimelineTickPos);
    procedure SetMax(value:TTimelineTickPos);
    procedure SetCursorPos(value:TTimelineTickPos);
  public
    property MinPosition:TTimelineTickPos read FMin write SetMin;
    property MaxPosition:TTimelineTickPos read FMax write SetMax;
    property Ticks[Index:Integer]:TTimelineToggleTick read GetTick;
    property Segments[Index:Integer]:TTimelineToggleSegment read GetSegment;default;
    property CursorPos:TTimelineTickPos read FCursorPos write SetCursorPos;
  private
    FOnUserChangeCursorPos:TTimelineCursorChangeEvent;
  public
    property OnUserChangeCursorPos:TTimelineCursorChangeEvent read FOnUserChangeCursorPos write FOnUserChangeCursorPos;
  end;

implementation

function millisec_to_format(ms:integer):string;
var a,b:integer;
  function zfill(value:integer;digit:integer):string;
  var len:integer;
  begin
    result:=IntToStr(value);
    len:=digit-length(result);
    while len>0 do begin
      result:='0'+result;
      dec(len);
    end;
  end;
begin
  a:=ms;
  result:=zfill(a div 3600000,1);
  a:=a mod 3600000;
  result:=result+':'+zfill(a div 60000,2);
  a:=a mod 60000;
  result:=result+':'+zfill(a div 1000,2);
  a:=a mod 1000;
  result:=result+'.'+zfill(a,3);
end;

{ TTimelineToggleSegment }
procedure TTimelineToggleSegment.XorEnabled;
begin
  Enabled:=not Enabled;
end;

{ TTimelineToggle }

function TTimelineToggle.TimelinePosToPixelPos(Timeline:TTimelineTickPos):Integer;
begin
  result:=(Width-2*FBoundary-FRightBound)*(Timeline-FMin) div (FMax-FMin) + FBoundary;
end;
function TTimelineToggle.PixelPosToTimelinePos(PixelPos:Integer):TTimelineTickPos;
begin
  result:=(FMax-FMin)*(PixelPos-FBoundary) div (Width-2*FBoundary-FRightBound) + FMin;
end;
function TTimelineToggle.SegmentToRect(Segment:TTimelineToggleSegment):TRect;
begin
  with result do begin
    Top:=FBoundary;
    Left:=(Self.Width-2*FBoundary-FRightBound)*(Segment.Left.Position-FMin) div (FMax-FMin) + FBoundary;
    Right:=Left+(Self.Width-2*FBoundary-FRightBound)*(Segment.Right.Position - Segment.Left.Position) div (FMax-FMin);
    Bottom:=Top+16;
  end;
end;

function TTimelineToggle.TimelinePosToSegment(Timeline:TTimelineTickPos):TTimelineToggleSegment;
var pi:integer;
begin
  pi:=0;
  while pi<FTicks.Count do
  begin
    if Timeline=Ticks[pi].Position then begin result:=nil;exit end;
    if Timeline<Ticks[pi].Position then break;
    inc(pi);
  end;
  result:=Segments[pi-1];
end;

procedure TTimelineToggle.Paint;
var pi,tlt,tlb,tmpx:integer;
begin
  inherited Paint;
  //为什么文字越描越粗？
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=clForm;
  Canvas.Clear;

  tlt:=FTimeLineTop;
  tlb:=Height-FBoundary;

  //segments
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=clForm;
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Width:=1;
  for pi:=0 to FSegments.Count-1 do
  begin
    with Segments[pi] do
    begin
      if Enabled then Canvas.Brush.Color:=FEnabledColor else Canvas.Brush.Color:=FDisabledColor;
      Canvas.Rectangle(
        TimelinePosToPixelPos(Left.Position),
        tlt,
        TimelinePosToPixelPos(Right.Position),
        tlb
      );
    end;
  end;

  //ticks
  Canvas.Font.Color:=clBlack;
  Canvas.Brush.Style:=bsClear;
  Canvas.Pen.Width:=0;
  for pi:=0 to FTicks.Count-1 do
  begin
    with Ticks[pi] do
    begin
      tmpx:=TimelinePosToPixelPos(Position);
      Canvas.Line(tmpx,tlt-8,tmpx,tlt);
      if pi<>FTicks.Count-1 then Canvas.TextRect(SegmentToRect(Segments[pi]),TimelinePosToPixelPos(Position),FBoundary,Caption,FTickCaptionStyle)
      else Canvas.TextOut(TimelinePosToPixelPos(Position),FBoundary,Caption);
    end;
  end;
  Canvas.TextOut(Width-FBoundary-FRightBound,Height-FBoundary-16,millisec_to_format(FCursorPos));

  //cursor
  Canvas.Pen.Color:=clRed;
  Canvas.Pen.Width:=2;
  tmpx:=TimelinePosToPixelPos(FCursorPos);
  Canvas.Line(tmpx,tlt,tmpx,tlb-1);

end;

procedure TTimelineToggle.MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
var cur:TTimelineTickPos;
begin
  if not (ssLeft in Shift) then exit;
  cur:=PixelPosToTimelinePos(X);
  if (cur>=FMin) and (cur<=FMax) then SetCursorPos(cur);
  if FOnUserChangeCursorPos<>nil then FOnUserChangeCursorPos(cur);
  Inherited MouseDown(Button,Shift,X,Y);
end;

procedure TTimelineToggle.MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
var cur:TTimelineTickPos;
    tmpSegment:TTimelineToggleSegment;
begin
  if Button=mbLeft then begin
    cur:=PixelPosToTimelinePos(X);
    if (cur>=FMin) and (cur<=FMax) then SetCursorPos(cur);
    if FOnUserChangeCursorPos<>nil then FOnUserChangeCursorPos(cur);
  end;
  if Button=mbRight then begin
    cur:=PixelPosToTimelinePos(X);
    tmpSegment:=nil;
    if (cur>=FMin) and (cur<=FMax) then tmpSegment:=TimelinePosToSegment(cur);
    if tmpSegment<>nil then tmpSegment.XorEnabled;
    Paint;
  end;
  Inherited MouseUp(Button,Shift,X,Y);
end;

procedure TTimelineToggle.MouseMove(Shift: TShiftState; X, Y: Integer);
var cur:TTimelineTickPos;
begin
  if not (ssLeft in Shift) then exit;
  cur:=PixelPosToTimelinePos(X);
  if (cur>=FMin) and (cur<=FMax) then SetCursorPos(cur);
  if FOnUserChangeCursorPos<>nil then FOnUserChangeCursorPos(cur);
  Inherited MouseMove(Shift,X,Y);
end;

constructor TTimelineToggle.Create(AOwner:TComponent);
var tmpTick1,tmpTick2:TTimelineToggleTick;
    tmpSegment:TTimelineToggleSegment;
begin
  inherited Create(AOwner);

  FOnUserChangeCursorPos:=nil;

  FBoundary:=8;
  FTimeLineTop:=36;
  FRightBound:=64;
  //Constraints.MinHeight:=64;//16+48;
  //Constraints.MinWidth:=16;
  FEnabledColor:=$ddddff;//$eeffee;
  FDisabledColor:=$dddddd;//$eeeeff;
  with FTickCaptionStyle do begin
    Alignment:=taLeftJustify;
    Clipping:=true;
    Wordbreak:=true;
  end;

  FMin:=0;
  FMax:=100;
  FCursor:=0;

  FTicks:=TList.Create;
  tmpTick1:=TTimelineToggleTick.Create;
  tmpTick1.Position:=FMin;
  tmpTick1.Caption:=millisec_to_format(FMin);
  FTicks.Add(tmpTick1);
  tmpTick2:=TTimelineToggleTick.Create;
  tmpTick2.Position:=FMax;
  tmpTick2.Caption:=millisec_to_format(FMax);
  FTicks.Add(tmpTick2);
  FSegments:=TList.Create;
  tmpSegment:=TTimelineToggleSegment.Create;
  tmpSegment.Left:=tmpTick1;
  tmpSegment.Right:=tmpTick2;
  tmpSegment.Enabled:=false;
  FSegments.Add(tmpSegment);
end;

destructor TTimelineToggle.Destroy;
begin
  while FSegments.Count>0 do begin
    TTimelineToggleSegment(FSegments.Items[0]).Free;
    FSegments.Delete(0);
  end;
  FSegments.Free;
  while FTicks.Count>0 do begin
    TTimelineToggleTick(FTicks.Items[0]).Free;
    FTicks.Delete(0);
  end;
  FTicks.Free;
  inherited Destroy;
end;

function TTimelineToggle.AddTick(position:TTimelineTickPos):integer;
var pi:integer;
    tmpTick:TTimelineToggleTick;
    tmpSegment:TTimelineToggleSegment;
    Seg_Enabled:boolean;
begin
  pi:=0;
  while pi<FTicks.Count do
  begin
    if position=Ticks[pi].Position then begin result:=pi;exit end;
    if position<Ticks[pi].Position then break;
    inc(pi);
  end;
  tmpTick:=TTimelineToggleTick.Create;
  tmpTick.Position:=position;
  tmpTick.Caption:=millisec_to_format(position);
  if pi=0 then begin
    FMin:=position;
    tmpSegment:=TTimelineToggleSegment.Create;
    tmpSegment.Left:=tmpTick;
    tmpSegment.Right:=Ticks[0];
    tmpSegment.Enabled:=false;
    FSegments.Insert(0,tmpSegment);
  end else if pi=FTicks.Count then begin
    FMax:=position;
    tmpSegment:=TTimelineToggleSegment.Create;
    tmpSegment.Left:=Ticks[pi-1];
    tmpSegment.Right:=tmpTick;
    tmpSegment.Enabled:=false;
    FSegments.Add(tmpSegment);
  end else begin
    tmpSegment:=Segments[pi-1];
    tmpSegment.Left:=tmpTick;
    Seg_Enabled:=tmpSegment.Enabled;
    tmpSegment:=TTimelineToggleSegment.Create;
    tmpSegment.Left:=Ticks[pi-1];
    tmpSegment.Right:=tmpTick;
    tmpSegment.Enabled:=Seg_Enabled;
    FSegments.Insert(pi-1,tmpSegment);
  end;
  FTicks.Insert(pi,tmpTick);
  result:=pi;
  Paint;
end;

procedure TTimelineToggle.DelTick(position:TTimelineTickPos);
begin

end;

procedure TTimelineToggle.AddTickAtCursorPos;
begin
  AddTick(FCursorPos);
end;

function TTimelineToggle.GetSegment(Index:Integer):TTimelineToggleSegment;
begin
  if (Index>=FSegments.Count) or (Index<0) then result:=nil
  else result:=TTimelineToggleSegment(FSegments.Items[Index]);
end;

function TTimelineToggle.GetTick(Index:Integer):TTimelineToggleTick;
begin
  if Index>=FTicks.Count then result:=nil
  else result:=TTimelineToggleTick(FTicks.Items[Index]);
end;

procedure TTimelineToggle.SetMin(value:TTimelineTickPos);
begin
  if FSegments.Count<>1 then raise Exception.Create('Segment数量不为1，不能修改边界值。');
  FMin:=value;
  Segments[0].Left.Position:=value;
  Segments[0].Left.Caption:=millisec_to_format(value);
end;

procedure TTimelineToggle.SetMax(value:TTimelineTickPos);
begin
  if FSegments.Count<>1 then raise Exception.Create('Segment数量不为1，不能修改边界值。');
  FMax:=value;
  Segments[0].Right.Position:=value;
  Segments[0].Right.Caption:=millisec_to_format(value);
end;

procedure TTimelineToggle.SetCursorPos(value:TTimelineTickPos);
begin
  FCursorPos:=value;
  Paint;
end;

end.

