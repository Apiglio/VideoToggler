unit timeline_toggle;

{$mode objfpc}{$H+}
{$inline on}
{$define test_mode}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Menus, Graphics, Windows, Dialogs, LazUTF8;

type

  TTimelineTickPos = Integer;
  TTimelineCursorChangeEvent = procedure(ACursorPos:TTimelineTickPos) of object;
  TTimelineZoneEvent = procedure(ADispMin,ADispMax:TTimelineTickPos) of object;

  TTimelineToggleTick = class
  public
    Caption:string;
    Position:TTimelineTickPos;
  end;

  TTimelineToggleSegment = class
    Enabled:boolean;
    Speed:double;
    Left,Right:TTimelineToggleTick;
  public
    procedure XorEnabled;{$ifndef test_mode}inline;{$endif}
  public
    constructor Create;
  end;

  TTimelineToggle = class(TPanel)
  private
    FMin,FMax:TTimelineTickPos;
    FDisplayMin,FDisplayMax:TTimelineTickPos;
    FTicks:TList;
    FSegments:TList;
    FCursorPos:TTimelineTickPos;
    FCursorPosPicked:TTimelineTickPos;//在鼠标事件中用于存储临时的游标
    FEditable:boolean;//是否可以通过用户操作修改时间轴

    FBoundary:Integer;//边界像素大小
    FTimeLineTop:Integer;//时间轴区域上缘的坐标
    FRightBound:Integer;//右边额外的空间
    FEnabledColor:TColor;//启用区段的时间轴颜色
    FDisabledColor:TColor;//禁用区段的时间轴颜色
    FTickCaptionStyle:TTextStyle;//时间标记的对齐方式等

  private
    FRightButtonMenu:TPopupMenu;//右键菜单
    procedure PopupClick_XOR_SEGMENT(Sender:TObject);
    procedure PopupClick_ADD_TICK(Sender:TObject);
    procedure PopupClick_DEL_LTICK(Sender:TObject);
    procedure PopupClick_DEL_RTICK(Sender:TObject);
    procedure PopupClick_EDIT_SPEED(Sender:TObject);

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
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure Clear;

  public
    function AddTick(position:TTimelineTickPos):integer;
    function DelTick(Index:Integer;left_merge:boolean):boolean;
    function DelTick(tick:TTimelineToggleTick;left_merge:boolean):boolean;

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
  public
    function Valid:boolean;
    function ValidCursor(cursor_pos:TTimelineTickPos):boolean;
    property Editable:boolean read FEditable write FEditable;

  private
    FOnUserChangeCursorPos:TTimelineCursorChangeEvent;
    FOnUserZone:TTimelineZoneEvent;
  public
    procedure Refresh;
    function Zone(ADispMin,ADispMax:TTimelineTickPos):boolean;
    procedure ZoneToWorld;
    property OnUserChangeCursorPos:TTimelineCursorChangeEvent read FOnUserChangeCursorPos write FOnUserChangeCursorPos;
    property OnUserZone:TTimelineZoneEvent read FOnUserZone write FOnUserZone;

  //对接视频剪辑的部分
  private
    FInputName:string;
    FOutputName:string;
  public
    property InputName:string read FInputName write FInputName;
    property OutputName:string read FOutputName write FOutputName;
  public
    procedure Run;

  end;

implementation

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

function millisec_to_format(ms:integer):string;
var a,b:integer;
begin
  if ms>=0 then begin
    result:='';
    a:=ms;
  end else begin
    result:='-';
    a:=-ms;
  end;
  result:=result+zfill(a div 3600000,1);
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

constructor TTimelineToggleSegment.Create;
begin
  inherited Create;
  Speed:=1.0;
  Enabled:=true;
end;


{ TTimelineToggle }


procedure TTimelineToggle.PopupClick_XOR_SEGMENT(Sender:TObject);
var tmpSegment:TTimelineToggleSegment;
begin
  with ((Sender as TMenuItem).Owner as TPopupMenu).Owner do
  begin
    tmpSegment:=TimelinePosToSegment(FCursorPosPicked);
    if tmpSegment<>nil then tmpSegment.XorEnabled;
    Paint;
  end;
end;

procedure TTimelineToggle.PopupClick_ADD_Tick(Sender:TObject);
begin
  with ((Sender as TMenuItem).Owner as TPopupMenu).Owner do
  begin
    AddTick(FCursorPosPicked);
    Paint;
  end;
end;

procedure TTimelineToggle.PopupClick_DEL_LTick(Sender:TObject);
var tmpSegment:TTimelineToggleSegment;
begin
  with ((Sender as TMenuItem).Owner as TPopupMenu).Owner do
  begin
    tmpSegment:=TimelinePosToSegment(FCursorPosPicked);
    if tmpSegment<>nil then
    begin
      DelTick(tmpSegment.Left,false);
      Paint;
    end;
  end;
end;

procedure TTimelineToggle.PopupClick_DEL_RTick(Sender:TObject);
var tmpSegment:TTimelineToggleSegment;
begin
  with ((Sender as TMenuItem).Owner as TPopupMenu).Owner do
  begin
    tmpSegment:=TimelinePosToSegment(FCursorPosPicked);
    if tmpSegment<>nil then
    begin
      DelTick(tmpSegment.Right,true);
      Paint;
    end;
  end;
end;

procedure TTimelineToggle.PopupClick_EDIT_SPEED(Sender:TObject);
var tmpSegment:TTimelineToggleSegment;
    stmp:string;
    dtmp:double;
begin
  with ((Sender as TMenuItem).Owner as TPopupMenu).Owner do
  begin
    tmpSegment:=TimelinePosToSegment(FCursorPosPicked);
    if tmpSegment<>nil then
    begin
      stmp:=InputBox('速度：','修改片段速度',FloatToStrF(tmpSegment.Speed,ffFixed,5,8));
      try
        dtmp:=StrToFloat(stmp);
        tmpSegment.Speed:=dtmp;
      except
        ShowMessage('错误的速度格式！');
      end;
    end;
    Paint;
  end;
end;


function TTimelineToggle.TimelinePosToPixelPos(Timeline:TTimelineTickPos):Integer;
begin
  result:=(Width-2*FBoundary-FRightBound)*(Timeline-FDisplayMin) div (FDisplayMax-FDisplayMin) + FBoundary;
end;

function TTimelineToggle.PixelPosToTimelinePos(PixelPos:Integer):TTimelineTickPos;
begin
  result:=(FDisplayMax-FDisplayMin)*(PixelPos-FBoundary) div (Width-2*FBoundary-FRightBound) + FDisplayMin;
end;

function TTimelineToggle.SegmentToRect(Segment:TTimelineToggleSegment):TRect;
begin
  with result do begin
    Top:=FBoundary;
    Left:=(Self.Width-2*FBoundary-FRightBound)*(Segment.Left.Position-FDisplayMin) div (FDisplayMax-FDisplayMin) + FBoundary;
    Right:=Left+(Self.Width-2*FBoundary-FRightBound)*(Segment.Right.Position - Segment.Left.Position) div (FDisplayMax-FDisplayMin);
    Bottom:=Top+16;
  end;
end;

function TTimelineToggle.TimelinePosToSegment(Timeline:TTimelineTickPos):TTimelineToggleSegment;
var pi:integer;
begin
  result:=nil;
  pi:=0;
  while pi<FTicks.Count do
  begin
    if Timeline=Ticks[pi].Position then exit;
    if Timeline<Ticks[pi].Position then break;
    inc(pi);
  end;
  if pi>=FTicks.Count then exit;
  result:=Segments[pi-1];
end;

procedure TTimelineToggle.Paint;
var pi,tlt,tlb,tll,tlr,tmpx:integer;
    pos_l,pos_r:integer;
    first_tick,last_tick:integer;
begin
  inherited Paint;

  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=clForm;
  Canvas.Clear;

  tlt:=FTimeLineTop;
  tlb:=Height-FBoundary;
  tll:=Self.FBoundary;
  tlr:=Self.Width-FBoundary-FRightBound;

  if not Valid then exit;

  //segments
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=clForm;
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Width:=1;
  for pi:=0 to FSegments.Count-1 do
  begin
    with Segments[pi] do
    begin
      pos_l:=TimelinePosToPixelPos(Left.Position);
      pos_r:=TimelinePosToPixelPos(Right.Position);
      if pos_r<tll then continue;
      if pos_l>tlr then continue;
      if pos_l<tll then pos_l:=tll;
      if pos_r>tlr then pos_r:=tlr;
      if Enabled then Canvas.Brush.Color:=FEnabledColor else Canvas.Brush.Color:=FDisabledColor;
      Canvas.Rectangle(pos_l,tlt,pos_r,tlb);
    end;
  end;

  //ticks
  Canvas.Font.Color:=clBlack;
  Canvas.Brush.Style:=bsClear;
  Canvas.Pen.Width:=0;
  first_tick:=0;
  last_tick:=FTicks.Count-1;
  while first_tick<=last_tick do
  begin
    pos_l:=TimelinePosToPixelPos(Ticks[first_tick].Position);
    if pos_l>tll then break;
    inc(first_tick);
  end;
  while last_tick>=first_tick do
  begin
    pos_r:=TimelinePosToPixelPos(Ticks[last_tick].Position);
    if pos_r<tlr then break;
    dec(last_tick);
  end;
  Canvas.TextRect(SegmentToRect(Segments[first_tick-1]),tll,FBoundary,millisec_to_format(FDisplayMin),FTickCaptionStyle);
  Canvas.TextOut(tlr,FBoundary,millisec_to_format(FDisplayMax));
  for pi:=first_tick to last_tick do
  begin
    with Ticks[pi] do
    begin
      tmpx:=TimelinePosToPixelPos(Position);
      if tmpx<tll then continue;
      if tmpx>tlr then continue;
      Canvas.Line(tmpx,tlt-8,tmpx,tlt);
      if pi<>FTicks.Count-1 then
        Canvas.TextRect(SegmentToRect(Segments[pi]),tmpx,FBoundary,Caption,FTickCaptionStyle)
      else
        Canvas.TextOut(tmpx,FBoundary,Caption);
    end;
  end;
  Canvas.TextOut(Width-FBoundary-FRightBound,Height-FBoundary-16,millisec_to_format(FCursorPos));

  //cursor
  Canvas.Pen.Color:=clRed;
  Canvas.Pen.Width:=2;
  tmpx:=TimelinePosToPixelPos(FCursorPos);
  if (tmpx>=tll) and (tmpx<=tlr) then Canvas.Line(tmpx,tlt,tmpx,tlb-1);

end;

procedure TTimelineToggle.MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
  if not Valid then exit;
  if not (ssLeft in Shift) then exit;
  FCursorPosPicked:=PixelPosToTimelinePos(X);
  if ValidCursor(FCursorPosPicked) then SetCursorPos(FCursorPosPicked);
  if FOnUserChangeCursorPos<>nil then FOnUserChangeCursorPos(FCursorPosPicked);
  Inherited MouseDown(Button,Shift,X,Y);
end;

procedure TTimelineToggle.MouseUp(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
  if not Valid then exit;
  FCursorPosPicked:=PixelPosToTimelinePos(X);
  if not ValidCursor(FCursorPosPicked) then exit;
  if Button=mbLeft then begin
    SetCursorPos(FCursorPosPicked);
    if FOnUserChangeCursorPos<>nil then FOnUserChangeCursorPos(FCursorPosPicked);
  end;
  if Button=mbRight then begin
    if FEditable then FRightButtonMenu.PopUp;
  end;
  Inherited MouseUp(Button,Shift,X,Y);
end;

procedure TTimelineToggle.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not Valid then exit;
  if not (ssLeft in Shift) then exit;
  FCursorPosPicked:=PixelPosToTimelinePos(X);
  if ValidCursor(FCursorPosPicked) then SetCursorPos(FCursorPosPicked);
  if FOnUserChangeCursorPos<>nil then FOnUserChangeCursorPos(FCursorPosPicked);
  Inherited MouseMove(Shift,X,Y);
end;

procedure TTimelineToggle.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var select_timepos:TTimelineTickPos;
    prev,next:longint;
begin
  if not Valid then exit;
  select_timepos:=PixelPosToTimelinePos(MousePos.x);
  prev:=select_timepos-FDisplayMin;
  next:=FDisplayMax-select_timepos;
  if WheelDelta>0 then begin
    if FDisplayMax - FDisplayMin < 200 then exit;
    prev:=round(prev*0.8);
    next:=round(next*0.8);
  end else begin
    prev:=round(prev*1.25);
    next:=round(next*1.25);
  end;
  prev:=select_timepos - prev;
  next:=select_timepos + next;
  if prev<=FMin then FDisplayMin:=FMin else FDisplayMin:=prev;
  if next>=FMax then FDisplayMax:=FMax else FDisplayMax:=next;
  if FOnUserZone<>nil then FOnUserZone(FDisplayMin,FDisplayMax);
  Paint;
end;

constructor TTimelineToggle.Create(AOwner:TComponent);
var tmpTick1,tmpTick2:TTimelineToggleTick;
    tmpSegment:TTimelineToggleSegment;
    tmpMenuItem:TMenuItem;
begin
  inherited Create(AOwner);

  FRightButtonMenu:=TPopupMenu.Create(Self);
  FRightButtonMenu.Parent:=Self;
  FRightButtonMenu.PopupComponent:=Self;

  tmpMenuItem:=TMenuItem.create(FRightButtonMenu);
  tmpMenuItem.Caption:='禁用/启用';
  tmpMenuItem.Enabled:=true;
  tmpMenuItem.OnClick:=@PopupClick_XOR_SEGMENT;
  FRightButtonMenu.Items.Add(tmpMenuItem);

  tmpMenuItem:=TMenuItem.create(FRightButtonMenu);
  tmpMenuItem.Caption:='-';
  tmpMenuItem.Enabled:=false;
  tmpMenuItem.OnClick:=nil;
  FRightButtonMenu.Items.Add(tmpMenuItem);

  tmpMenuItem:=TMenuItem.create(FRightButtonMenu);
  tmpMenuItem.Caption:='添加断点';
  tmpMenuItem.Enabled:=true;
  tmpMenuItem.OnClick:=@PopupClick_ADD_TICK;
  FRightButtonMenu.Items.Add(tmpMenuItem);

  tmpMenuItem:=TMenuItem.create(FRightButtonMenu);
  tmpMenuItem.Caption:='移除左断点';
  tmpMenuItem.Enabled:=true;
  tmpMenuItem.OnClick:=@PopupClick_DEL_LTICK;
  FRightButtonMenu.Items.Add(tmpMenuItem);

  tmpMenuItem:=TMenuItem.create(FRightButtonMenu);
  tmpMenuItem.Caption:='移除右断点';
  tmpMenuItem.Enabled:=true;
  tmpMenuItem.OnClick:=@PopupClick_DEL_RTICK;
  FRightButtonMenu.Items.Add(tmpMenuItem);

  tmpMenuItem:=TMenuItem.create(FRightButtonMenu);
  tmpMenuItem.Caption:='-';
  tmpMenuItem.Enabled:=false;
  tmpMenuItem.OnClick:=nil;
  FRightButtonMenu.Items.Add(tmpMenuItem);

  tmpMenuItem:=TMenuItem.create(FRightButtonMenu);
  tmpMenuItem.Caption:='片段速度';
  tmpMenuItem.Enabled:=true;
  tmpMenuItem.OnClick:=@PopupClick_EDIT_SPEED;
  FRightButtonMenu.Items.Add(tmpMenuItem);


  FOnUserChangeCursorPos:=nil;

  FBoundary:=8;
  FTimeLineTop:=36;
  FRightBound:=64;

  FEnabledColor:=$ddddff;//$eeffee;
  FDisabledColor:=$dddddd;//$eeeeff;
  with FTickCaptionStyle do begin
    Alignment:=taLeftJustify;
    Clipping:=true;
    Wordbreak:=true;
  end;

  FMin:=0;
  FMax:=0;
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
  FSegments.Add(tmpSegment);

  Self.OnMouseWheel:=@MouseWheel;
  FEditable:=true;

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

procedure TTimelineToggle.Clear;
var seg:TTimelineToggleSegment;
    tick,tick2:TTimelineToggleTick;
begin
  while FSegments.Count>0 do begin
    seg:=TTimelineToggleSegment(FSegments.Items[0]);
    seg.Free;
    FSegments.Delete(0);
  end;
  while FTicks.Count>0 do begin
    tick:=TTimelineToggleTick(FTicks.Items[0]);
    tick.Free;
    FTicks.Delete(0);
  end;

  FMin:=0;
  FMax:=0;
  FCursorPos:=0;

  FTicks:=TList.Create;
  tick:=TTimelineToggleTick.Create;
  tick.Position:=FMin;
  tick.Caption:=millisec_to_format(FMin);
  FTicks.Add(tick);
  tick2:=TTimelineToggleTick.Create;
  tick2.Position:=FMax;
  tick2.Caption:=millisec_to_format(FMax);
  FTicks.Add(tick2);
  FSegments:=TList.Create;
  seg:=TTimelineToggleSegment.Create;
  seg.Left:=tick;
  seg.Right:=tick2;
  FSegments.Add(seg);

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

function TTimelineToggle.DelTick(Index:Integer;left_merge:boolean):boolean;
var seg_1,seg_2:TTimelineToggleSegment;
    tick:TTimelineToggleTick;
begin
  result:=false;
  if Index<=0 then exit;
  if Index>=FTicks.Count-1 then exit;
  tick:=TTimelineToggleTick(FTicks.Items[Index]);
  seg_1:=TTimelineToggleSegment(FSegments.Items[Index-1]);
  seg_2:=TTimelineToggleSegment(FSegments.Items[Index]);
  if left_merge then
  begin
    seg_1.Right:=seg_2.Right;
    FSegments.Delete(Index);
    seg_2.Free;
  end else begin
    seg_2.Left:=seg_1.Left;
    FSegments.Delete(Index-1);
    seg_1.Free;
  end;
  FTicks.Delete(Index);
  tick.Free;
  Paint;
  result:=true;
end;

function TTimelineToggle.DelTick(tick:TTimelineToggleTick;left_merge:boolean):boolean;
begin
  DelTick(FTicks.IndexOf(tick),left_merge);

end;

procedure TTimelineToggle.AddTickAtCursorPos;
begin
  AddTick(FCursorPos);
end;

function TTimelineToggle.GetSegment(Index:Integer):TTimelineToggleSegment;
begin
  result:=nil;
  if (Index>=FSegments.Count) or (Index<-FSegments.Count) then exit;
  if Index>=0 then
    result:=TTimelineToggleSegment(FSegments.Items[Index])
  else
    result:=TTimelineToggleSegment(FSegments.Items[Index+FSegments.Count])
end;

function TTimelineToggle.GetTick(Index:Integer):TTimelineToggleTick;
begin
  result:=nil;
  if (Index>=FTicks.Count) or (Index<-FTicks.Count) then exit;
  if Index>=0 then
    result:=TTimelineToggleTick(FTicks.Items[Index])
  else
    result:=TTimelineToggleTick(FTicks.Items[Index+FTicks.Count]);
end;

procedure TTimelineToggle.SetMin(value:TTimelineTickPos);
begin
  if FSegments.Count<>1 then raise Exception.Create('Segment数量不为1，不能修改边界值。');
  FMin:=value;
  FDisplayMin:=value;
  Segments[0].Left.Position:=value;
  Segments[0].Left.Caption:=millisec_to_format(value);
end;

procedure TTimelineToggle.SetMax(value:TTimelineTickPos);
begin
  if FSegments.Count<>1 then raise Exception.Create('Segment数量不为1，不能修改边界值。');
  FMax:=value;
  FDisplayMax:=value;
  Segments[0].Right.Position:=value;
  Segments[0].Right.Caption:=millisec_to_format(value);
end;

procedure TTimelineToggle.SetCursorPos(value:TTimelineTickPos);
var step,prev,next,nmax,nmin:integer;
begin
  if (FCursorPos>FMax) or (FCursorPos<FMin) then raise Exception.Create('CursorPos值超出边界值。');
  //计划加上播放时的范围平移
  FCursorPos:=value;
  Paint;
end;

function TTimelineToggle.Valid:boolean;
begin
  result:=FMax>FMin;
end;
function TTimelineToggle.ValidCursor(cursor_pos:TTimelineTickPos):boolean;
begin
  result:=(cursor_pos>=FMin) and (cursor_pos<=FMax);
end;

procedure TTimelineToggle.Refresh;
begin
  if Valid then Paint;
end;

function TTimelineToggle.Zone(ADispMin,ADispMax:TTimelineTickPos):boolean;
begin
  if (FMin <= ADispMin) and (ADispMin < ADispMax) and (ADispMax <= FMax) then begin
    FDisplayMin:=ADispMin;
    FDisplayMax:=ADispMax;
    Paint;
    result:=true;
  end else
    result:=false;
end;

procedure TTimelineToggle.ZoneToWorld;
begin
  FDisplayMin:=FMin;
  FDisplayMax:=FMax;
end;

function arg_atempo(multiply:double):string;
var dtmp:double;
begin
  if multiply<=0 then exit;
  dtmp:=multiply;
  result:=' -af "';
  while dtmp>2 do
  begin
    result:=result+'atempo=2.0,';
    dtmp:=dtmp/2;
  end;
  while dtmp<0.5 do
  begin
    result:=result+'atempo=0.5,';
    dtmp:=dtmp*2;
  end;
  result:=result+'atempo='+FloatToStrF(dtmp,ffFixed,3,8)+'"';
end;

//.\ffmpeg.exe -i foochow.mp4 -ss 00:00:00 -vframes 1 out.png
//.\ffmpeg -i foochow.mp4 -i out.png -map 0 -map 1 -c copy -c:v:1 png -disposition:v:1 attached_pic test.mp4
//.\ffmpeg.exe -y -i Foochow.mp4 -ss 0:0:15 -to 0:0:17 out3.ts
//.\ffmpeg.exe -i "concat:out1.ts|out2.ts|out3.ts" -c copy output.mp4
procedure TTimelineToggle.Run;
var seg:TTimelineToggleSegment;
    pi,ts:integer;
    cmd:string;
    thumb_pos:TTimelineTickPos;
    batch_lines:TStringlist;


begin
  if not Valid then exit;

  batch_lines:=TStringlist.Create;
  try
    batch_lines.add('setlocal');

    //clip
    pi:=0;
    ts:=0;
    thumb_pos:=-1;
    while pi<FSegments.Count do begin
      seg:=TTimelineToggleSegment(FSegments.Items[pi]);
      if seg.Enabled then begin
        if thumb_pos<0 then thumb_pos:=seg.Left.Position;
        cmd:='-y';
        cmd:=cmd+' -ss '+millisec_to_format(seg.Left.Position);
        cmd:=cmd+' -to '+millisec_to_format(seg.Right.Position);
        //cmd:=cmd+' -t '+millisec_to_format(round((seg.Right.Position-seg.Left.Position)/seg.Speed));
        cmd:=cmd+' -i "'+FInputName+'"';
        if seg.Speed<>1.0 then begin
          cmd:=cmd+' -vf "setpts='+FloatToStrF(1.0/seg.Speed,ffFixed,3,8)+'*PTS"';
          cmd:=cmd+arg_atempo(seg.Speed);
        end else begin
          cmd:=cmd+' -c copy';
        end;
        cmd:=cmd+' OutTemp_'+IntToStr(ts)+'.ts';
        batch_lines.add('.\ffmpeg.exe '+cmd);
        inc(ts);
      end;
      inc(pi);
    end;
    if ts=0 then exit;

    //thumbnail
    batch_lines.add('.\ffmpeg.exe -y -ss '+millisec_to_format(thumb_pos)+' -i "'+FInputName+'" -vframes 1 OutTemp_Thumb.png');

    //concat
    cmd:='concat:OutTemp_0.ts';
    for pi:=1 to ts-1 do cmd:=cmd+'|OutTemp_'+IntToStr(pi)+'.ts';
    batch_lines.add('.\ffmpeg.exe -y -i "'+cmd+'" -i OutTemp_Thumb.png -map 0 -map 1 -c copy -c:v:1 png -disposition:v:1 attached_pic Output.mp4');

    //delete temporary file
    for pi:=0 to ts-1 do
      batch_lines.add('del OutTemp_'+IntToStr(pi)+'.ts');
    batch_lines.add('del OutTemp_Thumb.png');

    //rename output
    batch_lines.add('move Output.mp4 "'+FOutputName+'"');

    //run batch file
    batch_lines.SaveToFile('video_toggler_gen.bat');
    ShellExecute(0,'open','cmd.exe','/c call "video_toggler_gen.bat"',pchar(ExtractFileDir(ParamStr(0))),SW_HIDE);

  finally
    batch_lines.Free;
  end;


end;

end.

