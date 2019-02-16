{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{           Alternative TList implementation             }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZList;

interface

uses Classes, Math;

{$INCLUDE ../Zeos.inc}

{ Items signatures }
const
  ITEM_CLEAR    = 0;
  ITEM_ENABLED  = 1;
  ITEM_FILTERED = 2;
  ITEM_DELETED  = 3;

type

{ TZItemList class }

  TPointerList = array[0..MaxListSize - 1] of PChar;
  PPointerList = ^TPointerList;
  PByte = ^Byte;

  TZItemListSortEvent = function (Item1, Item2: Pointer): Integer of object;
  TZItemListFilterEvent = function (Item: Pointer): Boolean of object;

  TZItemListNotification = (lnAdded, lnExtracted, lnDeleted);

  TZItemList = class(TObject)
  private
  {$IFNDEF LIST_MULTIPLE_BLOCKS}
    FBuffer: PChar;                     // Pointer to items list
  {$ELSE}
    FBuffer: PPointerList;              // Pointer to items list
  {$ENDIF}
    FList: PPointerList;                // Pointer to reference list
    FFillList: PPointerList;            // Pointer to allocated list
    FItemSize: Integer;                 // Size of an item
    FCount: Integer;                    // Count of enabled items
    FFillCount: Integer;                // Count of filled items
    FCapacity: Integer;                 // Capacity of the buffer
    FSorted: Boolean;                   // Is list sorted
    FFiltered: Boolean;                 // Is list filtered
    FFillValue: Byte;
    FOnSort: TZItemListSortEvent;       // Callback to sorting function
    FOnFilter: TZItemListFilterEvent;   // Callback to filtering function
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);

    property FillList: PPointerList read FFillList write FFillList;
    property List: PPointerList read FList write FList;
    property ItemSize: Integer read FItemSize write FItemSize;
  public
    constructor Create(ItemSize: Integer);
    destructor Destroy; override;

    class procedure Error(const Msg: string; Data: Integer); virtual;

    function First: Pointer;
    function Last: Pointer;
    function IndexOf(Item: Pointer): Integer;
    function Expand: TZItemList;
    procedure Clear; virtual;

    function Add: Pointer;
    function Delete(Index: Integer): Pointer;
    function Exchange(Index1, Index2: Integer): Pointer;
    function Insert(Index: Integer): Pointer;
    function Move(CurIndex, NewIndex: Integer): Pointer;
    function Remove(Item: Pointer): Integer;

    function GetRealIndex(Item: Pointer): Integer;
    function GetRealItem(Index: Integer): Pointer;

    procedure Sort;
    procedure ClearSort;
    procedure Filter;
    function FilterItem(Index: Integer): Boolean;
    procedure ClearFilter;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write FCount;
    property FillCount: Integer read FFillCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;

    property Sorted: Boolean read FSorted;
    property Filtered: Boolean read FFiltered;
    property FillValue: Byte read FFillValue write FFillValue;

    property OnSort: TZItemListSortEvent read FOnSort write FOnSort;
    property OnFilter: TZItemListFilterEvent read FOnFilter write FOnFilter;
  end;

implementation

{*********** TZItemList class implementation ***************}

{ Class constructor }
constructor TZItemList.Create(ItemSize: Integer);
begin
  inherited Create;
  FItemSize := ItemSize + 1;
end;

{ Class destructor }
destructor TZItemList.Destroy;
begin
  Clear;
end;

{ Get the first item }
function TZItemList.First: Pointer;
begin
  Result := Get(0);
end;

{ Get the last item }
function TZItemList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

{ Safe call of the item }
function TZItemList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error('List Index Error at %d', Index);
  Result := FList^[Index];
end;

{ Assign a new item value }
procedure TZItemList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error('List Index Error at %d', Index);
  System.Move(Item^, FList^[Index]^, FItemSize-1);
end;

{ Resize buffer capacity with new value }
procedure TZItemList.Grow;
var
  Delta: Integer;
begin
{$IFNDEF LIST_MULTIPLE_BLOCKS}
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  if Delta > LIST_BLOCK_SIZE then
    Delta := LIST_BLOCK_SIZE;
{$ELSE}
  Delta := LIST_BLOCK_SIZE;
{$ENDIF}
  SetCapacity(FCapacity + Delta);
end;

{ Calling exception }
class procedure TZItemList.Error(const Msg: string; Data: Integer);
  function ReturnAddr: Pointer;
  asm
    MOV EAX,[EBP+4]
  end;
begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

{ Quick sorting of poiter list }
procedure QuickSort(SortList: PPointerList; L, R: Integer;
  Compare: TZItemListSortEvent);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while Compare(SortList^[I], P) < 0 do
        Inc(I);
      while Compare(SortList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, Compare);
    L := I;
  until I >= R;
end;

{ List sorting }
procedure TZItemList.Sort;
begin
  if (FList <> nil) and (Count > 0) and Assigned(FOnSort) then
    QuickSort(FList, 0, Count - 1, FOnSort);
  FSorted := True;
end;

{ Clear sorting }
procedure TZItemList.ClearSort;
var
  I: Integer;
begin
  FCount := 0;
  FSorted := False;
  for I := 0 to FFillCount-1 do
  begin
    if PByte(FFillList^[I])^ = ITEM_ENABLED then
    begin
      FList^[FCount] := FFillList^[I] + 1;
      Inc(FCount);
    end;
  end;
end;

{ Filter records }
procedure TZItemList.Filter;
var
  I: Integer;
  Ptr: PChar;
begin
  if not Assigned(OnFilter) then Exit;

  FCount := 0;
  FFiltered := True;
  for I := 0 to FFillCount-1 do
  begin
    Ptr := FFillList^[I];
    if PByte(Ptr)^ <> ITEM_DELETED then
    begin
      if not OnFilter(Ptr + 1) then
        PByte(Ptr)^ := ITEM_FILTERED
      else begin
        PByte(Ptr)^ := ITEM_ENABLED;
        FList^[FCount] := Ptr + 1;
        Inc(FCount);
      end;
    end;
  end;

  if Sorted then Sort;
end;

{ Filter one item }
function TZItemList.FilterItem(Index: Integer): Boolean;
var
  Ptr: PChar;
begin
  if (Index < 0) or (Index >= FCount) then
    Error('List Index Error at %d', Index);
  Result := True;
  if not Assigned(OnFilter) then Exit;

  Result := not OnFilter(FList^[Index]);
  if Result then
  begin
    Ptr := FList^[Index] - 1;
    PByte(Ptr)^ := ITEM_FILTERED;
    Dec(FCount);
    if Index < FCount then
      System.Move(FList^[Index + 1], FList^[Index],
        (FCount - Index) * SizeOf(Pointer));
  end;
end;


{ Clear filtering }
procedure TZItemList.ClearFilter;
var
  I: Integer;
  Ptr: PChar;
begin
  FCount := 0;
  FFiltered := False;
  for I := 0 to FFillCount-1 do
  begin
    Ptr := FFillList^[I];
    if PByte(Ptr)^ = ITEM_FILTERED then
      PByte(Ptr)^ := ITEM_ENABLED;
    if PByte(Ptr)^ = ITEM_ENABLED then
    begin
      FList^[FCount] := Ptr + 1;
      Inc(FCount);
    end;
  end;

  if Sorted then Sort;
end;

{ Clear the list }
procedure TZItemList.Clear;
{$IFDEF LIST_MULTIPLE_BLOCKS}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF LIST_MULTIPLE_BLOCKS}
  if FBuffer <> nil then
    for I := 0 to (FCapacity-1) div LIST_BLOCK_SIZE do
      ReallocMem(FBuffer^[I], 0);
{$ENDIF}
  ReallocMem(FBuffer, 0);
  ReallocMem(FList, 0);
  ReallocMem(FFillList, 0);

  FCapacity := 0;
  FFillCount := 0;
  FCount := 0;
end;

{ Set new capacity of the list }
procedure TZItemList.SetCapacity(NewCapacity: Integer);

  procedure FixList(List: PPointerList; Count: LongInt;
    StartPtr, EndPtr: PChar; Offs: LongInt);
  var
    I: Integer;
  begin
    if Offs = 0 then Exit;
    for I := 0 to Count-1 do
    begin
      if (List^[I] >= StartPtr) and (List^[I] < EndPtr) then
        List^[I] := List^[I] + Offs;
    end;
  end;

var
{$IFDEF LIST_MULTIPLE_BLOCKS}
  I, J, N: Integer;
{$ELSE}
  OldBuffer: PChar;
{$ENDIF}
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error('List Capacity Error at %d', NewCapacity);

  if NewCapacity > FCapacity then
  begin
{$IFDEF LIST_MULTIPLE_BLOCKS}
    N := (NewCapacity div LIST_BLOCK_SIZE) * LIST_BLOCK_SIZE;
    if (NewCapacity mod LIST_BLOCK_SIZE) <> 0 then
      N := N + LIST_BLOCK_SIZE;
    NewCapacity := N;
{$ENDIF}

    { Realloc FList }
    ReallocMem(Pointer(FList), NewCapacity * SizeOf(Pointer));
    FillChar(Pointer(LongInt(FList) + FCapacity * SizeOf(Pointer))^,
      (NewCapacity - FCapacity) * SizeOf(Pointer), 0);
    { Realloc FFillList }
    ReallocMem(Pointer(FFillList), NewCapacity * SizeOf(Pointer));
    FillChar(Pointer(LongInt(FFillList) + FCapacity * SizeOf(Pointer))^,
      (NewCapacity - FCapacity) * SizeOf(Pointer), 0);

{$IFNDEF LIST_MULTIPLE_BLOCKS}
    { Realloc Buffer }
    OldBuffer := FBuffer;
    ReallocMem(FBuffer, NewCapacity * FItemSize);
    FillChar((FBuffer + FCapacity * FItemSize)^,
      (NewCapacity - FCapacity) * FItemSize, FillValue);
    { Fix FList }
    FixList(FList, FCount, OldBuffer, OldBuffer + FCapacity * FItemSize,
      FBuffer - OldBuffer);
    { Fix FFillList }
    FixList(FFillList, FCapacity, OldBuffer, OldBuffer + FCapacity * FItemSize,
      FBuffer - OldBuffer);
    { Fill FFillList }
    while FCapacity < NewCapacity do
    begin
      FFillList^[FCapacity] := FBuffer + FCapacity * FItemSize;
      Inc(FCapacity);
    end;
{$ELSE}
    ReallocMem(FBuffer, (((NewCapacity-1) div LIST_BLOCK_SIZE) + 1) * SizeOf(Pointer));
    N := FCapacity div LIST_BLOCK_SIZE;

    for I := N to (NewCapacity div LIST_BLOCK_SIZE)-1 do
    begin
      { Realloc buffer }
      FBuffer^[I] := nil;
      GetMem(FBuffer^[I], LIST_BLOCK_SIZE * FItemSize);
      FillChar(FBuffer^[I]^, LIST_BLOCK_SIZE * FItemSize, FillValue);
      { Fill FFillList }
      for J := 0 to LIST_BLOCK_SIZE-1 do
      begin
        FFillList^[FCapacity] := FBuffer^[I] + J * FItemSize;
        Inc(FCapacity);
      end;
    end;
{$ENDIF}
  end;

  if FFillCount > FCapacity then
    FFillCount := FCapacity;
end;

{ Set new enabled count }
procedure TZItemList.SetCount(NewCount: Integer);
begin
  if (NewCount <= FCount) or (NewCount > MaxListSize) then
    Error('List Count Error at %d', NewCount);
  if (FFillCount + NewCount - FCount) > FCapacity then
    Grow;

  while FCount < NewCount do
  begin
    PByte(FFillList^[FFillCount])^ := ITEM_ENABLED;
    FList^[FCount] := FFillList^[FFillCount] + 1;
    Inc(FCount);
    Inc(FFillCount);
  end;
end;

{ Exchange two items }
function TZItemList.Exchange(Index1, Index2: Integer): Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error('List Index Error at %d', Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error('List Index Error at %d', Index2);
  Result := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Result;
end;

{ Get index of item }
function TZItemList.IndexOf(Item: Pointer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FCount-1 do
  begin
    if FList^[I] = Item then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

{ Expand the list }
function TZItemList.Expand: TZItemList;
begin
  if FFillCount = FCapacity then
    Grow;
  Result := Self;
end;

{ Remove the item by value }
function TZItemList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

{ Add a new item }
function TZItemList.Add: Pointer;
begin
  SetCount(FCount + 1);
  Result := FList^[FCount - 1];
end;

{ Delete an item }
function TZItemList.Delete(Index: Integer): Pointer;
var
  Ptr: PChar;
begin
  if (Index < 0) or (Index >= FCount) then
    Error('List Index Error at %d', Index);
  Result := FList[Index];
  Ptr := PChar(LongInt(Result) - 1);
  PByte(Ptr)^ := ITEM_DELETED;
  Dec(FCount);
  System.Move(FList^[Index + 1], FList^[Index],
    (FCount - Index) * SizeOf(Pointer));
end;

{ Insert an item }
function TZItemList.Insert(Index: Integer): Pointer;
begin
  Add;
  Result := Move(FCount-1, Index);
end;

{ Move item to another position }
function TZItemList.Move(CurIndex, NewIndex: Integer): Pointer;
var
  FillCurIndex: Integer;
  FillNewIndex: Integer;
  SavePtr: PChar;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error('List Index Error at %d', NewIndex);

    FillCurIndex := GetRealIndex(FList^[CurIndex]);
    FillNewIndex := GetRealIndex(FList^[NewIndex]);
    if (FillCurIndex < 0) or (FillNewIndex < 0) then
      Error('List Index Error at %d', NewIndex);

    SavePtr := FList^[CurIndex];
    Result := SavePtr;
    System.Move(FList^[CurIndex + 1], FList^[CurIndex],
      (FCount - CurIndex) * SizeOf(Pointer));
    System.Move(FList^[NewIndex], FList^[NewIndex + 1],
      (FCount - NewIndex - 1) * SizeOf(Pointer));
    FList^[NewIndex] := SavePtr;

    SavePtr := FFillList^[FillCurIndex];
    System.Move(FFillList^[FillCurIndex + 1], FFillList^[FillCurIndex],
      (FFillCount - FillCurIndex) * SizeOf(Pointer));
    System.Move(FFillList^[FillNewIndex], FFillList^[FillNewIndex + 1],
      (FFillCount - FillNewIndex - 1) * SizeOf(Pointer));
    FFillList^[FillNewIndex] := SavePtr;
  end else
    Result := nil;
end;

{ Get real index by pointer }
function TZItemList.GetRealIndex(Item: Pointer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FFillCount-1 do
  begin
    if (FFillList^[I]+1) = Item then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

{ Get pointer by index }
function TZItemList.GetRealItem(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FFillCount) then
    Error('List Index Error at %d', Index);
  Result := FFillList^[Index];
end;

end.
