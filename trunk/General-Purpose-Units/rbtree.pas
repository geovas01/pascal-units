{
  Red-black tree class, based on the STL tree implementation of
  gcc-3.4.4 (/libstdc++-v3/include/bits/stl_tree.h and
  /libstdc++-v3/src/tree.cc) of which the insertion and deletion
  algorithms are based on those in Cormen, Leiserson and Rivest,
  Introduction to Algorithms (MIT Press, 1990).
  
  This unit should work ok with Borland Delphi and Free Pascal (I used
  fpc-2.0.0 with the -Sd commandline switch).
  
  USAGE
  The TRBTree class behaves somewhat like a TList: it stores pointers
  and uses the same comparison function as TList.Sort (TListSortCompare).
  Functions Clear, Add, Delete, First and Last are equivalent,
  except that First and Last return a TRBNodeP instead of its key so they
  can be used for comparisons in loops. All values occur only once in the
  tree: when the same value is added twice, the second one is not stored.
  
  To be able to manage the tree, the Create constructor has a argument
  specifying the comparison function that should be used.
  
  The function Find can be used to find a value that was put in the tree,
  it searches for the given pointer using the comparison function given
  at time of object creation. It returns a TRBNodeP.
  
  The functions RBInc and RBDec can be used to "walk" through the tree:
  given a TRBNodeP x, RBInc returns the TRBNodeP with the smallest key that
  is larger than x, RBDec returns the TRBNodeP with the largest key that is
  smaller than x. RBInc(tree.Last) and RBDec(tree.First) are not defined.
  
  EXAMPLE
  An example for usage of this unit can be found at
  http://www.vanwal.nl/rbtree/example.dpr
  
  COMPLEXITY
  Create, First and Last are done in constant time.
  Find, Add, Delete, RBInc and RBDec take O(log n) time, where n is the
  number of items in the tree.
  Destroy and Clear take O(n) time.
  
  
  AUTHOR
  Written (or "translated" ;-)) by Freek van Walderveen, November 2005.
  Includes bug fixes by Jani Mátyás, July 2008.
  
  LICENCE
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.
  See http://www.gnu.org/copyleft/gpl.html
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  As a special exception, you may use this file as part of a free software
  library without restriction.  Specifically, if you compile
  this file and link it with other files to produce an executable, this
  file does not by itself cause the resulting executable to be covered by
  the GNU General Public License.  This exception does not however
  invalidate any other reasons why the executable file might be covered by
  the GNU General Public License.
}

unit rbtree;

interface

uses
  Classes;

type  
  TColor = (clRed, clBlack);
  TRBNodeP = ^TRBNode;
  TRBNode = record
    k: Pointer;
    left, right, parent: TRBNodeP;
    color: TColor;
  end;
  
  TRBTree = class
    private
      root: TRBNodeP;
      leftmost: TRBNodeP;
      rightmost: TRBNodeP;
      compareFunc: TListSortCompare;

      procedure RotateLeft(var x: TRBNodeP);
      procedure RotateRight(var x: TRBNodeP);
      function Minimum(var x: TRBNodeP): TRBNodeP;
      function Maximum(var x: TRBNodeP): TRBNodeP;
      
    public
      constructor Create(Compare: TListSortCompare);
      destructor Destroy(); override;
      
      procedure Clear();

      function Find(key: Pointer): TRBNodeP;
      function Add(key: Pointer): TRBNodeP;
      procedure Delete(z: TRBNodeP);
      
      property First: TRBNodeP read leftmost;
      property Last: TRBNodeP read rightmost;
   end; { class TRBTree }

procedure RBInc(var x: TRBNodeP);
procedure RBDec(var x: TRBNodeP);

implementation

constructor TRBTree.Create(Compare: TListSortCompare);
begin
  inherited Create;
  compareFunc := Compare;
  root := nil;
  leftmost := nil;
  rightmost := nil;
end;

destructor TRBTree.Destroy();
begin
  Clear();
  inherited Destroy;
end;

procedure fast_erase(x: TRBNodeP);
begin
  if (x^.left <> nil) then  fast_erase(x^.left);
  if (x^.right <> nil) then fast_erase(x^.right);
  dispose(x);
end;

procedure TRBTree.Clear();
begin
  if (root <> nil) then
    fast_erase(root);
  root := nil;
  leftmost := nil;
  rightmost := nil;
end;

function TRBTree.Find(key: Pointer): TRBNodeP;
var
  cmp: integer;
begin
  Result := root;
  while (Result <> nil) do begin
    cmp := compareFunc(Result^.k, key);
    if cmp < 0 then begin
      Result := Result^.right;
    end else if cmp > 0 then begin
      Result := Result^.left;
    end else begin
      break;
    end;
  end;
end;

procedure TRBTree.RotateLeft(var x: TRBNodeP);
var
  y: TRBNodeP;
begin
  y := x^.right;
  x^.right := y^.left;
  if (y^.left <> nil) then begin
    y^.left^.parent := x;
  end;
  y^.parent := x^.parent;
  if (x = root) then begin
    root := y;
  end else if (x = x^.parent^.left) then begin
    x^.parent^.left := y;
  end else begin
    x^.parent^.right := y;
  end;
  y^.left := x;
  x^.parent := y;
end;

procedure TRBTree.RotateRight(var x: TRBNodeP);
var
  y: TRBNodeP;
begin
  y := x^.left;
  x^.left := y^.right;
  if (y^.right <> nil) then begin
    y^.right^.parent := x;
  end;
  y^.parent := x^.parent;
  if (x = root) then begin
    root := y;
  end else if (x = x^.parent^.right) then begin
    x^.parent^.right := y;
  end else begin
    x^.parent^.left := y;
  end;
  y^.right := x;
  x^.parent := y;
end;

function TRBTree.Minimum(var x: TRBNodeP): TRBNodeP;
begin
  Result := x;
  while (Result^.left <> nil) do
    Result := Result^.left;
end;

function TRBTree.Maximum(var x: TRBNodeP): TRBNodeP;
begin
  Result := x;
  while (Result^.right <> nil) do
    Result := Result^.right;
end;

function TRBTree.Add(key: Pointer): TRBNodeP;
var
  x, y, z, zpp: TRBNodeP;
  cmp: Integer;
begin
  z := New(TRBNodeP);
  { Initialize fields in new node z }
  z^.k := key;
  z^.left := nil;
  z^.right := nil;
  z^.color := clRed;
  
  Result := z;
  
  { Maintain leftmost and rightmost nodes }
  if ((leftmost = nil) or (compareFunc(key, leftmost^.k) < 0)) then begin
    leftmost := z;
  end;
  if ((rightmost = nil) or (compareFunc(rightmost^.k, key) < 0)) then begin
    rightmost := z;
  end;
  
  { Insert node z }
  y := nil;
  x := root;
  while (x <> nil) do begin
    y := x;
    cmp := compareFunc(key, x^.k);
    if (cmp < 0) then begin
      x := x^.left;
    end else if (cmp > 0) then begin
      x := x^.right;
    end else begin
      { Value already exists in tree. }
      Result := x;
      dispose(z);
      exit;
    end;
  end;
  z^.parent := y;
  if (y = nil) then begin
    root := z;
  end else if (compareFunc(key, y^.k) < 0) then begin
    y^.left := z;
  end else begin
    y^.right := z;
  end;

  { Rebalance tree }
  while ((z <> root) and (z^.parent^.color = clRed)) do begin
    zpp := z^.parent^.parent;
    if (z^.parent = zpp^.left) then begin
      y := zpp^.right;
      if ((y <> nil) and (y^.color = clRed)) then begin
        z^.parent^.color := clBlack;
        y^.color := clBlack;
        zpp^.color := clRed;
        z := zpp;
      end else begin
        if (z = z^.parent^.right) then begin
          z := z^.parent;
          rotateLeft(z);
        end;
        z^.parent^.color := clBlack;
        zpp^.color := clRed;
        rotateRight(zpp);
      end;
    end else begin
      y := zpp^.left;
      if ((y <> nil) and (y^.color = clRed)) then begin
        z^.parent^.color := clBlack;
        y^.color := clBlack;
        zpp^.color := clRed;
        z := zpp;
      end else begin
        if (z = z^.parent^.left) then begin
          z := z^.parent;
          rotateRight(z);
        end;
        z^.parent^.color := clBlack;
        zpp^.color := clRed;
        rotateLeft(zpp);
      end;
    end;
  end;
  root^.color := clBlack;
end;


procedure TRBTree.Delete(z: TRBNodeP);
var
  w, x, y, x_parent: TRBNodeP;
  tmpcol: TColor;
begin
  y := z;
  x := nil;
  x_parent := nil;

  if (y^.left = nil) then begin    { z has at most one non-null child. y = z. }
    x := y^.right;     { x might be null. }
  end else begin
    if (y^.right = nil) then begin { z has exactly one non-null child. y = z. }
      x := y^.left;    { x is not null. }
    end else begin
      { z has two non-null children.  Set y to }
      y := y^.right;   {   z's successor.  x might be null. }
      while (y^.left <> nil) do begin
        y := y^.left;
      end;
      x := y^.right;
    end;
  end;
  
  if (y <> z) then begin
    { "copy y's sattelite data into z" }
    { relink y in place of z.  y is z's successor }
    z^.left^.parent := y; 
    y^.left := z^.left;
    if (y <> z^.right) then begin
      x_parent := y^.parent;
      if (x <> nil) then begin
        x^.parent := y^.parent;
      end;
      y^.parent^.left := x;   { y must be a child of left }
      y^.right := z^.right;
      z^.right^.parent := y;
    end else begin
      x_parent := y;
    end;
    if (root = z) then begin
      root := y;
    end else if (z^.parent^.left = z) then begin
      z^.parent^.left := y;
    end else begin
      z^.parent^.right := y;
    end;
    y^.parent := z^.parent;
    tmpcol := y^.color;
    y^.color := z^.color;
    z^.color := tmpcol;
    y := z;
    { y now points to node to be actually deleted }
  end else begin                        { y = z }
    x_parent := y^.parent;
    if (x <> nil)  then begin
      x^.parent := y^.parent;
    end;   
    if (root = z) then begin
      root := x;
    end else begin
      if (z^.parent^.left = z) then begin
        z^.parent^.left := x;
      end else begin
        z^.parent^.right := x;
      end;
    end;
	  if (leftmost = z) then begin
	    if (z^.right = nil) then begin      { z^.left must be null also }
	      leftmost := z^.parent;
	    end else begin
	      leftmost := minimum(x);
      end;
    end;
	  if (rightmost = z) then begin
	    if (z^.left = nil) then begin       { z^.right must be null also }
	      rightmost := z^.parent;  
	    end else begin                     { x == z^.left }
	      rightmost := maximum(x);
      end;
    end;
  end;
  
  { Rebalance tree }
  if (y^.color = clBlack)  then begin 
    while ((x <> root) and ((x = nil) or (x^.color = clBlack))) do begin
      if (x = x_parent^.left)  then begin
          w := x_parent^.right;
          if (w^.color = clRed)  then begin
            w^.color := clBlack;
            x_parent^.color := clRed;
            rotateLeft(x_parent);
            w := x_parent^.right;
          end;
          if (((w^.left = nil) or 
               (w^.left^.color = clBlack)) and
              ((w^.right = nil) or 
               (w^.right^.color = clBlack)))  then begin
            w^.color := clRed;
            x := x_parent;
            x_parent := x_parent^.parent;
          end else begin
            if ((w^.right = nil) or (w^.right^.color = clBlack)) then begin
              w^.left^.color := clBlack;
              w^.color := clRed;
              rotateRight(w);
              w := x_parent^.right;
            end;
            w^.color := x_parent^.color;
            x_parent^.color := clBlack;
            if (w^.right <> nil)  then begin
              w^.right^.color := clBlack;
            end;
            rotateLeft(x_parent);
            x := root; { break; }
         end
      end else begin   
        { same as above, with right <^. left. }
        w := x_parent^.left;
        if (w^.color = clRed)  then begin
          w^.color := clBlack;
          x_parent^.color := clRed;
          rotateRight(x_parent);
          w := x_parent^.left;
        end;
        if (((w^.right = nil) or 
             (w^.right^.color = clBlack)) and
            ((w^.left = nil) or 
             (w^.left^.color = clBlack)))  then begin
          w^.color := clRed;
          x := x_parent;
          x_parent := x_parent^.parent;
        end else begin
          if ((w^.left = nil) or (w^.left^.color = clBlack)) then begin
            w^.right^.color := clBlack;
            w^.color := clRed;
            rotateLeft(w);
            w := x_parent^.left;
          end;
          w^.color := x_parent^.color;
          x_parent^.color := clBlack;
          if (w^.left <> nil) then begin
            w^.left^.color := clBlack;
          end;
          rotateRight(x_parent);
          x := root; { break; }
        end;
      end;
    end;
    if (x <> nil) then begin
      x^.color := clBlack;
    end;
  end;
  dispose(y);
end;

{ Pre: x <> last }
procedure RBInc(var x: TRBNodeP);
var
  y: TRBNodeP;
begin
  if (x^.right <> nil) then begin
    x := x^.right;
    while (x^.left <> nil) do begin
      x := x^.left;
    end;
  end else begin
    y := x^.parent;
    while (x = y^.right) do begin
      x := y;
      y := y^.parent;
    end;
    if (x^.right <> y) then
      x := y;
  end
end;

{ Pre: x <> first }
procedure RBDec(var x: TRBNodeP);
var
  y: TRBNodeP;
begin
  if (x^.left <> nil)  then begin
    y := x^.left;
    while (y^.right <> nil) do begin
      y := y^.right;
    end;
    x := y;
  end else begin
    y := x^.parent;
    while (x = y^.left) do begin
      x := y;
      y := y^.parent;
    end;
    x := y;
  end
end;

end.

