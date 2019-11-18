{\rtf1\ansi\ansicpg1252\cocoartf1404\cocoasubrtf470
{\fonttbl\f0\fnil\fcharset0 Calibri;}
{\colortbl;\red255\green255\blue255;}
\margl1440\margr1440\vieww9980\viewh8160\viewkind0
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardirnatural\partightenfactor0

\f0\fs24 \cf0 \CocoaLigature0 (* Taryn Burns CptS 355, ML Hw1 *)\
(* *)\
\pard\pardeftab720\ri0\partightenfactor0
\cf0 \CocoaLigature1 (* --------------------------------------------------------------- *)\
(* --------------------------------------------------------------- *)\
(* 1. inList *)\CocoaLigature0 \
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardirnatural\partightenfactor0
\cf0 fun inList(m,nil) = false  (* if m is not in the empty list, since nil = [] and has type \'91a list *)\
 | inList(m,x::xs) = if (m=x) \
    then true\
    else inList(m,xs);\
(* Test for inList *)\
fun myTest_inList(t,b,r) = \
  if(inList t b) = r then true else false;\
val inList_test1 = myTest_inList(1,[]); \
\pard\pardeftab720\ri0\partightenfactor0
\cf0 \CocoaLigature1 (* --------------------------------------------------------------- *)\
(* --------------------------------------------------------------- *)\
(* 2. removeDuplicate *)\
(* helper function for removeDuplicates, it does all the work of removing *) \
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardirnatural\partightenfactor0
\cf0 \CocoaLigature0 fun remove(x,L) = \
    if (L=[]) then [] (* if the list is empty, then pretty much do nothing, its empty *) \
    else (if (x=hd(L)) (* if the list is not empty *) \
      then remove(x,tl(L)) (* remove the element *) \
      else hd(L)::remove(x,tl(L)));\
(* val remove = fn : ''a * ''a list -> ''a list *)\
(* actually goes through and finds the duplicate then calls remove to remove/delete the duplicated value in list *) \
fun removeDuplicates(L) = \
    if(L = []) then [] (* if the list is empty, then pretty much do nothing, its empty *) \
    else hd(L)::remove(hd(L), removeDuplicates(tl(L))); (* calls for remove and removeDuplicates to handle that duplicated value *) \
(* val removeDuplicates = fn : ''a list -> ''a list *)\
removeDuplicates[1,5,1,3,4,3,5];\
(* val it = [1,5,3,4] : int list *)\
(* Test for removeDuplicate *) \
fun myTest_removeDuplicates(\CocoaLigature1 b,t, a) =\
\pard\pardeftab720\ri0\partightenfactor0
\cf0        if(removeDuplicates b, t) = a then true else false;\
val removeDuplicates_test1 =      myTest_removeDuplicates([1,5,1,3,4,3,5]);\
val removeDuplicates_test2 = myTest_removeDuplicates([]); \
(* --------------------------------------------------------------- *)\
(* --------------------------------------------------------------- *)\
(* 3. listIntersect *)\
(* helper function for listInterseect *) \
\pard\pardeftab720\partightenfactor0
\cf0 \expnd0\expndtw0\kerning0
fun member(x,[]) = false\
|   member(x,b::y) =\
       if x=b then true\
       else member(x,y);\
(* val member = fn : ''a * ''a list -> bool *)\
(* the listIntersect funct *) \
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0
\cf0 \kerning1\expnd0\expndtw0 \CocoaLigature0 fun listIntersect([],y) = []\
 | listIntersect(a::x, y) = \
       if member(a,y) then a::listIntersect(x,y)\
       else listIntersect(x,y);\
(* val listIntersect = fn : ''a list * ''a list -> ''a list *)\
listIntersect([1,3,5],[2,3,4]);\
(* + + + + + ++ + + ++ + + + + +  *)\
(* Test for listIntersect *)\
fun myTest_listIntersect(a,b,c) =\
   if(listIntersect a b) = c then true else false;\
val listIntersect_test1 = myTest_listIntersect([1,2,3],[1,1,2]);\
val listIntersect_test2 = myTest_listIntersect([1],[1]);\
(* val it = [3] : int list *)\
\pard\pardeftab720\ri0\partightenfactor0
\cf0 \CocoaLigature1 (* --------------------------------------------------------------- *)\
(* --------------------------------------------------------------- *)\
(* 4. Range *)\
fun range min 0 max = []\
| range min count max =\
if ((real(max-min))/(real count)) <= 0.0 then []\
else min::range (min+count) count max;\
(* Test Cases for Range *) \
fun myTest_Range(d,e,f) = \
   if(range d e) = f then true else false;\
val range_test1 = myTest_Range(0 5 30);\
val range_test2 = myTest_Range(10 1 10);\
(* --------------------------------------------------------------- *)\
(* --------------------------------------------------------------- *)\
(* 5. numberstoSum *)\
(* hd -> head and tl -> tail *)\
fun numstoSum sum [] = []\
| numstoSum sum sumList = \
if (sum-hd(sumList))>0 then hd(sumList)::numstoSum(sum-hd(sumList))(tl(sumList))\
else [];\
(* Test case for numstoSum *)\
fun myTest_numberstoSum(x,y,z) = \
    if (numberstoSum x y) = z then true else false;\
val numberstoSum_test1 = myTest_numberstoSum(100,[10,20,30,40]);\
val numberstoSum_test2 = myTest_numberstoSum(30,[5,4,6,10,4,2,1,5]);\
(* --------------------------------------------------------------- *)\
(* --------------------------------------------------------------- *)\
(* 6. Replace *)\
fun replace index rvalue [] = []\
| replace 0 rvalue rlist = rvalue::(tl(rlist))\
| replace index rvalue rlist = (hd(rlist))::(replace(index-1) rvalue\
(tl(rlist));\
(* Test case for replace *) \
fun myTest_replace(t,u,v) = \
    if (replace t u) = v then true else false;\
val replace_test1 = myTest_replace(3 40,[1,2,3,4,5,6]););\
val replace_test2 = myTest_replace();\
(* --------------------------------------------------------------- *)\
(* --------------------------------------------------------------- *)\
(* 7. GroupNLeft and GroupNRight *) \
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardeftab720\pardirnatural\partightenfactor0
\cf0 \CocoaLigature0 fun makeLists 0 y = [] \
| makeLists m n = hd(n)::makeLists(m-1)(tl(n));\
fun finished 0 n = n\
| finished m n = finished(m-1)(tl(n));\
fun groupNleft m [] = []\
| groupNleft 0 n = []\
| groupNleft m n = if (length(n) mod m > 0) then (makeLists((length(n)) mod m) n) :: (groupNleft m (finished(length(n) mod m) n ))\
else (makeLists m n) :: (groupNleft m (finished m n));\
fun groupNright m [] = []\
| groupNright 0 n = []\
| groupNright m n = if (length(n) < m) then (makeLists(length(n)) n) :: (makeLists m n) :: (groupNright m (finished m n));\
\pard\pardeftab720\ri0\partightenfactor0
\cf0 \CocoaLigature1 (* Test for Npairs *)\
fun myTest_pairNleft (n,L,output) =\
           if (pairNleft n L) = output then true else false;\
fun myTest_painNright(n, L, output) = \
            if (pairNright n L) = output then true else false ;\
val pairNleft_test1 = myTest_pairNleft(2,[1,2,3,4,5], [[1].[2,3],[4,5]]);\
val pairNright_test1 = myTest_pairNright(2,[1,2,3,4,5], [[1,2],[3,4],[5]]);\
(* --------------------------------------------------------------- *)\
}