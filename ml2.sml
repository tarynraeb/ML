{\rtf1\ansi\ansicpg1252\cocoartf1404\cocoasubrtf470
{\fonttbl\f0\fswiss\fcharset0 Helvetica;\f1\fnil\fcharset0 Calibri;}
{\colortbl;\red255\green255\blue255;}
\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 (* Taryn Burns *)\
(* CS 355 *)\
(* ML Homework 2 *)\
( * *)\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f1\fs28 \cf0 (* \'97\'97\'97\'97\'97\'97\'97\'97\'97\'97\'97 *)\
fun fold f base [] = base\
| fold f base (x::rest) = f x (fold f base rest);\
fun map f [] = []\
| map f (x::rest) = (f x)::(map f rest);\
(* 1a countinList *)\
fun countinList L t =\
	let\
		fun checkifequal counted [] t = counted\
		| checkifequal counted (x::rest) t =\
			 if t = x then checkifequal (counted + 1) rest t\
			else checkifequal counted rest t\
	in\
		checkifequal 0 L t\
	end;\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 countInList [\'933\'94,\'945\'94,\'945\'94,\'944\'94,\'945\'94,\'941\'94] \'935\'94;\
(* val it = 3 : int *)\
countInList [] \'935\'94;\
countInList [[],[1,2],[3,2],[5,6,7],[8],[]] [];
\f1\fs28 \
(* 1b ziptail *)\
fun zipTail list1 list2 = \
	let\
		fun makePair finallist list1 [] = finallist\
		| makePair finallist [] list2 = finallist\
		| makePair finallist (x::rest1) (y::rest2) = makepair (finalList@[(x,y)]) rest1 rest2\
	in \
		makePair [] list1 list2\
end;\
(* 1c histogram *)\
fun histogram L =\
	let\
		fun inList(h,[]) = false\
		| inList(h,x::rest) = if h=x then true else inList(h,rest)\
		fun removeDups [] = []\
		| removeDups (x::rest) = if inList(x,rest) then (removeDups rest)\
		fun makeHist L = zipTail (removeDups L) (map countinList L) (removeDups L));\
	in\
		makeHist L\
	end;\
(* 2a deepSum *)\
fun deepSum L = \
	let\
		fun add m n = m + n;\
		fun addup L = fold add 0 L;\
		fun allSum L = fold add 0 (map addup L);\
	in\
		allSum L\
	end;\

\f0\fs24 deepSum [[1,2,3],[4,5],[6,7,8,9],[]];\
deepSum [[10,10],[10,10,10],[10]];\
deepSum [[]];
\f1\fs28 \
(* 2b deepSumOption *)\
fun deepSumOption L =\
	let\
		fun add NONE n = n\
		| add m NONE = m\
		| add m n = SOME(getOpt (m,0) + getOpt(n,0));\
		fun addup L = fold add NONE L;\
		fun allSum L = fold add NONE (map addup L);\
	in\
		allSum L\
	end;\
(* 3. unzip *)\
fun unzip L = \
	let\
		fun getfirstval (m,n) = m;\
		fun getsecondval(m,n) = n;\
		fun separatethem L = [(map getfirstval L), (map getsecondval L)];\
	in	\
		separate L\
	end;\

\f0\fs24 unzip [(1,2),(3,4),(5,6)];\
unzip [(\'931\'94,\'94a\'94),(\'935\'94,\'94b\'94),(\'938\'94,\'94c\'94)];
\f1\fs28 \
(* \'97\'97\'97\'97\'97\'97\'97\'97\'97\'97\'97 *)
\f0\fs24 \
(* 4a eitherTree *)\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f1\fs28 \cf0 (* *)\
datatype either = ImAString of string | ImAnInt of int;\
(*datatype either = ImAString of string | ImAnInt of int *)\
\pard\pardeftab720\ri0\partightenfactor0
\cf0 datatype eitherTree = LEAF of either | NODE of (eitherTree * eitherTree);\
(* datatype eitherTree = LEAF of either | NODE of eitherTree * eitherTree *)\
(* *)\
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 (* \'97\'97\'97\'97\'97\'97\'97\'97\'97\'97\'97 *)
\f0\fs24 \
(* 4b eitherSearch *) \
(* *)\
\pard\pardeftab720\ri0\partightenfactor0

\f1\fs28 \cf0 fun eitherSearch(LEAF(ImAnInt x)) y = (x=y)\
  | eitherSearch(LEAF(ImAString x)) y = false\
  | eitherSearch(NODE (t1,t2)) y = (eitherSearch t1 y)\
  orelse (eitherSearch t2 y);\
(* val eitherSearch = fn : eitherTree -> int -> bool *)\
(* *)\
(* \'97\'97\'97\'97\'97\'97\'97\'97\'97\'97\'97 *)
\f0\fs24 \
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0
\cf0 (* 5 findMin *) \
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f1\fs28 \cf0 datatype \'91a Tree = LEAF of \'91a | NODE of (\'91a Tree) * (\'91a Tree)\
datatype \'91a myTree = myLEAF of \'91a | myNode of \'91a*\'92a*(\'91a myTree)*(\'91a myTree)\
(* *)\
fun findMin (LEAF v) = v\
| findMin (NODE (left,right)) =\
     let val minLeft = findMin left\
            val minRight = findMin right\
      in if minLeft < minRight then minLeft else minRight\
end;\
findMin (NODE(NODE(LEAF(5),NODE(LEAF(6),LEAF(8))),LEAF(4)));\
findMin (NODE(NODE(NODE(LEAF(0),LEAF(11)),LEAF(6)),NODE(LEAF(3),LEAF(10))));\
findMin (LEAF(5));\
(* \'97\'97\'97\'97\'97\'97\'97\'97\'97\'97\'97 *)
\f0\fs24 \
(* findMax *) \

\f1\fs28 fun findMax(LEAF v) = v\
| findMax (NODE (left,right)) =\
     let val maxLeft = findMax left\
            val maxRight = findMax right\
      in if maxLeft < maxRight then maxLeft else maxRight\
end;\
findMax (NODE(NODE(LEAF(5),NODE(LEAF(6),LEAF(8))),LEAF(4)));\
findMax (NODE(NODE(NODE(LEAF(0),LEAF(11)),LEAF(6)),NODE(LEAF(3),LEAF(10))));\
findMax (LEAF(5));\
(* \'97\'97\'97\'97\'97\'97\'97\'97\'97\'97\'97 *)
\f0\fs24 \
(* minMaxTree *) \

\f1\fs28 (* \'97\'97\'97\'97\'97\'97\'97\'97\'97\'97\'97 *)\
(* Test Functions *) \
(* *) \
\pard\pardeftab720\ri0\partightenfactor0
\cf0 ( * ************************** *)\
(* Test for the trees *) \
(* setting the nodes and leaves to variables *)\
fun eitherTest () = \
let\
    val L1 = LEAF(ImAnInt 1)\
    val L2 = LEAF(ImAnInt 2)\
    val L3 = LEAF(ImAnInt 3)\
    val L4 = LEAF(ImAnInt 4)\
    val L5 = LEAF(ImAnInt 5)\
    val L6 = LEAF(ImAString \'93a\'94)\
    val L7 = LEAF(ImAString \'93b\'94)\
    val L8 = LEAF(ImAString \'93c\'94)\
    val L9 = LEAF(ImAString \'93d\'94)\
    val L10 = LEAF(ImAString \'93e\'94)\
    val N1 = NODE (L1, L2)\
    val N2 = NODE (N1, L3)\
    val N3 = NODE (N2, L4)\
    val N4 = NODE (N3, L5)\
    val N5 = NODE (N4, L6)\
    val N6 = NODE (N5, L7)\
    val N7 = NODE (N6, L8)\
    val N8 = NODE (N7, L9)\
    val N9 = NODE (N8, L10)\
        in\
           not (eitherSearch N9 5)\
        end;}