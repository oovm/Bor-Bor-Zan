(* ::Package:: *)

(* ::Section:: *)
(*Auxiliary Functions*)


(* ::Subsection:: *)
(*Usage*)


V::usage="V[i,j] \:8868\:793a\:6211\:65b9 i \:6c14, \:654c\:65b9 j \:6c14\:65f6\:7684\:80dc\:7387\:8bc4\:4f30";
ExpandV::usage="ExpandV \:80fd\:5c55\:5f00\:652f\:4ed8\:77e9\:9635";


(* ::Subsection:: *)
(*Definition  *)


ExpandV[e__] := Block[
	{eval, V},
	eval[i_, j_] := Which[
		j == i, {1 / 2, 1 / 2},
		j > i, {1 - V[i, j], V[i, j]},
		True, {V[i, j], 1 - V[i, j]}
	];
	Return[e /. V -> eval]
];
getAction[V[i_,j_]]:=Outer[List,getAction[i],getAction[j]]
getAction[energy_Integer] := Block[
	{slot, rm},
	slot = {Overkill, Dodge, Rebound, Skill, Defense, Attack, Power};
	rm[ops_] := DeleteCases[slot, Alternatives @@ ops];
	Which[
		energy >= 5, slot,
		energy >= 3, rm@{Overkill},
		energy >= 1, rm@{Overkill, Skill},
		energy == 0, {Power, Defense}
	]
];


(* ::Section:: *)
(*State Transition*)


(* ::Subsection::Closed:: *)
(*Definition  *)


VS[V[me_, enemy_], {Overkill, s_}] := Switch[s,
	Overkill, {0, 0},
	___, {1, 0}
];
VS[V[me_, enemy_], {Dodge, s_}] := Switch[s,
	Overkill, {0, 1},
	Dodge, V[me - 1, enemy - 1],
	Rebound, V[me - 1, enemy - 1],
	Skill, V[me - 1, enemy - 3],
	Defense, V[me - 1, enemy],
	Attack, V[me - 1, enemy - 1],
	Power, V[me - 1, enemy + 1]
];
VS[V[me_, enemy_], {Rebound, s_}] := Switch[s,
	Overkill, {0, 1},
	Dodge, V[me - 1, enemy - 1],
	Rebound, V[me - 1, enemy - 1],
	Skill, {0, 1},
	Defense, V[me - 1, enemy],
	Attack, {1, 0},
	Power, V[me - 1, enemy + 1]
];
VS[V[me_, enemy_], {Skill, s_}] := Switch[s,
	Overkill, {0, 1},
	Dodge, V[me - 3, enemy - 1],
	Rebound, {1, 0},
	Skill, V[me - 3, enemy - 3],
	Defense, {1, 0},
	Attack, {1, 0},
	Power, {1, 0}
];
VS[V[me_, enemy_], {Defense, s_}] := Switch[s,
	Overkill, {0, 1},
	Dodge, V[me, enemy - 1],
	Rebound, V[me, enemy - 1],
	Skill, {0, 1},
	Defense, V[me, enemy],
	Attack, V[me, enemy - 1],
	Power, V[me, enemy + 1]
];
VS[V[me_, enemy_], {Attack, s_}] := Switch[s,
	Overkill, {0, 1},
	Dodge, V[me - 1, enemy - 1],
	Rebound, {0, 1},
	Skill, {0, 1},
	Defense, V[me - 1, enemy],
	Attack, V[me - 1, enemy - 1],
	Power, {1, 0}
];
VS[V[me_, enemy_], {Power, s_}] := Switch[s,
	Overkill, {0, 1},
	Dodge, V[me + 1, enemy - 1],
	Rebound, V[me + 1, enemy - 1],
	Skill, {0, 1},
	Defense, V[me + 1, enemy],
	Attack, {0, 1},
	Power, V[me + 1, enemy + 1]
];


(* ::Subsection:: *)
(*Test*)


(* ::Input:: *)
(*With[*)
(*	{pos = Tuples[{Overkill, Dodge, Rebound, Skill, Defense, Attack, Power}, {2}]},*)
(*	MapIndexed[# -> VS[V[5, 5], #]&, pos] /. {V[i_, i_] :> {1 / 2, 1 / 2}}*)
(*]*)
