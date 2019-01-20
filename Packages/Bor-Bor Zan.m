(* ::Package:: *)

(* ::Section:: *)
(*State Transition*)


(* ::Subsection:: *)
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
