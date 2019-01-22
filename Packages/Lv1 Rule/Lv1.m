(* ::Package:: *)

Clear["*"];
$Slot = Sort@{Skill, Defense, Attack, Energy};
$SkillCost = 3;
$EnergyAdd = 1;


(* ::Section::Closed:: *)
(*State Transition*)


(* ::Subsection:: *)
(*Definition  *)


VS[V[me_, enemy_], {Rebound, s_}] := Switch[s,
	Rebound, V[me - 1, enemy - 1],
	Skill, {0, 1},
	Defense, V[me - 1, enemy],
	Attack, {1, 0},
	Energy, V[me - 1, enemy + $EnergyAdd]
];
VS[V[me_, enemy_], {Skill, s_}] := Switch[s,
	Rebound, {1, 0},
	Skill, V[me - $SkillCost, enemy - $SkillCost],
	Defense, {1, 0},
	Attack, {1, 0},
	Energy, {1, 0}
];
VS[V[me_, enemy_], {Defense, s_}] := Switch[s,
	Rebound, V[me, enemy - 1],
	Skill, {0, 1},
	Defense, V[me, enemy],
	Attack, V[me, enemy - 1],
	Energy, V[me, enemy + $EnergyAdd]
];
VS[V[me_, enemy_], {Attack, s_}] := Switch[s,
	Rebound, {0, 1},
	Skill, {0, 1},
	Defense, V[me - 1, enemy],
	Attack, V[me - 1, enemy - 1],
	Energy, {1, 0}
];
VS[V[me_, enemy_], {Energy, s_}] := Switch[s,
	Rebound, V[me + $EnergyAdd, enemy - 1],
	Skill, {0, 1},
	Defense, V[me + $EnergyAdd, enemy],
	Attack, {0, 1},
	Energy, V[me + $EnergyAdd, enemy + $EnergyAdd]
];


(* ::Section::Closed:: *)
(*Auxiliary Functions*)


(* ::Subsection:: *)
(*Usage*)


V::usage = "V[i,j] \:8868\:793a\:6211\:65b9 i \:6c14, \:654c\:65b9 j \:6c14\:65f6\:7684\:80dc\:7387\:8bc4\:4f30";
ExpandV::usage = "ExpandV \:80fd\:5c55\:5f00\:652f\:4ed8\:77e9\:9635";


(* ::Subsection:: *)
(*Definition  *)


EvaluateV[i_, j_] := Which[
	i >= $SkillCost > j, 1,
	{i, j} == {$SkillCost - 1, 0}, 1,
	j == i, 1 / 2,
	j > i, 1 - V[j, i],
	True, V[i, j]
];
ExpandV[e__] := Block[
	{eval},
	expand[i_, j_] := {EvaluateV[i, j], 1 - EvaluateV[i, j]};
	Return[e /. {V -> expand} /. {V -> EvaluateV}]
];
getAction[V[i_, j_]] := Outer[List, getAction[i], getAction[j]]
getAction[energy_Integer] := GeneralUtilities`Scope[
	rm = DeleteCases[$Slot, Alternatives @@ #]&;
	Which[
		energy >= $SkillCost, $Slot,
		energy >= 1, rm@{Skill},
		energy == 0, rm@{Rebound, Skill, Attack}
	]
];
getSituation[i_, j_] := ExpandV@Map[VS[V[i, j], #]&, getAction[V[i, j]], {2}]
getEqn[i_, j_] := GeneralUtilities`Scope[
	If[j > i, Return[False]];
	{me, enemy} = Transpose[getSituation[i, j], 1<->3];
	{
		Equal @@ Prepend[me.getAction[i], V[i, j]],
		If[MemberQ[ $Slot, Rebound ],
			If[j == 0, Defense == 0 && Rebound == 0, Nothing],
			If[j == 0, Defense == 0, Nothing]
		]
	}
]
getRelation[i_, j_] := GeneralUtilities`Scope[
	If[j > i, Return[False]];
	{me, enemy} = Transpose[getSituation[i, j], 1<->3];
	eqn = {
		Equal @@ Prepend[me.getAction[i], V[i, j]],
		Tr@getAction[i] == 1,
		If[MemberQ[ $Slot, Rebound ],
			If[j == 0, Defense == 0 && Rebound == 0, Nothing],
			If[j == 0, Defense == 0, Nothing]
		]
	};
	Eliminate[eqn, getAction[i]]
]
EvaluateWins[f_ : NSolve] := GeneralUtilities`Scope[
	g = Table[#[i - 1, j], {i, 2, $SkillCost}, {j, 0, i - 2}]&;
	eqn = {
		g@getRelation,
		Thread[g@V > 0]
	};
	<|
		Flatten@{
			f[Flatten@eqn, Flatten@g@V],
			Table[V[i, i] -> 1 / 2, {i, 0, $SkillCost}]
		(*Table[V[$SkillCost,i]\[Rule]1,{i,0,$SkillCost-1}]*)
		}
	|>
];
getSolution[i_, j_] := GeneralUtilities`Scope[
	If[i + j == 0, Return@<|Energy -> 1|>];
	If[{i, j} == { $SkillCost , $SkillCost }, Return@<|Energy -> 1|>];
	If[i >= $SkillCost > j, Return@<|Skill -> 1|>];
	sol = Solve[getEqn[i, j], getAction[i]];
	Association@Flatten[sol /. win]
];


(* ::Section:: *)
(*Evaluation*)


(*EvaluateWins[ ] \:83b7\:5f97\:89e3\:6790\:89e3*)
win = N /@ EvaluateWins[NSolve]


Flatten@Table[V[i, j] -> getSolution[i, j], {j, 0, $SkillCost}, {i, j, $SkillCost}] // TableForm
