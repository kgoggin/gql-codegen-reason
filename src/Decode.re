open Context;

let typeNames = [];

let make = (types) => {
	types |> Array.map((t: Type.t) => {
		let typeNames = [...typeNames, t.name]
	})
}
