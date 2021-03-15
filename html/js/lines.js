/*global AGJ:false*/
;(function (that) {

	AGJ.defineModule(AGJ, "projects.spwords.languages", {

		// ENGLISH

		en: {
			letters: "abcdefghijklmnopqrstuvwxyz",
			names: {
				player: "player",
				computer: "computer",
				left: "left",
				right: "right",
				here: "here",
				there: "there"
			},
			comments: {
				loading: "(loading...)",
				toStart: "(done. press enter.)",
				playerOnline: "(opponent joined.)",
				start: "welcome to tonight's exciting match! " +
					"it's menacing {one} against favorite {two}! ",
				rules: "remember the rules: " +
					"in turns, contestants type words that start with the assigned letter, " +
					"and which contain the last letter of the previous word. then, press enter. " +
					"no repeats, and watch the time limit! " +
					"first to seize three rounds is the victor. " +
					"now, let the match begin!",
				roundStart: [
					"let's see who comes out victorious in the next round!",
					"now for another round full of suspense!",
					"who will make the best of this round?"
				],
				/*
				turn: [
					"{turn}'s turn!",
					"{turn} starts!",
					"{turn} speaks!",
					"{turn} serves!"
				],
				letterAnnounce: [
					"featuring letter \"{letter}\"!",
					"time for letter \"{letter}\"!",
					"watch for letter \"{letter}\"!",
					"here we go, letter \"{letter}\"!",
					"it's letter \"{letter}\"!",
					"we want letter \"{letter}\"!",
					"show us letter \"{letter}\"!"
				],
				*/
				turnAndLetter: [
					"starting, it's {turn} with \"{letter}\"!",
					"it's {turn} with \"{letter}\"!",
					"serving is {turn}, with \"{letter}\"!",
					"here we go, turn for {turn}, with \"{letter}\"!",
					"watch for that \"{letter}\", {turn}!",
					"time for \"{letter}\", {turn}!",
					"featuring \"{letter}\", speaks {turn}!",
					"we want letter \"{letter}\", {turn}!",
					"show us that \"{letter}\", {turn}!"
				],
				interjection: [
					"ooh!",
					"nice!",
					"good!",
					"safe!",
					"wow!",
					"works!",
					"fair!"
				],
				mistake: {
					startLetter: [
						"did not start with \"{letter}\"!"
					],
					includeLetter: [
						"does not contain previous word's \"{letter}\"!"
					],
					alreadyPlayed: [
						"we've seen that word before!",
						"that one's a repeat!"
					],
					doesntExist: [
						"is that english?",
						"no such word in my dictionary!",
						"what does that even mean?"
					],
					timeOut: [
						"time is up!",
						"time ran out!",
						"no more time!"
					]
				},
				roundEnd: {
					point: [
						"brilliant point for {winner}!",
						"{loser} wastes a chance!",
						"tough luck!",
						"what a shock!",
						"{winner} scores!",
						"too bad for {loser}!",
						"close, but no dice!",
						"it's {loser}'s miss!",
						"{winner} takes this point!"
					],
					winning: [
						"{winner} has the lead!",
						"{winner} is ahead!",
						"{loser} needs to step up!",
						"{loser} is not looking good!"
					],
					tie: [
						"it's tied!",
						"{points} all!",
						"it's a battle of noses!",
						"both sides equal!",
						"both with {points}!"
					]
				},
				scoreTally: [
					"we have our game at {one} {pointsOne}, {two} {pointsTwo}!",
					"the panel reads {one} {pointsOne} versus {two} {pointsTwo}.",
					"{one} at {pointsOne}, {two} at {pointsTwo}!"
				],
				gameEnd: "and it's settled! {winner} defeats {loser} {winnerPoints} to {loserPoints} in a match to remember! " +
					"we look forward to when these two titans have another face-off. " +
					"see you next time!"
			}
		},

		// SPANISH

		es: {
			letters: "abcdefghijklmnñopqrstuvwxyz",
			names: {
				player: "jugador",
				computer: "máquina",
				left: "izquierda",
				right: "derecha",
				here: "aquí",
				there: "allá"
			},
			comments: {
				loading: "(cargando...)",
				toStart: "(listo. aprieta enter.)",
				playerOnline: "(entró contrincante.)",
				start: "¡sean bienvenidos, esta noche, a una nueva lucha! " +
					"hoy se miden {one} con {two}, dos grandes. ",
				rules: "recuerden las reglas: " +
					"por turnos, los competidores tipean palabras que comiencen con la letra asignada, " +
					"y que contengan la última letra de la palabra anterior. luego, presionan enter. " +
					"no se valen repetidas, y cuidado con el tiempo. " +
					"el primero en dominar tres asaltos es el vencedor. " +
					"muy bien; ¡que empiece!",
				roundStart: [
					"veamos quién se apropia la victoria en este asalto.",
					"ahora a un nuevo asalto. ",
					"a ver quién le saca el provecho a este asalto."
				],
				turnAndLetter: [
					"el turno de {turn}, con \"{letter}\".",
					"sirve {turn}, con \"{letter}\".",
					"parte {turn}, con \"{letter}\".",
					"le toca a la letra \"{letter}\", habla {turn}.",
					"usando la \"{letter}\", habla {turn}.",
					"¡muéstranos una \"{letter}\", {turn}!",
					"saque de {turn}, con \"{letter}\".",
					"la \"{letter}\" para {turn}.",
					"ataca {turn}, con \"{letter}\".",
					"¡nos sirven las \"{letter}\", {turn}!"
				],
				interjection: [
					"¡bien!",
					"¡ah!",
					"¡entra!",
					"¡ok!",
					"¡pasa!"
				],
				mistake: {
					startLetter: [
						"¡no empezó con \"{letter}\"!"
					],
					includeLetter: [
						"¡no incluye la \"{letter}\" de la palabra anterior!"
					],
					alreadyPlayed: [
						"¡esa me suena que ya la vimos!",
						"¡palabra vista!",
						"¡no nos sirven repetidas!"
					],
					doesntExist: [
						"¿es castellano eso?",
						"¡esa palabra no me suena!",
						"¿y eso qué significa?"
					],
					timeOut: [
						"¡se acabó el tiempo!",
						"¡sin tiempo!",
						"¡no hay más tiempo!"
					]
				},
				roundEnd: {
					point: [
						"¡punto excepcional para {winner}!",
						"¡{loser} pierde su oportunidad!",
						"¡mala suerte!",
						"¡qué vergüenza!",
						"¡{winner} dilata la cuenta!",
						"¡casi!",
						"¡{loser} no la sabe colocar!",
						"¡{winner} se lleva este punto!",
						"¡{winner} la pone bien!"
					],
					winning: [
						"¡{winner} por la delantera!",
						"¡{winner} lleva la ventaja!",
						"¡mejor que se apure {loser}!",
						"¡se le ve mal a {loser}!"
					],
					tie: [
						"¡iguales!",
						"¡{points} iguales!",
						"¡una batalla de narices!"
					]
				},
				scoreTally: [
					"el partido va {one} {pointsOne}, {two} {pointsTwo}.",
					"el marcador lee {one} {pointsOne} contra {two} {pointsTwo}.",
					"{one} con {pointsOne}, {two} con {pointsTwo}."
				],
				gameEnd: "¡y se acabó! {winner} vence a {loser} {winnerPoints} a {loserPoints} en una contienda histórica. " +
					"casi no podemos esperar hasta la próxima ocasión en que estos gigantes se vuelvan a enfrentar. " +
					"¡nos vemos!"
			}
		}
	});

})(this);