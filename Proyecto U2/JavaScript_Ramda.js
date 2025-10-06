const R = require('ramda');

const logs = [
  "Error al conectar con la base de datos",
  "Conexión exitosa al servidor",
  "Error al leer el archivo",
  "Conexión cerrada correctamente"
];

// Funciones puras y composición
const limpiarTexto = R.pipe(
  R.toLower,
  R.replace(/al|con/g, ''),
  R.split(' ')
);

const todasPalabras = R.chain(limpiarTexto, logs);
const contarPalabras = R.countBy(R.identity, todasPalabras);

console.log("Conteo de palabras:", contarPalabras);
