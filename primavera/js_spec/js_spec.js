#! /usr/bin/env node

let argv = process.argv;


const ops = {
    add: (x,y) => x + y,
    sub: (x,y) => x - y,
    ceil: (x) => Math.ceil(x),
}

// source: https://stackoverflow.com/questions/70075794/javascript-convert-hex-to-ieee-754-64-bit-double-precision
const hex_to_number = (str) => {
  // Pad the string with zeroes to 16 characters.
  // You can omit this if you control your inputs.
  str = (str + "0000000000000000").slice(0,16);

  // Split into bits: sign (1), exponent (11), significand (52).
  var sign_and_exponent_bits = parseInt(str.slice(0,3), 16);
  var sign = sign_and_exponent_bits >= 0x800 ? -1 : +1;
  var exponent_bits = sign_and_exponent_bits & ((1<<11) - 1);
  var significand_bits = parseInt(str.slice(3,16), 16);

  // Classify the floating-point value.
  if (exponent_bits == 0x7FF)  // infinity | not a number
    return significand_bits == 0 ? sign * Number.POSITIVE_INFINITY : Number.NaN;
  else if (exponent_bits == 0)  // zero | subnormal number
    return sign * Math.pow(2, 1-1023-52) * significand_bits;
  else  // normal number
    return sign * Math.pow(2, exponent_bits-1023-52) * (Math.pow(2, 52) + significand_bits);
}

const number_to_hex = (n) => {
    const byte_buffer = new ArrayBuffer(8);
    const float_side = new Float64Array(byte_buffer);
    float_side[0] = n;


    const bytes = new Uint8Array(byte_buffer).reverse();
    let out = "";
    for (let b of bytes) {
        out += b.toString(16).padStart(2, '0');
    }
    return out
}


console.log(number_to_hex(ops[argv[2]].apply(null, argv.slice(3).map(hex_to_number))));
400000000