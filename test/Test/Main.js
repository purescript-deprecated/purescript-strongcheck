exports.throwIfInputIsA = function (c) {
  if (c === "A") {
      throw new Error("OOPS");
  }
  return true
}