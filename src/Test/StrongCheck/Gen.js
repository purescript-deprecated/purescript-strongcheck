exports.float32ToInt32 = function(n) {
    var arr = new ArrayBuffer(4),
        fv = new Float32Array(arr),
        iv = new Int32Array(arr);
    fv[0] = n;
    return iv[0];
};
