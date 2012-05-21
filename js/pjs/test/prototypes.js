function Vector3(opt_x, opt_y, opt_z) {
    this.set(opt_x || 0, opt_y || 0, opt_z || 0);
}

Vector3.prototype.set = function(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;
};

var t = fork(function() {
    return new Vector3(1, 2, 3);
});
oncompletion(function() {
    var v = t.get();
    assert(v.x == 1, "x");
    assert(v.y == 2, "y");
    assert(v.z == 3, "z");
    print([v.x, v.y, v.z]);
});
