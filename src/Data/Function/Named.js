exports.unsafeMergeImpl = function(r1) {
  return function(r2) {
    var r = {};
    for (var k1 in r2) {
      if ({}.hasOwnProperty.call(r2, k1)) {
         r[k1] = r2[k1];
       }
     }
     for (var k2 in r1) {
       if ({}.hasOwnProperty.call(r1, k2)) {
         r[k2] = r1[k2];
       }
     }
     return r;
  }
}
