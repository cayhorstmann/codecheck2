// From https://github.com/angular/angular.js/blob/master/src/Angular.js
function isDate(value) {
  return toString.call(value) === '[object Date]';
}
function isRegExp(value) {
  return toString.call(value) === '[object RegExp]';
}
function isFunction(value) {return typeof value === 'function';}

function deepEquals(o1, o2) {
  if (o1 === o2) return true;
  if (o1 === null || o2 === null) return false;
  // eslint-disable-next-line no-self-compare
  if (o1 !== o1 && o2 !== o2) return true; // NaN === NaN
  var t1 = typeof o1, t2 = typeof o2, length, key, keySet;
  if (t1 === t2 && t1 === 'object') {
    if (Array.isArray(o1)) {
      if (!Array.isArray(o2)) return false;
      if ((length = o1.length) === o2.length) {
        for (key = 0; key < length; key++) {
          if (!deepEquals(o1[key], o2[key])) return false;
        }
        return true;
      }
    } else if (isDate(o1)) {
      if (!isDate(o2)) return false;
      return deepEquals(o1.getTime(), o2.getTime());
    } else if (isRegExp(o1)) {
      if (!isRegExp(o2)) return false;
      return o1.toString() === o2.toString();
    } else {
      if (Array.isArray(o2) || isDate(o2) || isRegExp(o2)) return false;
      keySet = createMap();
      for (key in o1) {
        if (key.charAt(0) === '$' || isFunction(o1[key])) continue;
        if (!deepEquals(o1[key], o2[key])) return false;
        keySet[key] = true;
      }
      for (key in o2) {
        if (!(key in keySet) &&
            key.charAt(0) !== '$' &&
            isDefined(o2[key]) &&
            !isFunction(o2[key])) return false;
      }
      return true;
    }
  }
  return false;
}

exports.deepEquals = deepEquals


