/** @constructor
*/
var TickerApp = function(){
/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function Fay$$_(thunkish,nocache){
  while (thunkish instanceof Fay$$$) {
    thunkish = thunkish.force(nocache);
  }
  return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function Fay$$__(){
  var f = arguments[0];
  for (var i = 1, len = arguments.length; i < len; i++) {
    f = (f instanceof Fay$$$? Fay$$_(f) : f)(arguments[i]);
  }
  return f;
}

// Thunk object.
function Fay$$$(value){
  this.forced = false;
  this.value = value;
}

// Force the thunk.
Fay$$$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};


function Fay$$seq(x) {
  return function(y) {
    Fay$$_(x,false);
    return y;
  }
}

function Fay$$seq$36$uncurried(x,y) {
  Fay$$_(x,false);
  return y;
}

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
  this.value = value;
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then(a){
  return function(b){
    return Fay$$bind(a)(function(_){
      return b;
    });
  };
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then$36$uncurried(a,b){
  return Fay$$bind$36$uncurried(a,function(_){ return b; });
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind(m){
  return function(f){
    return new Fay$$$(function(){
      var monad = Fay$$_(m,true);
      return Fay$$_(f)(monad.value);
    });
  };
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind$36$uncurried(m,f){
  return new Fay$$$(function(){
    var monad = Fay$$_(m,true);
    return Fay$$_(f)(monad.value);
  });
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$$_return(a){
  return new Fay$$Monad(a);
}

// Allow the programmer to access thunk forcing directly.
function Fay$$force(thunk){
  return function(type){
    return new Fay$$$(function(){
      Fay$$_(thunk,type);
      return new Fay$$Monad(Fay$$unit);
    })
  }
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$return$36$uncurried(a){
  return new Fay$$Monad(a);
}

// Unit: ().
var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
  var base = type[0];
  var args = type[1];
  var jsObj;
  if(base == "action") {
    // A nullary monadic action. Should become a nullary JS function.
    // Fay () -> function(){ return ... }
    jsObj = function(){
      return Fay$$fayToJs(args[0],Fay$$_(fayObj,true).value);
    };

  }
  else if(base == "function") {
    // A proper function.
    jsObj = function(){
      var fayFunc = fayObj;
      var return_type = args[args.length-1];
      var len = args.length;
      // If some arguments.
      if (len > 1) {
        // Apply to all the arguments.
        fayFunc = Fay$$_(fayFunc,true);
        // TODO: Perhaps we should throw an error when JS
        // passes more arguments than Haskell accepts.
        for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
          // Unserialize the JS values to Fay for the Fay callback.
          fayFunc = Fay$$_(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
        }
        // Finally, serialize the Fay return value back to JS.
        var return_base = return_type[0];
        var return_args = return_type[1];
        // If it's a monadic return value, get the value instead.
        if(return_base == "action") {
          return Fay$$fayToJs(return_args[0],fayFunc.value);
        }
        // Otherwise just serialize the value direct.
        else {
          return Fay$$fayToJs(return_type,fayFunc);
        }
      } else {
        throw new Error("Nullary function?");
      }
    };

  }
  else if(base == "string") {
    jsObj = Fay$$fayToJs_string(fayObj);
  }
  else if(base == "list") {
    // Serialize Fay list to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[0],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    jsObj = arr;

  }
  else if(base == "tuple") {
    // Serialize Fay tuple to JavaScript array.
    var arr = [];
    fayObj = Fay$$_(fayObj);
    var i = 0;
    while(fayObj instanceof Fay$$Cons) {
      arr.push(Fay$$fayToJs(args[i++],fayObj.car));
      fayObj = Fay$$_(fayObj.cdr);
    }
    jsObj = arr;

  }
  else if(base == "defined") {
    fayObj = Fay$$_(fayObj);
    if (fayObj instanceof $_Language$Fay$FFI$Undefined) {
      jsObj = undefined;
    } else {
      jsObj = Fay$$fayToJsUserDefined(args[0],fayObj["slot1"]);
    }

  }
  else if(base == "nullable") {
    fayObj = Fay$$_(fayObj);
    if (fayObj instanceof $_Language$Fay$FFI$Null) {
      jsObj = null;
    } else {
      jsObj = Fay$$fayToJsUserDefined(args[0],fayObj["slot1"]);
    }

  }
  else if(base == "double" || base == "int" || base == "bool") {
    // Bools are unboxed.
    jsObj = Fay$$_(fayObj);

  }
  else if(base == "ptr" || base == "unknown")
    return fayObj;
  else if(base == "automatic" || base == "user") {
    if(fayObj instanceof Fay$$$)
      fayObj = Fay$$_(fayObj);
    jsObj = Fay$$fayToJsUserDefined(type,fayObj);

  }
  else
    throw new Error("Unhandled Fay->JS translation type: " + base);
  return jsObj;
}

// Specialized serializer for string.
function Fay$$fayToJs_string(fayObj){
  // Serialize Fay string to JavaScript string.
  var str = "";
  fayObj = Fay$$_(fayObj);
  while(fayObj instanceof Fay$$Cons) {
    str += fayObj.car;
    fayObj = Fay$$_(fayObj.cdr);
  }
  return str;
};
function Fay$$jsToFay_string(x){
  return Fay$$list(x)
};

// Special num/bool serializers.
function Fay$$jsToFay_int(x){return x;}
function Fay$$jsToFay_double(x){return x;}
function Fay$$jsToFay_bool(x){return x;}

function Fay$$fayToJs_int(x){return Fay$$_(x);}
function Fay$$fayToJs_double(x){return Fay$$_(x);}
function Fay$$fayToJs_bool(x){return Fay$$_(x);}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
  var base = type[0];
  var args = type[1];
  var fayObj;
  if(base == "action") {
    // Unserialize a "monadic" JavaScript return value into a monadic value.
    fayObj = new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));

  }
  else if(base == "string") {
    // Unserialize a JS string into Fay list (String).
    fayObj = Fay$$list(jsObj);
  }
  else if(base == "list") {
    // Unserialize a JS array into a Fay list ([a]).
    var serializedList = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
    }
    // Pop it all in a Fay list.
    fayObj = Fay$$list(serializedList);

  }
  else if(base == "tuple") {
    // Unserialize a JS array into a Fay tuple ((a,b,c,...)).
    var serializedTuple = [];
    for (var i = 0, len = jsObj.length; i < len; i++) {
      // Unserialize each JS value into a Fay value, too.
      serializedTuple.push(Fay$$jsToFay(args[i],jsObj[i]));
    }
    // Pop it all in a Fay list.
    fayObj = Fay$$list(serializedTuple);

  }
  else if(base == "defined") {
    if (jsObj === undefined) {
      fayObj = new $_Language$Fay$FFI$Undefined();
    } else {
      fayObj = new $_Language$Fay$FFI$Defined(Fay$$jsToFay(args[0],jsObj));
    }

  }
  else if(base == "nullable") {
    if (jsObj === null) {
      fayObj = new $_Language$Fay$FFI$Null();
    } else {
      fayObj = new $_Language$Fay$FFI$Nullable(Fay$$jsToFay(args[0],jsObj));
    }

  }
  else if(base == "int") {
    // Int are unboxed, so there's no forcing to do.
    // But we can do validation that the int has no decimal places.
    // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
    fayObj = Math.round(jsObj);
    if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!";

  }
  else if (base == "double" ||
           base == "bool" ||
           base ==  "ptr" ||
           base ==  "unknown") {
    return jsObj;
  }
  else if(base == "automatic" || base == "user") {
    if (jsObj && jsObj['instance']) {
      fayObj = Fay$$jsToFayUserDefined(type,jsObj);
    }
    else
      fayObj = jsObj;

  }
  else { throw new Error("Unhandled JS->Fay translation type: " + base); }
  return fayObj;
}

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
  this.car = car;
  this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
  var out = null;
  for(var i=xs.length-1; i>=0;i--)
    out = new Fay$$Cons(xs[i],out);
  return out;
}

// Built-in list cons.
function Fay$$cons(x){
  return function(y){
    return new Fay$$Cons(x,y);
  };
}

// List index.
// `list' is already forced by the time it's passed to this function.
// `list' cannot be null and `index' cannot be out of bounds.
function Fay$$index(index,list){
  for(var i = 0; i < index; i++) {
    list = Fay$$_(list.cdr);
  }
  return list.car;
}

// List length.
// `list' is already forced by the time it's passed to this function.
function Fay$$listLen(list,max){
  for(var i = 0; list !== null && i < max + 1; i++) {
    list = Fay$$_(list.cdr);
  }
  return i == max;
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) * Fay$$_(y);
    });
  };
}

function Fay$$mult$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) * Fay$$_(y);
  });

}

// Built-in +.
function Fay$$add(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) + Fay$$_(y);
    });
  };
}

// Built-in +.
function Fay$$add$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) + Fay$$_(y);
  });

}

// Built-in -.
function Fay$$sub(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) - Fay$$_(y);
    });
  };
}
// Built-in -.
function Fay$$sub$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) - Fay$$_(y);
  });

}

// Built-in /.
function Fay$$divi(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) / Fay$$_(y);
    });
  };
}

// Built-in /.
function Fay$$divi$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) / Fay$$_(y);
  });

}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
  // Simple case
  lit1 = Fay$$_(lit1);
  lit2 = Fay$$_(lit2);
  if (lit1 === lit2) {
    return true;
  }
  // General case
  if (lit1 instanceof Array) {
    if (lit1.length != lit2.length) return false;
    for (var len = lit1.length, i = 0; i < len; i++) {
      if (!Fay$$equal(lit1[i], lit2[i])) return false;
    }
    return true;
  } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
    do {
      if (!Fay$$equal(lit1.car,lit2.car))
        return false;
      lit1 = Fay$$_(lit1.cdr), lit2 = Fay$$_(lit2.cdr);
      if (lit1 === null || lit2 === null)
        return lit1 === lit2;
    } while (true);
  } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
             lit1.constructor === lit2.constructor) {
    for(var x in lit1) {
      if(!(lit1.hasOwnProperty(x) && lit2.hasOwnProperty(x) &&
           Fay$$equal(lit1[x],lit2[x])))
        return false;
    }
    return true;
  } else {
    return false;
  }
}

// Built-in ==.
function Fay$$eq(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$equal(x,y);
    });
  };
}

function Fay$$eq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$equal(x,y);
  });

}

// Built-in /=.
function Fay$$neq(x){
  return function(y){
    return new Fay$$$(function(){
      return !(Fay$$equal(x,y));
    });
  };
}

// Built-in /=.
function Fay$$neq$36$uncurried(x,y){

  return new Fay$$$(function(){
    return !(Fay$$equal(x,y));
  });

}

// Built-in >.
function Fay$$gt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) > Fay$$_(y);
    });
  };
}

// Built-in >.
function Fay$$gt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) > Fay$$_(y);
  });

}

// Built-in <.
function Fay$$lt(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) < Fay$$_(y);
    });
  };
}


// Built-in <.
function Fay$$lt$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) < Fay$$_(y);
  });

}


// Built-in >=.
function Fay$$gte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) >= Fay$$_(y);
    });
  };
}

// Built-in >=.
function Fay$$gte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) >= Fay$$_(y);
  });

}

// Built-in <=.
function Fay$$lte(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) <= Fay$$_(y);
    });
  };
}

// Built-in <=.
function Fay$$lte$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) <= Fay$$_(y);
  });

}

// Built-in &&.
function Fay$$and(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) && Fay$$_(y);
    });
  };
}

// Built-in &&.
function Fay$$and$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) && Fay$$_(y);
  });
  ;
}

// Built-in ||.
function Fay$$or(x){
  return function(y){
    return new Fay$$$(function(){
      return Fay$$_(x) || Fay$$_(y);
    });
  };
}

// Built-in ||.
function Fay$$or$36$uncurried(x,y){

  return new Fay$$$(function(){
    return Fay$$_(x) || Fay$$_(y);
  });

}

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
  this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
  ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref,x){
  return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
  return window.Date.parse(str);
}

/*******************************************************************************
 * Application code.
 */

var Language$Fay$FFI$Nullable = function(slot1){return new Fay$$$(function(){return new $_Language$Fay$FFI$Nullable(slot1);});};var Language$Fay$FFI$Null = new Fay$$$(function(){return new $_Language$Fay$FFI$Null();});var Language$Fay$FFI$Defined = function(slot1){return new Fay$$$(function(){return new $_Language$Fay$FFI$Defined(slot1);});};var Language$Fay$FFI$Undefined = new Fay$$$(function(){return new $_Language$Fay$FFI$Undefined();});var Prelude$Just = function(slot1){return new Fay$$$(function(){return new $_Prelude$Just(slot1);});};var Prelude$Nothing = new Fay$$$(function(){return new $_Prelude$Nothing();});var Prelude$Left = function(slot1){return new Fay$$$(function(){return new $_Prelude$Left(slot1);});};var Prelude$Right = function(slot1){return new Fay$$$(function(){return new $_Prelude$Right(slot1);});};var Prelude$maybe = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) instanceof $_Prelude$Nothing) {var m = $p1;return m;}if (Fay$$_($p3) instanceof $_Prelude$Just) {var x = Fay$$_($p3).slot1;var f = $p2;return Fay$$_(f)(x);}throw ["unhandled case in maybe",[$p1,$p2,$p3]];});};};};var Prelude$Ratio = function(slot1){return function(slot2){return new Fay$$$(function(){return new $_Prelude$Ratio(slot1,slot2);});};};var Prelude$$62$$62$$61$ = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$bind(Fay$$fayToJs(["action",[["unknown"]]],$p1))(Fay$$fayToJs(["function",[["unknown"],["action",[["unknown"]]]]],$p2))));});};};var Prelude$$62$$62$ = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$then(Fay$$fayToJs(["action",[["unknown"]]],$p1))(Fay$$fayToJs(["action",[["unknown"]]],$p2))));});};};var Prelude$$_return = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$return(Fay$$fayToJs(["unknown"],$p1))));});};var Prelude$when = function($p1){return function($p2){return new Fay$$$(function(){var m = $p2;var p = $p1;return Fay$$_(p) ? Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Fay$$$_return)(Fay$$unit)) : Fay$$_(Fay$$$_return)(Fay$$unit);});};};var Prelude$forM_ = function($p1){return function($p2){return new Fay$$$(function(){var m = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude$forM_)(xs))(m));}if (Fay$$_($p1) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in forM_",[$p1,$p2]];});};};var Prelude$mapM_ = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var m = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(m)(x)))(Fay$$_(Fay$$_(Prelude$mapM_)(m))(xs));}if (Fay$$_($p2) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in mapM_",[$p1,$p2]];});};};var Prelude$$61$$60$$60$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$bind)(x))(f);});};};var Prelude$sequence = function($p1){return new Fay$$$(function(){var ms = $p1;return (function(){var k = function($p1){return function($p2){return new Fay$$$(function(){var m$39$ = $p2;var m = $p1;return Fay$$_(Fay$$_(Fay$$bind)(m))(function($p1){var x = $p1;return Fay$$_(Fay$$_(Fay$$bind)(m$39$))(function($p1){var xs = $p1;return Fay$$_(Fay$$$_return)(Fay$$_(Fay$$_(Fay$$cons)(x))(xs));});});});};};return Fay$$_(Fay$$_(Fay$$_(Prelude$foldr)(k))(Fay$$_(Fay$$$_return)(null)))(ms);})();});};var Prelude$sequence_ = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var m = $tmp1.car;var ms = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$then)(m))(Fay$$_(Prelude$sequence_)(ms));}throw ["unhandled case in sequence_",[$p1]];});};var Prelude$GT = new Fay$$$(function(){return new $_Prelude$GT();});var Prelude$LT = new Fay$$$(function(){return new $_Prelude$LT();});var Prelude$EQ = new Fay$$$(function(){return new $_Prelude$EQ();});var Prelude$compare = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(y)) ? Prelude$GT : Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(y)) ? Prelude$LT : Prelude$EQ;});};};var Prelude$succ = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$add)(x))(1);});};var Prelude$pred = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$sub)(x))(1);});};var Prelude$enumFrom = function($p1){return new Fay$$$(function(){var i = $p1;return Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Prelude$enumFrom)(Fay$$_(Fay$$_(Fay$$add)(i))(1)));});};var Prelude$enumFromTo = function($p1){return function($p2){return new Fay$$$(function(){var n = $p2;var i = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(i))(n)) ? null : Fay$$_(Fay$$_(Fay$$cons)(i))(Fay$$_(Fay$$_(Prelude$enumFromTo)(Fay$$_(Fay$$_(Fay$$add)(i))(1)))(n));});};};var Prelude$enumFromBy = function($p1){return function($p2){return new Fay$$$(function(){var by = $p2;var fr = $p1;return Fay$$_(Fay$$_(Fay$$cons)(fr))(Fay$$_(Fay$$_(Prelude$enumFromBy)(Fay$$_(Fay$$_(Fay$$add)(fr))(by)))(by));});};};var Prelude$enumFromThen = function($p1){return function($p2){return new Fay$$$(function(){var th = $p2;var fr = $p1;return Fay$$_(Fay$$_(Prelude$enumFromBy)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr));});};};var Prelude$enumFromByTo = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var to = $p3;var by = $p2;var fr = $p1;return (function(){var neg = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(neg)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));});};var pos = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(to)) ? null : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(pos)(Fay$$_(Fay$$_(Fay$$add)(x))(by)));});};return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(by))(0)) ? Fay$$_(neg)(fr) : Fay$$_(pos)(fr);})();});};};};var Prelude$enumFromThenTo = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var to = $p3;var th = $p2;var fr = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude$enumFromByTo)(fr))(Fay$$_(Fay$$_(Fay$$sub)(th))(fr)))(to);});};};};var Prelude$fromIntegral = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Fay$$fayToJs_int($p1));});};var Prelude$fromInteger = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Fay$$fayToJs_int($p1));});};var Prelude$not = function($p1){return new Fay$$$(function(){var p = $p1;return Fay$$_(p) ? false : true;});};var Prelude$otherwise = true;var Prelude$show = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_string(JSON.stringify(Fay$$fayToJs(["automatic"],$p1)));});};var Prelude$error = function($p1){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],(function() { throw Fay$$fayToJs_string($p1) })());});};var Prelude$$_undefined = new Fay$$$(function(){return Fay$$_(Prelude$error)(Fay$$list("Prelude.undefined"));});var Prelude$either = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) instanceof $_Prelude$Left) {var a = Fay$$_($p3).slot1;var f = $p1;return Fay$$_(f)(a);}if (Fay$$_($p3) instanceof $_Prelude$Right) {var b = Fay$$_($p3).slot1;var g = $p2;return Fay$$_(g)(b);}throw ["unhandled case in either",[$p1,$p2,$p3]];});};};};var Prelude$until = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var f = $p2;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? x : Fay$$_(Fay$$_(Fay$$_(Prelude$until)(p))(f))(Fay$$_(f)(x));});};};};var Prelude$$36$$33$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$seq)(x))(Fay$$_(f)(x));});};};var Prelude$$_const = function($p1){return function($p2){return new Fay$$$(function(){var a = $p1;return a;});};};var Prelude$id = function($p1){return new Fay$$$(function(){var x = $p1;return x;});};var Prelude$$46$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var x = $p3;var g = $p2;var f = $p1;return Fay$$_(f)(Fay$$_(g)(x));});};};};var Prelude$$36$ = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(f)(x);});};};var Prelude$flip = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var y = $p3;var x = $p2;var f = $p1;return Fay$$_(Fay$$_(f)(y))(x);});};};};var Prelude$curry = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var y = $p3;var x = $p2;var f = $p1;return Fay$$_(f)(Fay$$list([x,y]));});};};};var Prelude$uncurry = function($p1){return function($p2){return new Fay$$$(function(){var p = $p2;var f = $p1;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var x = Fay$$index(0,Fay$$_($tmp1));var y = Fay$$index(1,Fay$$_($tmp1));return Fay$$_(Fay$$_(f)(x))(y);}return (function(){ throw (["unhandled case",$tmp1]); })();})(p);});};};var Prelude$snd = function($p1){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p1),2)) {var x = Fay$$index(1,Fay$$_($p1));return x;}throw ["unhandled case in snd",[$p1]];});};var Prelude$fst = function($p1){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p1),2)) {var x = Fay$$index(0,Fay$$_($p1));return x;}throw ["unhandled case in fst",[$p1]];});};var Prelude$div = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude$quot)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(1);} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude$quot)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(1);}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude$quot)(x))(y);});};};var Prelude$mod = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude$rem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y)))(y)))(1);} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(0)))) {return Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude$rem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y)))(y)))(1);}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude$rem)(x))(y);});};};var Prelude$divMod = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$gt)(x))(0)))(Fay$$_(Fay$$_(Fay$$lt)(y))(0)))) {return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var q = Fay$$index(0,Fay$$_($tmp1));var r = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude$quotRem)(Fay$$_(Fay$$_(Fay$$sub)(x))(1)))(y));} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$and)(Fay$$_(Fay$$_(Fay$$lt)(x))(0)))(Fay$$_(Fay$$_(Fay$$gt)(y))(1)))) {return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var q = Fay$$index(0,Fay$$_($tmp1));var r = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$sub)(q))(1),Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Fay$$add)(r))(y)))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude$quotRem)(Fay$$_(Fay$$_(Fay$$add)(x))(1)))(y));}}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude$quotRem)(x))(y);});};};var Prelude$min = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.min(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));});};};var Prelude$max = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.max(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));});};};var Prelude$recip = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(1))(x);});};var Prelude$negate = function($p1){return new Fay$$$(function(){var x = $p1;return (-(Fay$$_(x)));});};var Prelude$abs = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude$negate)(x) : x;});};var Prelude$signum = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$gt)(x))(0)) ? 1 : Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(x))(0)) ? 0 : (-(1));});};var Prelude$pi = new Fay$$$(function(){return Fay$$jsToFay_double(Math.PI);});var Prelude$exp = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.exp(Fay$$fayToJs_double($p1)));});};var Prelude$sqrt = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.sqrt(Fay$$fayToJs_double($p1)));});};var Prelude$log = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.log(Fay$$fayToJs_double($p1)));});};var Prelude$$42$$42$ = new Fay$$$(function(){return Prelude$unsafePow;});var Prelude$$94$$94$ = new Fay$$$(function(){return Prelude$unsafePow;});var Prelude$unsafePow = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay(["unknown"],Math.pow(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));});};};var Prelude$$94$ = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0))) {return Fay$$_(Prelude$error)(Fay$$list("(^): negative exponent"));} else {if (Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(b))(0))) {return 1;} else {if (Fay$$_(Fay$$_(Prelude$even)(b))) {return (function(){var x = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$$94$)(a))(Fay$$_(Fay$$_(Prelude$quot)(b))(2));});return Fay$$_(Fay$$_(Fay$$mult)(x))(x);})();}}}var b = $p2;var a = $p1;return Fay$$_(Fay$$_(Fay$$mult)(a))(Fay$$_(Fay$$_(Prelude$$94$)(a))(Fay$$_(Fay$$_(Fay$$sub)(b))(1)));});};};var Prelude$logBase = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var b = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude$log)(x)))(Fay$$_(Prelude$log)(b));});};};var Prelude$sin = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.sin(Fay$$fayToJs_double($p1)));});};var Prelude$tan = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.tan(Fay$$fayToJs_double($p1)));});};var Prelude$cos = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.cos(Fay$$fayToJs_double($p1)));});};var Prelude$asin = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.asin(Fay$$fayToJs_double($p1)));});};var Prelude$atan = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.atan(Fay$$fayToJs_double($p1)));});};var Prelude$acos = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_double(Math.acos(Fay$$fayToJs_double($p1)));});};var Prelude$sinh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Prelude$exp)(x)))(Fay$$_(Prelude$exp)((-(Fay$$_(x)))))))(2);});};var Prelude$tanh = function($p1){return new Fay$$$(function(){var x = $p1;return (function(){var a = new Fay$$$(function(){return Fay$$_(Prelude$exp)(x);});var b = new Fay$$$(function(){return Fay$$_(Prelude$exp)((-(Fay$$_(x))));});return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$sub)(a))(b)))(Fay$$_(Fay$$_(Fay$$add)(a))(b));})();});};var Prelude$cosh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Prelude$exp)(x)))(Fay$$_(Prelude$exp)((-(Fay$$_(x)))))))(2);});};var Prelude$asinh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude$log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude$sqrt)(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude$$42$$42$)(x))(2)))(1))));});};var Prelude$atanh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude$log)(Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$add)(1))(x)))(Fay$$_(Fay$$_(Fay$$sub)(1))(x)))))(2);});};var Prelude$acosh = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude$log)(Fay$$_(Fay$$_(Fay$$add)(x))(Fay$$_(Prelude$sqrt)(Fay$$_(Fay$$_(Fay$$sub)(Fay$$_(Fay$$_(Prelude$$42$$42$)(x))(2)))(1))));});};var Prelude$properFraction = function($p1){return new Fay$$$(function(){var x = $p1;return (function(){var a = new Fay$$$(function(){return Fay$$_(Prelude$truncate)(x);});return Fay$$list([a,Fay$$_(Fay$$_(Fay$$sub)(x))(Fay$$_(Prelude$fromIntegral)(a))]);})();});};var Prelude$truncate = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(x))(0)) ? Fay$$_(Prelude$ceiling)(x) : Fay$$_(Prelude$floor)(x);});};var Prelude$round = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.round(Fay$$fayToJs_double($p1)));});};var Prelude$ceiling = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.ceil(Fay$$fayToJs_double($p1)));});};var Prelude$floor = function($p1){return new Fay$$$(function(){return Fay$$jsToFay_int(Math.floor(Fay$$fayToJs_double($p1)));});};var Prelude$subtract = new Fay$$$(function(){return Fay$$_(Prelude$flip)(Fay$$sub);});var Prelude$even = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$eq)(Fay$$_(Fay$$_(Prelude$rem)(x))(2)))(0);});};var Prelude$odd = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Prelude$not)(Fay$$_(Prelude$even)(x));});};var Prelude$gcd = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === 0) {var x = $p1;return x;}var y = $p2;var x = $p1;return Fay$$_(Fay$$_(go)(y))(Fay$$_(Fay$$_(Prelude$rem)(x))(y));});};};return Fay$$_(Fay$$_(go)(Fay$$_(Prelude$abs)(a)))(Fay$$_(Prelude$abs)(b));})();});};};var Prelude$quot = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude$error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude$quot$39$)(x))(y);});};};var Prelude$quot$39$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay_int(~~(Fay$$fayToJs_int($p1)/Fay$$fayToJs_int($p2)));});};};var Prelude$quotRem = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$list([Fay$$_(Fay$$_(Prelude$quot)(x))(y),Fay$$_(Fay$$_(Prelude$rem)(x))(y)]);});};};var Prelude$rem = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(y))(0)) ? Fay$$_(Prelude$error)(Fay$$list("Division by zero")) : Fay$$_(Fay$$_(Prelude$rem$39$)(x))(y);});};};var Prelude$rem$39$ = function($p1){return function($p2){return new Fay$$$(function(){return Fay$$jsToFay_int(Fay$$fayToJs_int($p1) % Fay$$fayToJs_int($p2));});};};var Prelude$lcm = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === 0) {return 0;}if (Fay$$_($p1) === 0) {return 0;}var b = $p2;var a = $p1;return Fay$$_(Prelude$abs)(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Prelude$quot)(a))(Fay$$_(Fay$$_(Prelude$gcd)(a))(b))))(b));});};};var Prelude$find = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Prelude$Just)(x) : Fay$$_(Fay$$_(Prelude$find)(p))(xs);}if (Fay$$_($p2) === null) {return Prelude$Nothing;}throw ["unhandled case in find",[$p1,$p2]];});};};var Prelude$filter = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$filter)(p))(xs)) : Fay$$_(Fay$$_(Prelude$filter)(p))(xs);}if (Fay$$_($p2) === null) {return null;}throw ["unhandled case in filter",[$p1,$p2]];});};};var Prelude$$_null = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return true;}return false;});};var Prelude$map = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(f)(x)))(Fay$$_(Fay$$_(Prelude$map)(f))(xs));}throw ["unhandled case in map",[$p1,$p2]];});};};var Prelude$nub = function($p1){return new Fay$$$(function(){var ls = $p1;return Fay$$_(Fay$$_(Prelude$nub$39$)(ls))(null);});};var Prelude$nub$39$ = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var ls = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$_(Prelude$elem)(x))(ls)) ? Fay$$_(Fay$$_(Prelude$nub$39$)(xs))(ls) : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$nub$39$)(xs))(Fay$$_(Fay$$_(Fay$$cons)(x))(ls)));}throw ["unhandled case in nub'",[$p1,$p2]];});};};var Prelude$elem = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var y = $tmp1.car;var ys = $tmp1.cdr;var x = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(x))(y)))(Fay$$_(Fay$$_(Prelude$elem)(x))(ys));}if (Fay$$_($p2) === null) {return false;}throw ["unhandled case in elem",[$p1,$p2]];});};};var Prelude$notElem = function($p1){return function($p2){return new Fay$$$(function(){var ys = $p2;var x = $p1;return Fay$$_(Prelude$not)(Fay$$_(Fay$$_(Prelude$elem)(x))(ys));});};};var Prelude$sort = new Fay$$$(function(){return Fay$$_(Prelude$sortBy)(Prelude$compare);});var Prelude$sortBy = function($p1){return new Fay$$$(function(){var cmp = $p1;return Fay$$_(Fay$$_(Prelude$foldr)(Fay$$_(Prelude$insertBy)(cmp)))(null);});};var Prelude$insertBy = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var x = $p2;return Fay$$list([x]);}var ys = $p3;var x = $p2;var cmp = $p1;return (function($tmp1){if (Fay$$_($tmp1) === null) {return Fay$$list([x]);}var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var y = $tmp2.car;var ys$39$ = $tmp2.cdr;return (function($tmp2){if (Fay$$_($tmp2) instanceof $_Prelude$GT) {return Fay$$_(Fay$$_(Fay$$cons)(y))(Fay$$_(Fay$$_(Fay$$_(Prelude$insertBy)(cmp))(x))(ys$39$));}return Fay$$_(Fay$$_(Fay$$cons)(x))(ys);})(Fay$$_(Fay$$_(cmp)(x))(y));}return (function(){ throw (["unhandled case",$tmp1]); })();})(ys);});};};};var Prelude$conc = function($p1){return function($p2){return new Fay$$$(function(){var ys = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$conc)(xs))(ys));}var ys = $p2;if (Fay$$_($p1) === null) {return ys;}throw ["unhandled case in conc",[$p1,$p2]];});};};var Prelude$concat = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$foldr)(Prelude$conc))(null);});var Prelude$concatMap = function($p1){return new Fay$$$(function(){var f = $p1;return Fay$$_(Fay$$_(Prelude$foldr)(Fay$$_(Fay$$_(Prelude$$46$)(Prelude$$43$$43$))(f)))(null);});};var Prelude$foldr = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return z;}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Fay$$_(Prelude$foldr)(f))(z))(xs));}throw ["unhandled case in foldr",[$p1,$p2,$p3]];});};};};var Prelude$foldr1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p2),1)) {var x = Fay$$index(0,Fay$$_($p2));return x;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(f)(x))(Fay$$_(Fay$$_(Prelude$foldr1)(f))(xs));}if (Fay$$_($p2) === null) {return Fay$$_(Prelude$error)(Fay$$list("foldr1: empty list"));}throw ["unhandled case in foldr1",[$p1,$p2]];});};};var Prelude$foldl = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return z;}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude$foldl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);}throw ["unhandled case in foldl",[$p1,$p2,$p3]];});};};};var Prelude$foldl1 = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude$foldl)(f))(x))(xs);}if (Fay$$_($p2) === null) {return Fay$$_(Prelude$error)(Fay$$list("foldl1: empty list"));}throw ["unhandled case in foldl1",[$p1,$p2]];});};};var Prelude$$43$$43$ = function($p1){return function($p2){return new Fay$$$(function(){var y = $p2;var x = $p1;return Fay$$_(Fay$$_(Prelude$conc)(x))(y);});};};var Prelude$$33$$33$ = function($p1){return function($p2){return new Fay$$$(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("(!!): index too large"));}if (Fay$$_($p2) === 0) {var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}}var n = $p2;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return Fay$$_(Fay$$_(go)(t))(Fay$$_(Fay$$_(Fay$$sub)(n))(1));}throw ["unhandled case in go",[$p1,$p2]];});};};return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(b))(0)) ? Fay$$_(Prelude$error)(Fay$$list("(!!): negative index")) : Fay$$_(Fay$$_(go)(a))(b);})();});};};var Prelude$head = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("head: empty list"));}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}throw ["unhandled case in head",[$p1]];});};var Prelude$tail = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("tail: empty list"));}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return t;}throw ["unhandled case in tail",[$p1]];});};var Prelude$init = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("init: empty list"));}if (Fay$$listLen(Fay$$_($p1),1)) {var a = Fay$$index(0,Fay$$_($p1));return Fay$$list([a]);}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;var t = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(h))(Fay$$_(Prelude$init)(t));}throw ["unhandled case in init",[$p1]];});};var Prelude$last = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("last: empty list"));}if (Fay$$listLen(Fay$$_($p1),1)) {var a = Fay$$index(0,Fay$$_($p1));return a;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return Fay$$_(Prelude$last)(t);}throw ["unhandled case in last",[$p1]];});};var Prelude$iterate = function($p1){return function($p2){return new Fay$$$(function(){var x = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$iterate)(f))(Fay$$_(f)(x)));});};};var Prelude$repeat = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Prelude$repeat)(x));});};var Prelude$replicate = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === 0) {return null;}var x = $p2;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$_(Prelude$error)(Fay$$list("replicate: negative length")) : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$replicate)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(x));});};};var Prelude$cycle = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("cycle: empty list"));}var xs = $p1;return (function(){var xs$39$ = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$$43$$43$)(xs))(xs$39$);});return xs$39$;})();});};var Prelude$take = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p1) === 0) {return null;}if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$_(Prelude$error)(Fay$$list("take: negative length")) : Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$take)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));}throw ["unhandled case in take",[$p1,$p2]];});};};var Prelude$drop = function($p1){return function($p2){return new Fay$$$(function(){var xs = $p2;if (Fay$$_($p1) === 0) {return xs;}if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$_(Prelude$error)(Fay$$list("drop: negative length")) : Fay$$_(Fay$$_(Prelude$drop)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs);}throw ["unhandled case in drop",[$p1,$p2]];});};};var Prelude$splitAt = function($p1){return function($p2){return new Fay$$$(function(){var xs = $p2;if (Fay$$_($p1) === 0) {return Fay$$list([null,xs]);}if (Fay$$_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(0)) ? Fay$$_(Prelude$error)(Fay$$list("splitAt: negative length")) : (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude$splitAt)(Fay$$_(Fay$$_(Fay$$sub)(n))(1)))(xs));}throw ["unhandled case in splitAt",[$p1,$p2]];});};};var Prelude$takeWhile = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$takeWhile)(p))(xs)) : null;}throw ["unhandled case in takeWhile",[$p1,$p2]];});};};var Prelude$dropWhile = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? Fay$$_(Fay$$_(Prelude$dropWhile)(p))(xs) : Fay$$_(Fay$$_(Fay$$cons)(x))(xs);}throw ["unhandled case in dropWhile",[$p1,$p2]];});};};var Prelude$span = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(p)(x)) ? (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude$span)(p))(xs)) : Fay$$list([null,Fay$$_(Fay$$_(Fay$$cons)(x))(xs)]);}throw ["unhandled case in span",[$p1,$p2]];});};};var Prelude$$_break = function($p1){return new Fay$$$(function(){var p = $p1;return Fay$$_(Prelude$span)(Fay$$_(Fay$$_(Prelude$$46$)(Prelude$not))(p));});};var Prelude$zipWith = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(a))(b)))(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith)(f))(as))(bs));}}return null;});};};};var Prelude$zipWith3 = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var $tmp1 = Fay$$_($p4);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(Fay$$_(f)(a))(b))(c)))(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Prelude$zipWith3)(f))(as))(bs))(cs));}}}return null;});};};};};var Prelude$zip = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b])))(Fay$$_(Fay$$_(Prelude$zip)(as))(bs));}}return null;});};};var Prelude$zip3 = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$list([a,b,c])))(Fay$$_(Fay$$_(Fay$$_(Prelude$zip3)(as))(bs))(cs));}}}return null;});};};};var Prelude$unzip = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),2)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var xs = Fay$$index(0,Fay$$_($tmp1));var ys = Fay$$index(1,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Prelude$unzip)(ps));}}if (Fay$$_($p1) === null) {return Fay$$list([null,null]);}throw ["unhandled case in unzip",[$p1]];});};var Prelude$unzip3 = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),3)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var z = Fay$$index(2,Fay$$_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),3)) {var xs = Fay$$index(0,Fay$$_($tmp1));var ys = Fay$$index(1,Fay$$_($tmp1));var zs = Fay$$index(2,Fay$$_($tmp1));return Fay$$list([Fay$$_(Fay$$_(Fay$$cons)(x))(xs),Fay$$_(Fay$$_(Fay$$cons)(y))(ys),Fay$$_(Fay$$_(Fay$$cons)(z))(zs)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Prelude$unzip3)(ps));}}if (Fay$$_($p1) === null) {return Fay$$list([null,null,null]);}throw ["unhandled case in unzip3",[$p1]];});};var Prelude$lines = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var s = $p1;return (function(){var isLineBreak = function($p1){return new Fay$$$(function(){var c = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(Fay$$_(Fay$$eq)(c))("\r")))(Fay$$_(Fay$$_(Fay$$eq)(c))("\n"));});};return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));if (Fay$$_(Fay$$index(1,Fay$$_($tmp1))) === null) {return Fay$$list([a]);}var a = Fay$$index(0,Fay$$_($tmp1));var $tmp2 = Fay$$_(Fay$$index(1,Fay$$_($tmp1)));if ($tmp2 instanceof Fay$$Cons) {var cs = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude$lines)(cs));}}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude$$_break)(isLineBreak))(s));})();});};var Prelude$unlines = new Fay$$$(function(){return Fay$$_(Prelude$intercalate)(Fay$$list("\n"));});var Prelude$words = function($p1){return new Fay$$$(function(){var str = $p1;return (function(){var words$39$ = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return null;}var s = $p1;return (function($tmp1){if (Fay$$listLen(Fay$$_($tmp1),2)) {var a = Fay$$index(0,Fay$$_($tmp1));var b = Fay$$index(1,Fay$$_($tmp1));return Fay$$_(Fay$$_(Fay$$cons)(a))(Fay$$_(Prelude$words)(b));}return (function(){ throw (["unhandled case",$tmp1]); })();})(Fay$$_(Fay$$_(Prelude$$_break)(isSpace))(s));});};var isSpace = function($p1){return new Fay$$$(function(){var c = $p1;return Fay$$_(Fay$$_(Prelude$elem)(c))(Fay$$list(" \t\r\n\u000c\u000b"));});};return Fay$$_(words$39$)(Fay$$_(Fay$$_(Prelude$dropWhile)(isSpace))(str));})();});};var Prelude$unwords = new Fay$$$(function(){return Fay$$_(Prelude$intercalate)(Fay$$list(" "));});var Prelude$and = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return true;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$and)(x))(Fay$$_(Prelude$and)(xs));}throw ["unhandled case in and",[$p1]];});};var Prelude$or = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return false;}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Fay$$or)(x))(Fay$$_(Prelude$or)(xs));}throw ["unhandled case in or",[$p1]];});};var Prelude$any = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return false;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(Fay$$or)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude$any)(p))(xs));}throw ["unhandled case in any",[$p1,$p2]];});};};var Prelude$all = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return true;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return Fay$$_(Fay$$_(Fay$$and)(Fay$$_(p)(x)))(Fay$$_(Fay$$_(Prelude$all)(p))(xs));}throw ["unhandled case in all",[$p1,$p2]];});};};var Prelude$intersperse = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$prependToAll)(sep))(xs));}throw ["unhandled case in intersperse",[$p1,$p2]];});};};var Prelude$prependToAll = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return Fay$$_(Fay$$_(Fay$$cons)(sep))(Fay$$_(Fay$$_(Fay$$cons)(x))(Fay$$_(Fay$$_(Prelude$prependToAll)(sep))(xs)));}throw ["unhandled case in prependToAll",[$p1,$p2]];});};};var Prelude$intercalate = function($p1){return function($p2){return new Fay$$$(function(){var xss = $p2;var xs = $p1;return Fay$$_(Prelude$concat)(Fay$$_(Fay$$_(Prelude$intersperse)(xs))(xss));});};};var Prelude$maximum = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("maximum: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Prelude$foldl1)(Prelude$max))(xs);});};var Prelude$minimum = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("minimum: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Prelude$foldl1)(Prelude$min))(xs);});};var Prelude$product = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("product: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude$foldl)(Fay$$mult))(1))(xs);});};var Prelude$sum = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Prelude$error)(Fay$$list("sum: empty list"));}var xs = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude$foldl)(Fay$$add))(0))(xs);});};var Prelude$scanl = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var l = $p3;var z = $p2;var f = $p1;return Fay$$_(Fay$$_(Fay$$cons)(z))((function($tmp1){if (Fay$$_($tmp1) === null) {return null;}var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var x = $tmp2.car;var xs = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$_(Prelude$scanl)(f))(Fay$$_(Fay$$_(f)(z))(x)))(xs);}return (function(){ throw (["unhandled case",$tmp1]); })();})(l));});};};};var Prelude$scanl1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$_(Prelude$scanl)(f))(x))(xs);}throw ["unhandled case in scanl1",[$p1,$p2]];});};};var Prelude$scanr = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$_($p3) === null) {var z = $p2;return Fay$$list([z]);}var $tmp1 = Fay$$_($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return (function($tmp1){var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));}return Prelude$$_undefined;})(Fay$$_(Fay$$_(Fay$$_(Prelude$scanr)(f))(z))(xs));}throw ["unhandled case in scanr",[$p1,$p2,$p3]];});};};};var Prelude$scanr1 = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return null;}if (Fay$$listLen(Fay$$_($p2),1)) {var x = Fay$$index(0,Fay$$_($p2));return Fay$$list([x]);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return (function($tmp1){var $tmp2 = Fay$$_($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return Fay$$_(Fay$$_(Fay$$cons)(Fay$$_(Fay$$_(f)(x))(h)))(Fay$$_(Fay$$_(Fay$$cons)(h))(t));}return Prelude$$_undefined;})(Fay$$_(Fay$$_(Prelude$scanr1)(f))(xs));}throw ["unhandled case in scanr1",[$p1,$p2]];});};};var Prelude$lookup = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {var _key = $p1;return Prelude$Nothing;}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(Fay$$_($tmp1.car),2)) {var x = Fay$$index(0,Fay$$_($tmp1.car));var y = Fay$$index(1,Fay$$_($tmp1.car));var xys = $tmp1.cdr;var key = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$eq)(key))(x)) ? Fay$$_(Prelude$Just)(y) : Fay$$_(Fay$$_(Prelude$lookup)(key))(xys);}}throw ["unhandled case in lookup",[$p1,$p2]];});};};var Prelude$length = function($p1){return new Fay$$$(function(){var xs = $p1;return Fay$$_(Fay$$_(Prelude$length$39$)(0))(xs);});};var Prelude$length$39$ = function($p1){return function($p2){return new Fay$$$(function(){var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;var acc = $p1;return Fay$$_(Fay$$_(Prelude$length$39$)(Fay$$_(Fay$$_(Fay$$add)(acc))(1)))(xs);}var acc = $p1;return acc;});};};var Prelude$reverse = function($p1){return new Fay$$$(function(){var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(Prelude$reverse)(xs)))(Fay$$list([x]));}if (Fay$$_($p1) === null) {return null;}throw ["unhandled case in reverse",[$p1]];});};var Prelude$print = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["automatic"],$p1))));});};var Prelude$putStrLn = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs_string($p1))));});};var Language$Fay$FFI$Nullable = function(slot1){return new Fay$$$(function(){return new $_Language$Fay$FFI$Nullable(slot1);});};var Language$Fay$FFI$Null = new Fay$$$(function(){return new $_Language$Fay$FFI$Null();});var Language$Fay$FFI$Defined = function(slot1){return new Fay$$$(function(){return new $_Language$Fay$FFI$Defined(slot1);});};var Language$Fay$FFI$Undefined = new Fay$$$(function(){return new $_Language$Fay$FFI$Undefined();});var JSAPI$addEventListener = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['addEventListener'](Fay$$fayToJs_string($p2),Fay$$fayToJs(["function",[["user","Event",[]],["action",[["bool"]]]]],$p3),false)));});};};};var JSAPI$addWindowEventListener = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['addEventListener'](Fay$$fayToJs_string($p1),Fay$$fayToJs(["function",[["user","Event",[]],["action",[["bool"]]]]],$p2),false)));});};};var JSAPI$getElementById = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Element",[]],document['getElementById'](Fay$$fayToJs_string($p1))));});};var JSAPI$getElementsById = new Fay$$$(function(){return Fay$$_(JSAPI$mapM)(JSAPI$getElementById);});var JSAPI$setClassName = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p2)['className']=Fay$$fayToJs_string($p1)));});};};var JSAPI$setInterval = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['setInterval'](Fay$$fayToJs(["action",[["unknown"]]],$p1),Fay$$fayToJs_double($p2))));});};};var JSAPI$alert = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],window['alert'](Fay$$fayToJs_string($p1))));});};var JSAPI$mapM = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) === null) {return Fay$$_(Fay$$$_return)(null);}var $tmp1 = Fay$$_($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(f)(x)))(function($p1){var vx = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(JSAPI$mapM)(f))(xs)))(function($p1){var vxs = $p1;return Fay$$_(Fay$$$_return)(Fay$$_(Fay$$_(Fay$$cons)(vx))(vxs));});});}throw ["unhandled case in mapM",[$p1,$p2]];});};};var CanvasAPI$getContext = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Context",[]],Fay$$fayToJs(["user","Element",[]],$p1)['getContext'](Fay$$fayToJs_string($p2))));});};};var CanvasAPI$setHeight = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['height']=Fay$$fayToJs_int($p2)));});};};var CanvasAPI$setWidth = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Element",[]],$p1)['width']=Fay$$fayToJs_int($p2)));});};};var CanvasAPI$setFillStyle = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['fillStyle']=Fay$$fayToJs_string($p2)));});};};var CanvasAPI$setStrokeStyle = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['strokeStyle']=Fay$$fayToJs_string($p2)));});};};var CanvasAPI$setLineWidth = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['lineWidth']=Fay$$fayToJs_double($p2)));});};};var CanvasAPI$beginPath = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['beginPath']()));});};var CanvasAPI$endPath = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['endPath']()));});};var CanvasAPI$moveTo = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p2),2)) {var x = Fay$$index(0,Fay$$_($p2));var y = Fay$$index(1,Fay$$_($p2));var context = $p1;return Fay$$_(Fay$$_(Fay$$_(CanvasAPI$moveTo$39$)(context))(x))(y);}throw ["unhandled case in moveTo",[$p1,$p2]];});};};var CanvasAPI$moveTo$39$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['moveTo'](Fay$$fayToJs_double($p2),Fay$$fayToJs_double($p3))));});};};};var CanvasAPI$lineTo = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p2),2)) {var x = Fay$$index(0,Fay$$_($p2));var y = Fay$$index(1,Fay$$_($p2));var context = $p1;return Fay$$_(Fay$$_(Fay$$_(CanvasAPI$lineTo$39$)(context))(x))(y);}throw ["unhandled case in lineTo",[$p1,$p2]];});};};var CanvasAPI$lineTo$39$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['lineTo'](Fay$$fayToJs_double($p2),Fay$$fayToJs_double($p3))));});};};};var CanvasAPI$stroke = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['stroke']()));});};var CanvasAPI$fillRect = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p3),2)) {var w = Fay$$index(0,Fay$$_($p3));var h = Fay$$index(1,Fay$$_($p3));if (Fay$$listLen(Fay$$_($p2),2)) {var x = Fay$$index(0,Fay$$_($p2));var y = Fay$$index(1,Fay$$_($p2));var context = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(Fay$$_(CanvasAPI$fillRect$39$)(context))(x))(y))(w))(h);}}throw ["unhandled case in fillRect",[$p1,$p2,$p3]];});};};};var CanvasAPI$fillRect$39$ = function($p1){return function($p2){return function($p3){return function($p4){return function($p5){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['fillRect'](Fay$$fayToJs_double($p2),Fay$$fayToJs_double($p3),Fay$$fayToJs_double($p4),Fay$$fayToJs_double($p5))));});};};};};};var CanvasAPI$strokeText = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p3),2)) {var x = Fay$$index(0,Fay$$_($p3));var y = Fay$$index(1,Fay$$_($p3));var str = $p2;var context = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(CanvasAPI$strokeText$39$)(context))(str))(x))(y);}throw ["unhandled case in strokeText",[$p1,$p2,$p3]];});};};};var CanvasAPI$strokeText$39$ = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['strokeText'](Fay$$fayToJs_string($p2),Fay$$fayToJs_double($p3),Fay$$fayToJs_double($p4))));});};};};};var CanvasAPI$setFont = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['font']=Fay$$fayToJs_string($p2)));});};};var CanvasAPI$setTextAlignment = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['textAlignment']=Fay$$fayToJs_string($p2)));});};};var CanvasAPI$setTextBaseline = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['textBaseline']=Fay$$fayToJs_string($p2)));});};};var CanvasAPI$save = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['save']()));});};var CanvasAPI$restore = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['restore']()));});};var CanvasAPI$rotate = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['rotate'](Fay$$fayToJs_double($p2))));});};};var CanvasAPI$translate = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$listLen(Fay$$_($p2),2)) {var x = Fay$$index(0,Fay$$_($p2));var y = Fay$$index(1,Fay$$_($p2));var context = $p1;return Fay$$_(Fay$$_(Fay$$_(CanvasAPI$translate$39$)(context))(x))(y);}throw ["unhandled case in translate",[$p1,$p2]];});};};var CanvasAPI$translate$39$ = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$fayToJs(["user","Context",[]],$p1)['translate'](Fay$$fayToJs_double($p2),Fay$$fayToJs_double($p3))));});};};};var CanvasAPI$dottedLine = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var seg = $p4;var end = $p3;var start = $p2;var context = $p1;return Fay$$_(Fay$$_(Prelude$forM_)(Fay$$_(Fay$$_(Fay$$_(CanvasAPI$genDottedLine)(start))(end))(seg)))(function($p1){if (Fay$$listLen(Fay$$_($p1),2)) {var p1 = Fay$$index(0,Fay$$_($p1));var p2 = Fay$$index(1,Fay$$_($p1));return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(CanvasAPI$moveTo)(context))(p1)))(Fay$$_(Fay$$_(CanvasAPI$lineTo)(context))(p2));}throw ["unhandled case",$p1];});});};};};};var CanvasAPI$genDottedLine = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var seg = $p3;if (Fay$$listLen(Fay$$_($p2),2)) {var ex = Fay$$index(0,Fay$$_($p2));var ey = Fay$$index(1,Fay$$_($p2));if (Fay$$listLen(Fay$$_($p1),2)) {var sx = Fay$$index(0,Fay$$_($p1));var sy = Fay$$index(1,Fay$$_($p1));return (function(){var len = function($p1){return function($p2){return new Fay$$$(function(){var dy = $p2;var dx = $p1;return Fay$$_(Fay$$_(Prelude$$36$)(Prelude$sqrt))(Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Prelude$$94$)(dx))(2)))(Fay$$_(Fay$$_(Prelude$$94$)(dy))(2)));});};};return (function(){var dx = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$sub)(ex))(sx);});var dy = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$sub)(ey))(sy);});var l = new Fay$$$(function(){return Fay$$_(Fay$$_(len)(dx))(dy);});var xStep = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$divi)(dx))(l);});var yStep = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$divi)(dy))(l);});var half = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$divi)(seg))(2);});return Fay$$_(Fay$$_(Prelude$map)(function($p1){var n = $p1;return Fay$$list([Fay$$list([Fay$$_(Fay$$_(Fay$$add)(sx))(Fay$$_(Fay$$_(Fay$$mult)(n))(xStep)),Fay$$_(Fay$$_(Fay$$add)(sy))(Fay$$_(Fay$$_(Fay$$mult)(n))(yStep))]),Fay$$list([Fay$$_(Fay$$_(Fay$$add)(sx))(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$add)(n))(half)))(xStep)),Fay$$_(Fay$$_(Fay$$add)(sy))(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$add)(n))(half)))(yStep))])]);}))(Prelude$enumFromThenTo(0)(seg)(l));})();})();}}throw ["unhandled case in genDottedLine",[$p1,$p2,$p3]];});};};};var CanvasAPI$preservingMatrix = function($p1){return function($p2){return new Fay$$$(function(){var actions = $p2;var context = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(CanvasAPI$save)(context)))(Fay$$_(Fay$$_(Fay$$bind)(actions))(function($p1){return Fay$$_(CanvasAPI$restore)(context);}));});};};var Ref$newRef = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["user","Ref",[["unknown"]]],new Fay$$Ref(Fay$$fayToJs(["unknown"],$p1))));});};var Ref$readRef = function($p1){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$readRef(Fay$$fayToJs(["user","Ref",[["unknown"]]],$p1))));});};var Ref$writeRef = function($p1){return function($p2){return new Fay$$$(function(){return new Fay$$Monad(Fay$$jsToFay(["unknown"],Fay$$writeRef(Fay$$fayToJs(["user","Ref",[["unknown"]]],$p1),Fay$$fayToJs(["unknown"],$p2))));});};};var TickerTimeSerie$TimeSerie = function(slot1){return function(slot2){return new Fay$$$(function(){return new $_TickerTimeSerie$TimeSerie(slot1,slot2);});};};var TickerTimeSerie$DataItem = function(slot1){return function(slot2){return new Fay$$$(function(){return new $_TickerTimeSerie$DataItem(slot1,slot2);});};};var TickerTimeSerie$TimeItem = function(slot1){return function(slot2){return new Fay$$$(function(){return new $_TickerTimeSerie$TimeItem(slot1,slot2);});};};var TickerTimeSerie$mkSineTimeSerie = function($p1){return function($p2){return function($p3){return new Fay$$$(function(){var mx = $p3;var start = $p2;var num = $p1;return (function(){var mkItem = function($p1){return new Fay$$$(function(){var n = $p1;return Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(TickerTimeSerie$DataItem)(n)))(Fay$$_(mkValue)(Fay$$_(Fay$$_(Fay$$add)(n))(start)));});};var mkValue = function($p1){return new Fay$$$(function(){var n = $p1;return Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$add)(1))(Fay$$_(Fay$$_(Prelude$$36$)(Prelude$sin))(Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$mult)(freq))(rad)))(Fay$$_(Prelude$fromIntegral)(n))))))(Fay$$_(Fay$$_(Fay$$divi)(mx))(2));});};var mkTime = function($p1){return new Fay$$$(function(){var n = $p1;return Fay$$_(Fay$$_(TickerTimeSerie$TimeItem)(n))(Fay$$_(Fay$$_(Fay$$add)(n))(start));});};var isMod5 = function($p1){return new Fay$$$(function(){var n = $p1;return Fay$$_(Fay$$_(Fay$$eq)(Fay$$_(Fay$$_(Prelude$mod)(n))(5)))(0);});};var rad = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Fay$$_(Fay$$mult)(2))(Prelude$pi)))(Fay$$_(Prelude$fromIntegral)(num));});var freq = 2;return (function(){var ns = new Fay$$$(function(){return Prelude$enumFromTo(0)(Fay$$_(Fay$$_(Fay$$sub)(num))(1));});var d = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$map)(mkItem))(ns);});var t = new Fay$$$(function(){var $gen_1 = function($p1){return new Fay$$$(function(){var n = $p1;return Fay$$_(Fay$$_(isMod5)(Fay$$_(Fay$$_(Fay$$add)(n))(start))) ? Fay$$list([Fay$$_(mkTime)(n)]) : null;return null;});};return Fay$$_(Fay$$_(Prelude$concatMap)($gen_1))(ns);});return Fay$$_(Fay$$_(TickerTimeSerie$TimeSerie)(mx))(Fay$$list([d,t]));})();})();});};};};var Ticker$State = function(drawingContext){return function(graphButtons){return function(colorButtons){return function(dataRenderer){return function(graphColors){return function(timeSerie){return function(startTime){return new Fay$$$(function(){return new $_Ticker$State(drawingContext,graphButtons,colorButtons,dataRenderer,graphColors,timeSerie,startTime);});};};};};};};};var Ticker$drawingContext = function(x){return new Fay$$$(function(){return Fay$$_(x).drawingContext;});};var Ticker$graphButtons = function(x){return new Fay$$$(function(){return Fay$$_(x).graphButtons;});};var Ticker$colorButtons = function(x){return new Fay$$$(function(){return Fay$$_(x).colorButtons;});};var Ticker$dataRenderer = function(x){return new Fay$$$(function(){return Fay$$_(x).dataRenderer;});};var Ticker$graphColors = function(x){return new Fay$$$(function(){return Fay$$_(x).graphColors;});};var Ticker$timeSerie = function(x){return new Fay$$$(function(){return Fay$$_(x).timeSerie;});};var Ticker$startTime = function(x){return new Fay$$$(function(){return Fay$$_(x).startTime;});};var Ticker$Colors = function(background){return function(grid){return function(plot){return new Fay$$$(function(){return new $_Ticker$Colors(background,grid,plot);});};};};var Ticker$background = function(x){return new Fay$$$(function(){return Fay$$_(x).background;});};var Ticker$grid = function(x){return new Fay$$$(function(){return Fay$$_(x).grid;});};var Ticker$plot = function(x){return new Fay$$$(function(){return Fay$$_(x).plot;});};var Ticker$tickerInit = function($p1){return new Fay$$$(function(){var name = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(JSAPI$getElementById)(name)))(function($p1){var canvas = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(CanvasAPI$getContext)(canvas))(Fay$$list("2d"))))(function($p1){var context = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(CanvasAPI$setWidth)(canvas)))(Fay$$_(Prelude$floor)(Ticker$cwidth))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(CanvasAPI$setHeight)(canvas)))(Fay$$_(Prelude$floor)(Ticker$cheight))))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(JSAPI$getElementsById)(Fay$$list([Fay$$list("CurveButton"),Fay$$list("BarButton")]))))(function($p1){var graphButtons$39$ = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Ticker$selectFirst)(graphButtons$39$)))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(JSAPI$getElementsById)(Fay$$list([Fay$$list("BlackButton"),Fay$$list("BlueButton")]))))(function($p1){var colorButtons$39$ = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Ticker$selectFirst)(colorButtons$39$)))(Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Fay$$_(Prelude$$36$)(Ref$newRef))((function(){var State = new $_Ticker$State();State.drawingContext = context;State.graphButtons = graphButtons$39$;State.colorButtons = colorButtons$39$;State.dataRenderer = Ticker$renderDataPlotAsCurves;State.graphColors = Ticker$blackColorScheme;State.timeSerie = Fay$$_(Fay$$_(Fay$$_(TickerTimeSerie$mkSineTimeSerie)(60))(0))(100);State.startTime = 1;return State;})())))(function($p1){var state = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Ticker$addButtonListeners)(graphButtons$39$))(Ticker$handleGraphButton))(Fay$$list([Ticker$renderDataPlotAsCurves,Ticker$renderDataPlotAsBars])))(state)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(Ticker$addButtonListeners)(colorButtons$39$))(Ticker$handleColorButton))(Fay$$list([Ticker$blackColorScheme,Ticker$blueColorScheme])))(state)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Ticker$render)(state)))(Fay$$_(Fay$$_(JSAPI$setInterval)(Fay$$_(Ticker$animate)(state)))(1000))));}));}));})));});});});};var Ticker$render = function($p1){return new Fay$$$(function(){var state = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Ref$readRef)(state)))(function($p1){var state$39$ = $p1;return (function(){var context = new Fay$$$(function(){return Fay$$_(Ticker$drawingContext)(state$39$);});return (function(){var colors = new Fay$$$(function(){return Fay$$_(Ticker$graphColors)(state$39$);});return (function(){var renderer = new Fay$$$(function(){return Fay$$_(Ticker$dataRenderer)(state$39$);});return (function(){var tserie = new Fay$$$(function(){return Fay$$_(Ticker$timeSerie)(state$39$);});return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(CanvasAPI$setFillStyle)(context)))(Fay$$_(Ticker$background)(colors))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(CanvasAPI$fillRect)(context))(Fay$$list([0,0])))(Fay$$list([Ticker$cwidth,Ticker$cheight]))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(CanvasAPI$setStrokeStyle)(context)))(Fay$$_(Ticker$grid)(colors))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Ticker$renderHorizonalLines)(context)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(CanvasAPI$setFont)(context))(Fay$$list("9px sans-serif"))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Ticker$renderTimeMarks)(context))(tserie)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(CanvasAPI$setStrokeStyle)(context)))(Fay$$_(Ticker$plot)(colors))))(Fay$$_(Fay$$_(renderer)(context))(tserie))))))));})();})();})();})();});});};var Ticker$renderHorizonalLines = function($p1){return new Fay$$$(function(){var context = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(CanvasAPI$setLineWidth)(context))(1)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(CanvasAPI$beginPath)(context)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$forM_)(Prelude$enumFromThenTo(0)(10)(100)))(function($p1){var p = $p1;return Fay$$_(Fay$$_(Fay$$_(Fay$$_(CanvasAPI$dottedLine)(context))(Fay$$list([Ticker$gleft,Fay$$_(Ticker$yOnGraph)(p)])))(Fay$$list([Ticker$gright,Fay$$_(Ticker$yOnGraph)(p)])))(10);})))(Fay$$_(CanvasAPI$stroke)(context))));});};var Ticker$renderTimeMarks = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) instanceof $_TickerTimeSerie$TimeSerie) {if (Fay$$listLen(Fay$$_(Fay$$_($p2).slot2),2)) {var timeItems = Fay$$index(1,Fay$$_(Fay$$_($p2).slot2));var context = $p1;return (function(){var degToRad = function($p1){return new Fay$$$(function(){var deg = $p1;return Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$divi)(Prelude$pi))(180)))(deg);});};return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(CanvasAPI$setLineWidth)(context))(1)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(CanvasAPI$beginPath)(context)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(CanvasAPI$setTextBaseline)(context))(Fay$$list("middle"))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$forM_)(timeItems))(function($p1){if (Fay$$_($p1) instanceof $_TickerTimeSerie$TimeItem) {var x = Fay$$_($p1).slot1;var s = Fay$$_($p1).slot2;return (function(){var xpos = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$$36$)(Ticker$xOnGraph))(Fay$$_(Prelude$fromIntegral)(x));});return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Fay$$_(Fay$$_(CanvasAPI$dottedLine)(context))(Fay$$list([xpos,Ticker$gtop])))(Fay$$list([xpos,Ticker$gbottom])))(10)))(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(CanvasAPI$preservingMatrix)(context)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(CanvasAPI$translate)(context))(Fay$$list([xpos,Ticker$tbottom]))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(CanvasAPI$rotate)(context)))(Fay$$_(degToRad)((-(90))))))(Fay$$_(Fay$$_(Fay$$_(CanvasAPI$strokeText)(context))(Fay$$_(Ticker$secsToString)(s)))(Fay$$list([0,0]))))));})();}throw ["unhandled case",$p1];})))(Fay$$_(CanvasAPI$stroke)(context)))));})();}}throw ["unhandled case in renderTimeMarks",[$p1,$p2]];});};};var Ticker$renderDataPlotAsCurves = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) instanceof $_TickerTimeSerie$TimeSerie) {var mx = Fay$$_($p2).slot1;if (Fay$$listLen(Fay$$_(Fay$$_($p2).slot2),2)) {var $tmp1 = Fay$$_(Fay$$index(0,Fay$$_(Fay$$_($p2).slot2)));if ($tmp1 instanceof Fay$$Cons) {var p = $tmp1.car;var ps = $tmp1.cdr;var context = $p1;return (function(){var norm = function($p1){return new Fay$$$(function(){var n = $p1;return Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$divi)(n))(mx)))(100);});};var initialMove = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) instanceof $_TickerTimeSerie$DataItem) {var xVal = Fay$$_($p1).slot1;var yVal = Fay$$_($p1).slot2;return Fay$$_(Fay$$_(CanvasAPI$moveTo)(context))(Fay$$list([Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Fay$$_(Prelude$$46$)(Ticker$xOnGraph))(Prelude$fromIntegral)))(xVal),Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Fay$$_(Prelude$$46$)(Ticker$yOnGraph))(norm)))(yVal)]));}throw ["unhandled case in initialMove",[$p1]];});};return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(CanvasAPI$setLineWidth)(context))(1.5)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(CanvasAPI$beginPath)(context)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(initialMove)(p)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$forM_)(ps))(function($p1){if (Fay$$_($p1) instanceof $_TickerTimeSerie$DataItem) {var xVal = Fay$$_($p1).slot1;var yVal = Fay$$_($p1).slot2;return Fay$$_(Fay$$_(CanvasAPI$lineTo)(context))(Fay$$list([Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Fay$$_(Prelude$$46$)(Ticker$xOnGraph))(Prelude$fromIntegral)))(xVal),Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Fay$$_(Prelude$$46$)(Ticker$yOnGraph))(norm)))(yVal)]));}throw ["unhandled case",$p1];})))(Fay$$_(CanvasAPI$stroke)(context)))));})();}}}throw ["unhandled case in renderDataPlotAsCurves",[$p1,$p2]];});};};var Ticker$renderDataPlotAsBars = function($p1){return function($p2){return new Fay$$$(function(){if (Fay$$_($p2) instanceof $_TickerTimeSerie$TimeSerie) {var mx = Fay$$_($p2).slot1;if (Fay$$listLen(Fay$$_(Fay$$_($p2).slot2),2)) {var ps = Fay$$index(0,Fay$$_(Fay$$_($p2).slot2));var context = $p1;return (function(){var norm = function($p1){return new Fay$$$(function(){var n = $p1;return Fay$$_(Fay$$_(Fay$$mult)(Fay$$_(Fay$$_(Fay$$divi)(n))(mx)))(100);});};return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(CanvasAPI$setLineWidth)(context))(5)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(CanvasAPI$beginPath)(context)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$forM_)(ps))(function($p1){if (Fay$$_($p1) instanceof $_TickerTimeSerie$DataItem) {var xVal = Fay$$_($p1).slot1;var yVal = Fay$$_($p1).slot2;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(CanvasAPI$moveTo)(context))(Fay$$list([Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Fay$$_(Prelude$$46$)(Ticker$xOnGraph))(Prelude$fromIntegral)))(xVal),Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Fay$$_(Prelude$$46$)(Ticker$yOnGraph))(norm)))(yVal)]))))(Fay$$_(Fay$$_(CanvasAPI$lineTo)(context))(Fay$$list([Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Fay$$_(Prelude$$46$)(Ticker$xOnGraph))(Prelude$fromIntegral)))(xVal),Fay$$_(Ticker$yOnGraph)(0)])));}throw ["unhandled case",$p1];})))(Fay$$_(CanvasAPI$stroke)(context))));})();}}throw ["unhandled case in renderDataPlotAsBars",[$p1,$p2]];});};};var Ticker$yOnGraph = function($p1){return new Fay$$$(function(){var y = $p1;return Fay$$_(Fay$$_(Fay$$sub)(Ticker$gbottom))(Fay$$_(Fay$$_(Fay$$mult)(Ticker$gheight))(Fay$$_(Fay$$_(Fay$$divi)(y))(100)));});};var Ticker$xOnGraph = function($p1){return new Fay$$$(function(){var x = $p1;return Fay$$_(Fay$$_(Fay$$mult)(Ticker$gwidth))(Fay$$_(Fay$$_(Fay$$divi)(x))(60));});};var Ticker$ctop = 0;var Ticker$cbottom = 209;var Ticker$cleft = 0;var Ticker$cright = 499;var Ticker$cwidth = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$add)(Ticker$cright))(1);});var Ticker$cheight = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$add)(Ticker$cbottom))(1);});var Ticker$gtop = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$add)(Ticker$ctop))(10);});var Ticker$gbottom = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$sub)(Ticker$cbottom))(50);});var Ticker$gleft = new Fay$$$(function(){return Ticker$cleft;});var Ticker$gright = new Fay$$$(function(){return Ticker$cright;});var Ticker$gwidth = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$add)(Ticker$gright))(1);});var Ticker$gheight = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Fay$$_(Fay$$sub)(Ticker$gbottom))(Ticker$gtop)))(1);});var Ticker$tbottom = new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$sub)(Ticker$cbottom))(2);});var Ticker$blackColorScheme = new Fay$$$(function(){var Colors = new $_Ticker$Colors();Colors.background = Fay$$list("#040404");Colors.grid = Fay$$list("#358800");Colors.plot = Fay$$list("red");return Colors;});var Ticker$blueColorScheme = new Fay$$$(function(){var Colors = new $_Ticker$Colors();Colors.background = Fay$$list("#F2F7FE");Colors.grid = Fay$$list("#7B899B");Colors.plot = Fay$$list("#7B899B");return Colors;});var Ticker$addButtonListeners = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var state = $p4;var items = $p3;var cb = $p2;var buttons = $p1;return Fay$$_(Fay$$_(Prelude$forM_)(Fay$$_(Fay$$_(Prelude$zip)(buttons))(items)))(function($p1){if (Fay$$listLen(Fay$$_($p1),2)) {var b = Fay$$index(0,Fay$$_($p1));var i = Fay$$index(1,Fay$$_($p1));return Fay$$_(Fay$$_(Fay$$_(JSAPI$addEventListener)(b))(Fay$$list("click")))(Fay$$_(Fay$$_(Fay$$_(cb)(state))(i))(b));}throw ["unhandled case",$p1];});});};};};};var Ticker$selectFirst = function($p1){return new Fay$$$(function(){if (Fay$$_($p1) === null) {return Fay$$_(Fay$$$_return)(Fay$$unit);}var $tmp1 = Fay$$_($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;return Fay$$_(Fay$$_(JSAPI$setClassName)(Fay$$list("Selected")))(x);}throw ["unhandled case in selectFirst",[$p1]];});};var Ticker$handleGraphButton = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var me = $p3;var renderer = $p2;var state = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Ref$readRef)(state)))(function($p1){var state$39$ = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Prelude$mapM_)(Fay$$_(JSAPI$setClassName)(Fay$$list("")))))(Fay$$_(Ticker$graphButtons)(state$39$))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Ref$writeRef)(state)))((function(){var $36$_record_to_update = Object.create(Fay$$_(state$39$));$36$_record_to_update.dataRenderer = renderer;return $36$_record_to_update;})())))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(JSAPI$setClassName)(Fay$$list("Selected")))(me)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Ticker$render)(state)))(Fay$$_(Fay$$$_return)(false)))));});});};};};};var Ticker$handleColorButton = function($p1){return function($p2){return function($p3){return function($p4){return new Fay$$$(function(){var me = $p3;var colors = $p2;var state = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Ref$readRef)(state)))(function($p1){var state$39$ = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Prelude$mapM_)(Fay$$_(JSAPI$setClassName)(Fay$$list("")))))(Fay$$_(Ticker$colorButtons)(state$39$))))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Ref$writeRef)(state)))((function(){var $36$_record_to_update = Object.create(Fay$$_(state$39$));$36$_record_to_update.graphColors = colors;return $36$_record_to_update;})())))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(JSAPI$setClassName)(Fay$$list("Selected")))(me)))(Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Ticker$render)(state)))(Fay$$_(Fay$$$_return)(false)))));});});};};};};var Ticker$animate = function($p1){return new Fay$$$(function(){var state = $p1;return Fay$$_(Fay$$_(Fay$$bind)(Fay$$_(Ref$readRef)(state)))(function($p1){var state$39$ = $p1;return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Fay$$_(Prelude$$36$)(Fay$$_(Ref$writeRef)(state)))((function(){var $36$_record_to_update = Object.create(Fay$$_(state$39$));$36$_record_to_update.timeSerie = Fay$$_(Fay$$_(Fay$$_(TickerTimeSerie$mkSineTimeSerie)(60))(Fay$$_(Ticker$startTime)(state$39$)))(100);$36$_record_to_update.startTime = Fay$$_(Fay$$_(Fay$$add)(Fay$$_(Ticker$startTime)(state$39$)))(1);return $36$_record_to_update;})())))(Fay$$_(Ticker$render)(state));});});};var Ticker$secsToString = function($p1){return new Fay$$$(function(){var s = $p1;return (function(){var toStr = function($p1){return new Fay$$$(function(){var n = $p1;if (Fay$$_(Fay$$_(Fay$$_(Fay$$lt)(n))(10))) {return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list("0")))(Fay$$_(Prelude$show)(n));} else {if (true) {return Fay$$_(Prelude$show)(n);}}});};return (function(){var sec = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$mod)(s))(60);});var mns = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$mod)(Fay$$_(Fay$$_(Prelude$$36$)(Prelude$floor))(Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude$fromIntegral)(s)))(60))))(60);});var hrs = new Fay$$$(function(){return Fay$$_(Fay$$_(Prelude$$36$)(Prelude$floor))(Fay$$_(Fay$$_(Fay$$divi)(Fay$$_(Prelude$fromIntegral)(s)))(3600));});return Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(toStr)(hrs)))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list(":")))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$_(toStr)(mns)))(Fay$$_(Fay$$_(Prelude$$43$$43$)(Fay$$list(":")))(Fay$$_(toStr)(sec)))));})();})();});};var TickerApp$main = new Fay$$$(function(){return Fay$$_(Fay$$_(JSAPI$addWindowEventListener)(Fay$$list("load")))(TickerApp$handleLoad);});var TickerApp$handleLoad = function($p1){return new Fay$$$(function(){return Fay$$_(Fay$$_(Fay$$then)(Fay$$_(Ticker$tickerInit)(Fay$$list("canvas"))))(Fay$$_(Fay$$$_return)(false));});};var $_Language$Fay$FFI$Nullable = function(slot1){this.slot1 = slot1;};var $_Language$Fay$FFI$Null = function(){};var $_Language$Fay$FFI$Defined = function(slot1){this.slot1 = slot1;};var $_Language$Fay$FFI$Undefined = function(){};var $_Prelude$Just = function(slot1){this.slot1 = slot1;};var $_Prelude$Nothing = function(){};var $_Prelude$Left = function(slot1){this.slot1 = slot1;};var $_Prelude$Right = function(slot1){this.slot1 = slot1;};var $_Prelude$Ratio = function(slot1,slot2){this.slot1 = slot1;this.slot2 = slot2;};var $_Prelude$GT = function(){};var $_Prelude$LT = function(){};var $_Prelude$EQ = function(){};var $_Language$Fay$FFI$Nullable = function(slot1){this.slot1 = slot1;};var $_Language$Fay$FFI$Null = function(){};var $_Language$Fay$FFI$Defined = function(slot1){this.slot1 = slot1;};var $_Language$Fay$FFI$Undefined = function(){};var $_TickerTimeSerie$TimeSerie = function(slot1,slot2){this.slot1 = slot1;this.slot2 = slot2;};var $_TickerTimeSerie$DataItem = function(slot1,slot2){this.slot1 = slot1;this.slot2 = slot2;};var $_TickerTimeSerie$TimeItem = function(slot1,slot2){this.slot1 = slot1;this.slot2 = slot2;};var $_Ticker$State = function(drawingContext,graphButtons,colorButtons,dataRenderer,graphColors,timeSerie,startTime){this.drawingContext = drawingContext;this.graphButtons = graphButtons;this.colorButtons = colorButtons;this.dataRenderer = dataRenderer;this.graphColors = graphColors;this.timeSerie = timeSerie;this.startTime = startTime;};var $_Ticker$Colors = function(background,grid,plot){this.background = background;this.grid = grid;this.plot = plot;};var Fay$$fayToJsUserDefined = function(type,obj){var _obj = Fay$$_(obj);var argTypes = type[2];if (_obj instanceof $_Language$Fay$FFI$Nullable) {var obj_ = {"instance": "Nullable"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$FFI$Null) {var obj_ = {"instance": "Null"};return obj_;}if (_obj instanceof $_Language$Fay$FFI$Defined) {var obj_ = {"instance": "Defined"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$FFI$Undefined) {var obj_ = {"instance": "Undefined"};return obj_;}if (_obj instanceof $_Prelude$Just) {var obj_ = {"instance": "Just"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Prelude$Nothing) {var obj_ = {"instance": "Nothing"};return obj_;}if (_obj instanceof $_Prelude$Left) {var obj_ = {"instance": "Left"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Prelude$Right) {var obj_ = {"instance": "Right"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Prelude$Ratio) {var obj_ = {"instance": "Ratio"};var obj_slot1 = Fay$$fayToJs_int(_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}var obj_slot2 = Fay$$fayToJs_int(_obj.slot2);if (undefined !== obj_slot2) {obj_['slot2'] = obj_slot2;}return obj_;}if (_obj instanceof $_Prelude$GT) {var obj_ = {"instance": "GT"};return obj_;}if (_obj instanceof $_Prelude$LT) {var obj_ = {"instance": "LT"};return obj_;}if (_obj instanceof $_Prelude$EQ) {var obj_ = {"instance": "EQ"};return obj_;}if (_obj instanceof $_Language$Fay$FFI$Nullable) {var obj_ = {"instance": "Nullable"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$FFI$Null) {var obj_ = {"instance": "Null"};return obj_;}if (_obj instanceof $_Language$Fay$FFI$Defined) {var obj_ = {"instance": "Defined"};var obj_slot1 = Fay$$fayToJs(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$FFI$Undefined) {var obj_ = {"instance": "Undefined"};return obj_;}if (_obj instanceof $_TickerTimeSerie$TimeSerie) {var obj_ = {"instance": "TimeSerie"};var obj_slot1 = Fay$$fayToJs_double(_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}var obj_slot2 = Fay$$fayToJs(["tuple",[["list",[["user","DataItem",[]]]],["list",[["user","TimeItem",[]]]]]],_obj.slot2);if (undefined !== obj_slot2) {obj_['slot2'] = obj_slot2;}return obj_;}if (_obj instanceof $_TickerTimeSerie$DataItem) {var obj_ = {"instance": "DataItem"};var obj_slot1 = Fay$$fayToJs_int(_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}var obj_slot2 = Fay$$fayToJs_double(_obj.slot2);if (undefined !== obj_slot2) {obj_['slot2'] = obj_slot2;}return obj_;}if (_obj instanceof $_TickerTimeSerie$TimeItem) {var obj_ = {"instance": "TimeItem"};var obj_slot1 = Fay$$fayToJs_int(_obj.slot1);if (undefined !== obj_slot1) {obj_['slot1'] = obj_slot1;}var obj_slot2 = Fay$$fayToJs_int(_obj.slot2);if (undefined !== obj_slot2) {obj_['slot2'] = obj_slot2;}return obj_;}if (_obj instanceof $_Ticker$State) {var obj_ = {"instance": "State"};var obj_drawingContext = Fay$$fayToJs(["user","Context",[]],_obj.drawingContext);if (undefined !== obj_drawingContext) {obj_['drawingContext'] = obj_drawingContext;}var obj_graphButtons = Fay$$fayToJs(["list",[["user","Element",[]]]],_obj.graphButtons);if (undefined !== obj_graphButtons) {obj_['graphButtons'] = obj_graphButtons;}var obj_colorButtons = Fay$$fayToJs(["list",[["user","Element",[]]]],_obj.colorButtons);if (undefined !== obj_colorButtons) {obj_['colorButtons'] = obj_colorButtons;}var obj_dataRenderer = Fay$$fayToJs(["user","DataRenderer",[]],_obj.dataRenderer);if (undefined !== obj_dataRenderer) {obj_['dataRenderer'] = obj_dataRenderer;}var obj_graphColors = Fay$$fayToJs(["user","Colors",[]],_obj.graphColors);if (undefined !== obj_graphColors) {obj_['graphColors'] = obj_graphColors;}var obj_timeSerie = Fay$$fayToJs(["user","TimeSerie",[]],_obj.timeSerie);if (undefined !== obj_timeSerie) {obj_['timeSerie'] = obj_timeSerie;}var obj_startTime = Fay$$fayToJs_int(_obj.startTime);if (undefined !== obj_startTime) {obj_['startTime'] = obj_startTime;}return obj_;}if (_obj instanceof $_Ticker$Colors) {var obj_ = {"instance": "Colors"};var obj_background = Fay$$fayToJs_string(_obj.background);if (undefined !== obj_background) {obj_['background'] = obj_background;}var obj_grid = Fay$$fayToJs_string(_obj.grid);if (undefined !== obj_grid) {obj_['grid'] = obj_grid;}var obj_plot = Fay$$fayToJs_string(_obj.plot);if (undefined !== obj_plot) {obj_['plot'] = obj_plot;}return obj_;}return obj;};var Fay$$jsToFayUserDefined = function(type,obj){var argTypes = type[2];if (obj["instance"] === "Nullable") {return new $_Language$Fay$FFI$Nullable(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Null") {return new $_Language$Fay$FFI$Null();}if (obj["instance"] === "Defined") {return new $_Language$Fay$FFI$Defined(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Undefined") {return new $_Language$Fay$FFI$Undefined();}if (obj["instance"] === "Just") {return new $_Prelude$Just(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Nothing") {return new $_Prelude$Nothing();}if (obj["instance"] === "Left") {return new $_Prelude$Left(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Right") {return new $_Prelude$Right(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Ratio") {return new $_Prelude$Ratio(Fay$$jsToFay_int(obj["slot1"]),Fay$$jsToFay_int(obj["slot2"]));}if (obj["instance"] === "GT") {return new $_Prelude$GT();}if (obj["instance"] === "LT") {return new $_Prelude$LT();}if (obj["instance"] === "EQ") {return new $_Prelude$EQ();}if (obj["instance"] === "Nullable") {return new $_Language$Fay$FFI$Nullable(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Null") {return new $_Language$Fay$FFI$Null();}if (obj["instance"] === "Defined") {return new $_Language$Fay$FFI$Defined(Fay$$jsToFay(argTypes && (argTypes)[0] ? (argTypes)[0] : (type)[0] === "automatic" ? ["automatic"] : ["unknown"],obj["slot1"]));}if (obj["instance"] === "Undefined") {return new $_Language$Fay$FFI$Undefined();}if (obj["instance"] === "TimeSerie") {return new $_TickerTimeSerie$TimeSerie(Fay$$jsToFay_double(obj["slot1"]),Fay$$jsToFay(["tuple",[["list",[["user","DataItem",[]]]],["list",[["user","TimeItem",[]]]]]],obj["slot2"]));}if (obj["instance"] === "DataItem") {return new $_TickerTimeSerie$DataItem(Fay$$jsToFay_int(obj["slot1"]),Fay$$jsToFay_double(obj["slot2"]));}if (obj["instance"] === "TimeItem") {return new $_TickerTimeSerie$TimeItem(Fay$$jsToFay_int(obj["slot1"]),Fay$$jsToFay_int(obj["slot2"]));}if (obj["instance"] === "State") {return new $_Ticker$State(Fay$$jsToFay(["user","Context",[]],obj["drawingContext"]),Fay$$jsToFay(["list",[["user","Element",[]]]],obj["graphButtons"]),Fay$$jsToFay(["list",[["user","Element",[]]]],obj["colorButtons"]),Fay$$jsToFay(["user","DataRenderer",[]],obj["dataRenderer"]),Fay$$jsToFay(["user","Colors",[]],obj["graphColors"]),Fay$$jsToFay(["user","TimeSerie",[]],obj["timeSerie"]),Fay$$jsToFay_int(obj["startTime"]));}if (obj["instance"] === "Colors") {return new $_Ticker$Colors(Fay$$jsToFay_string(obj["background"]),Fay$$jsToFay_string(obj["grid"]),Fay$$jsToFay_string(obj["plot"]));}return obj;};
// Exports
this.TickerApp$main = TickerApp$main;

// Built-ins
this._ = Fay$$_;
this.$           = Fay$$$;
this.$fayToJs    = Fay$$fayToJs;
this.$jsToFay    = Fay$$jsToFay;

};
;
var main = new TickerApp();
main._(main.TickerApp$main);

