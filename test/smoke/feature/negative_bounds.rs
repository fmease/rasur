// (internal)
//: (2018->) accept (2015) reject

type Ty = dyn !Trait;
type Ty = impl !Trait;

fn func()
where
    !: !Bound + (!Bound);
