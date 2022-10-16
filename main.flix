use Monad.{>>=};


enum SKI with Eq {
  case S,
  case K,
  case I,
  case X,
  case Y,
  case Z,
  case App(SKI,SKI)
}


instance ToString[SKI] {
    pub def toString(t: SKI): String =
        match t {
            case S => "S"
            case K => "K"
            case I => "I"
            case X => "X"
            case Y => "Y"
            case Z => "Z"
            case App(x,App(y,z)) => "${x} (${y} ${z})"
            case App(x,y) => "${x} ${y}"
        }
}


def step(tm: SKI): Option[SKI] =
    match tm {
        case App(I,x) => Some(x)
        case App(App(K,x),_) => Some(x)
        case App(App(App(S,x),y),z) => Some(App(App(x,z),(App(y,z))))
        case App(x,y) => {
            let ltm = Option.getWithDefault(x,step(x));
            let rtm = Option.getWithDefault(y,step(y));
            Some(App(ltm,rtm))
        }
        case _ => None
    }


def stepThrough(tm: SKI): Option[SKI] =
    match step(tm) {
        case Some(t) => {
            if (tm == t) {
                Some(t)
            } else {
                stepThrough(t)
            }
        }
        case None => None
    }


def parse(prog: String): Option[SKI] =
    def innerParser(p: List[Char], acc: List[SKI]) =
        match p {
            case 'S'::xs => innerParser(xs,S::acc)
            case 'K'::xs => innerParser(xs,K::acc)
            case 'I'::xs => innerParser(xs,I::acc)
            case 'X'::xs => innerParser(xs,X::acc)
            case 'Y'::xs => innerParser(xs,Y::acc)
            case 'Z'::xs => innerParser(xs,Z::acc)
            case '('::xs => {
                let (x,y) = innerParser(xs,Nil);
                match x {
                    case None => (None, Nil)
                    case Some(ski) => innerParser(y, ski::acc)
                }
            }
            case ')'::xs => match List.reverse(acc) {
                case Nil => (None, Nil)
                case y::ys => 
                    (Some(List.foldLeft(z -> q -> App(z,q), y, ys)), xs)
            }
            case Nil => match List.reverse(acc) {
                case Nil => (None, Nil)
                case y::ys =>
                    (Some(List.foldLeft(z -> q -> App(z,q), y, ys)), Nil)
            }
            case _::xs => innerParser(xs,acc)
        };
        
    innerParser(String.toList(prog), Nil) |> fst


def main(): Unit \ IO =
    print(Console.greenBright(">>> "))
    |> Console.readLine
    >>= parse
    >>= stepThrough
    |> Option.map(ToString.toString)
    |> Option.getWithDefault(Console.redBright("Error!"))
    |> println
    |> main