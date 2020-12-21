package cs320

import Value._

object Implementation extends Template {

  def lookup(x:String, env: Env): Value ={
    env.getOrElse(x,error("1"))
  }

  def interp(expr: Expr): Value = {
    def interp1(expr: Expr, env: Env): Value = expr match{
      case Id(x) => lookup(x,env)
      case IntE(n) => IntV(n)
      case BooleanE(b) => BooleanV(b)
      case Add(l,r) => interp1(l,env) match{
        case IntV(n1) => interp1(r,env) match{
          case IntV(n2) => IntV(n1+n2)
          case _ => error("2")
        }
        case _=> error("3")
      }
      case Mul(l,r) => interp1(l,env) match{
        case IntV(n1) => interp1(r,env) match{
          case IntV(n2) => IntV(n1*n2)
          case _ => error("4")
        }
        case _=> error("5")
      }
      case Div(l,r) => interp1(l,env) match{
        case IntV(n1) => interp1(r,env) match{
          case IntV(n2) => if (n2==0) error("6") else IntV(n1/n2)
          case _ => error("7")
        }
        case _=> error("8")
      }
      case Mod(l,r) => interp1(l,env) match{
        case IntV(n1) => interp1(r,env) match{
          case IntV(n2) => if (n2==0) error("9") else IntV(n1%n2)
          case _ => error("10")
        }
        case _=> error("11")
      }
      case Eq(l,r) => interp1(l,env) match{
        case IntV(n1) => interp1(r,env) match{
          case IntV(n2) => BooleanV(n1==n2)
          case _ => error("12")
        }
        case _=> error("13")
      }
      case Lt(l,r) => interp1(l,env) match{
        case IntV(n1) => interp1(r,env) match{
          case IntV(n2) => BooleanV(n1<n2)
          case _ => error("14")
        }
        case _=> error("15")
      }
      case If(con, tr, fl) => interp1(con,env) match{
        case BooleanV(b) => b match{
          case true => interp1(tr,env)
          case false => interp1(fl,env)
        }
        case _ => error("16")
      }
      case TupleE(es) => TupleV(es.map(interp1(_,env)))
      case Proj(e, idx) => interp1(e,env) match{
        case TupleV(vs) => if (0<idx && idx<=vs.length) vs(idx-1) else error("17")
        case _ => error("18")
      }
      case NilE => NilV
      case ConsE(h,t) => interp1(h,env) match{
        case a=> interp1(t,env) match{
          case NilV => ConsV(a,NilV)
          case ConsV(h2,t2)=> ConsV(a,ConsV(h2,t2))
          case _ => error("19")
        }
      }
      case Empty(e) => interp1(e,env) match{
        case NilV => BooleanV(true)
        case ConsV(h,t) => BooleanV(false)
        case _ => error("20")
      }
      case Head(e) => interp1(e,env) match{
        case ConsV(h,t) => h
        case _ => error("21")
      }
      case Tail(e) => interp1(e,env) match{
        case ConsV(h,t) => t
        case _=> error("22")
      }
      case Val(x,e,b) => interp1(b,env + (x -> interp1(e,env)))
      case Fun(ps, b) => CloV(ps,b,env)
      case RecFuns(fs,b) =>{
        val vars= fs.map(x=>
          x match{
            case FunDef(n,param,bo)=>n
          case _=> error("23")
          })
        val clos= fs.map(x=>
          x match{
            case FunDef(n,param,bo)=>
            val clov = CloV(param,bo,env)
            clov
            case _=> error("24")
          })
        val nenv=env++(vars zip clos)
        for(i<-0 to fs.length-1){
          clos(i).env=nenv
        }
        interp1(b,nenv)
    }
      case App(f,args) => interp1(f,env) match{
        case clov:CloV => 
          if (args.length != clov.parameters.length) error("25")
          else {
          clov.parameters.foreach(x=>(clov.env = clov.env + (x->interp1(args(clov.parameters.indexOf(x,0)),env))))
          interp1(clov.body,clov.env)
          }
        case _ => error("26")
      }
      case Test(e:Expr, typ:Type) => 
        val a = interp1(e,env)
        typ match{
        case IntT => a match{
          case _:IntV => BooleanV(true)
          case _=> BooleanV(false)
        }
        case BooleanT => a match{
          case _:BooleanV => BooleanV(true)
          case _=> BooleanV(false)
        }
        case TupleT => a match{
          case _:TupleV => BooleanV(true)
          case _=> BooleanV(false)
        }
        case ListT => a match{
          case NilV => BooleanV(true)
          case _:ConsV => BooleanV(true)
          case _=> BooleanV(false)
        }
        case FunctionT => a match{
          case _:CloV => BooleanV(true)
          case _=> BooleanV(false)
        }
        case _=> error("27")
      }
    }
    interp1(expr,Map())
  }
  def addEnvright(fun1 : FunDef, env : Env): Env ={
    val clov = CloV(fun1.parameters, fun1.body, Map())
    val nenv = (fun1.name -> clov)
    clov.env = env + nenv
    env + (fun1.name -> clov)
  }
  def addEnvleft(env : Env, fun1 : FunDef): Env ={
    val clov = CloV(fun1.parameters, fun1.body, Map())
    val nenv = (fun1.name -> clov)
    clov.env = env + nenv
    env + (fun1.name -> clov)
  }

  def env_change(value:Value, env:Env):Value =value match{
    case clov:CloV => CloV(clov.parameters, clov.body, env)
    case a => a
  }
}
