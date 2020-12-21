package cs320

import Value._

object Implementation extends Template {

  sealed trait Handler
  case class HandlerV(e:Expr, env:Env, k:Cont, h:Handler) extends Handler
  case object HNone extends Handler

  def numVAdd(v1: Value, v2: Value): Value = {
  val IntV(n1) = v1
  val IntV(n2) = v2
  IntV(n1 + n2)
  }

  def numVMul(v1: Value, v2: Value): Value = {
  val IntV(n1) = v1
  val IntV(n2) = v2
  IntV(n1 * n2)
  }

  def numVDiv(v1: Value, v2: Value): Value = {
  val IntV(n1) = v1
  v2 match{
      case IntV(a) if a!=0 => IntV(n1 / a)
      case _ => error()
  }
  }

  def numMod(v1: Value, v2: Value): Value = {
  val IntV(n1) = v1
  v2 match{
    case IntV(a) if a!=0 => IntV(n1 % a)
    case _ => error()
  }
  }

  def booleanVEq(v1: Value, v2: Value): Value = {
  val IntV(n1) = v1
  val IntV(n2) = v2
  BooleanV(n1 == n2)
  }

  def booleanVLt(v1: Value, v2: Value): Value = {
  val IntV(n1) = v1
  val IntV(n2) = v2
  BooleanV(n1 < n2)
  }
  
  def lookup(x:String, env: Env): Value ={
    env.getOrElse(x,error("1"))
  }

  def interp(expr: Expr): Value = {
    cinterp(expr,Map(),HNone,x=>x)
  }

  def cinterp(expr: Expr, env: Env, h:Handler, k:Cont): Value = expr match{
      case Id(x) => k(lookup(x,env))
      case IntE(n) => k(IntV(n))
      case BooleanE(b) => k(BooleanV(b))
      case Add(l,r) => cinterp(l,env,h,lv => cinterp(r, env, h, rv => k(numVAdd(lv,rv))))
      case Mul(l,r) => cinterp(l,env,h,lv => cinterp(r, env, h, rv => k(numVMul(lv,rv))))
      case Div(l,r) => cinterp(l,env,h,lv => cinterp(r, env, h, rv => k(numVDiv(lv,rv))))
      case Mod(l,r) => cinterp(l,env,h,lv => cinterp(r, env, h, rv => k(numMod(lv,rv))))
      case Eq(l,r) => cinterp(l,env,h,lv => cinterp(r, env, h, rv => k(booleanVEq(lv,rv))))
      case Lt(l,r) => cinterp(l,env,h,lv => cinterp(r, env, h, rv => k(booleanVLt(lv,rv))))
      case If(con, tr, fl) => cinterp(con,env,h,x=> x match{
        case BooleanV(b) => if(b) cinterp(tr,env,h,k) else cinterp(fl,env,h,k)
        case _ => error()
      })
      case TupleE(es) => tinterp(es,env,h,k,Nil)
      case Proj(e, idx) => cinterp(e,env,h, x=> x match{
        case TupleV(vs) => if (0<idx && idx<=vs.length) k(vs(idx-1)) else error()
        case _ => error()
      })
      case NilE => k(NilV)
      case ConsE(a,t) => cinterp(a,env,h,x=> cinterp(t,env,h,y=> y match{
        case NilV => k(ConsV(x,NilV))
        case ConsV(h2,t2)=> k(ConsV(x,ConsV(h2,t2)))
        case _ => error()
      }))
      case Empty(e) => cinterp(e,env,h,x => x match{
        case NilV => k(BooleanV(true))
        case ConsV(a,t) => k(BooleanV(false))
        case _ => error()
      })
      case Head(e) => cinterp(e,env,h,x => x match{
        case ConsV(a,t) => k(a)
        case _ => error()
      })
      case Tail(e) => cinterp(e,env,h,x=> x match{
        case ConsV(a,t) => k(t)
        case _=> error()
      })
      case Val(x,e,b) => cinterp(e,env,h,t=>cinterp(b,env + (x -> t),h,k))
      case Vcc(x,e) => cinterp(e,env + (x->ContV(k)),h,k)
      case Fun(ps, b) => k(CloV(ps,b,env))
      case RecFuns(fs,b) =>{
        val vars= fs.map(x=>
          x match{
            case FunDef(n,param,bo)=>n
          case _=> error()
          })
        val clos= fs.map(x=>
          x match{
            case FunDef(n,param,bo)=>
            val clov = CloV(param,bo,env)
            clov
            case _=> error()
          })
        val nenv=env++(vars zip clos)
        for(i<-0 to fs.length-1){
          clos(i).env=nenv
        }
        cinterp(b,nenv,h,k)
      }
      /* //best
      case App(f,args) =>
        cinterp(f,env,h,fx => 
          cinterp(TupleE(args),env,h,xs =>
            fx match{
              case CloV(ps,b,fenv) if(ps.length == args.length) =>
                xs match{
                  case TupleV(vs) =>
                    val nenv = fenv ++ (ps zip vs)
                    cinterp(b,nenv,h,k)
                  case _ => error()
                }
              case ContV(cont) if(args.length == 1) =>
                xs match{
                  case TupleV(vs) =>
                    cont(vs(0))
                  case _=> error()
                }
              case _ => error()
            }))*/

      
      case App(f,args) => cinterp(f,env,h,x => x match{
        case CloV(ps, b, fenv)=> 
          cinterp(TupleE(args),env,h,y=> y match{
            case TupleV(vs) if (vs.length == ps.length) => 
              val nenv = fenv ++ (ps zip vs) 
              cinterp(b, nenv, h, k)
            case _=> error()
          })
        case ContV(cont) =>
          if (args.length != 1) 
            error()
          else 
            cinterp(args(0),env,h,cont)
        case _ =>
          cinterp(TupleE(args),env,h,y=> error())
      })
/*
      case App(f,args) => cinterp(f,env,h,x => x match{
        case CloV(ps, b, fenv)=> 
          if (args.length != ps.length)
            error()
          else
            cinterp(TupleE(args),env,h,y=> y match{
              case TupleV(vs) => 
                val nenv = fenv ++ (ps zip vs) 
                cinterp(b, nenv, h, k)
              case _=> error()
            })
        case ContV(cont) =>
          if (args.length != 1) 
            error()
          else 
            cinterp(args(0),env,h,cont)
        case _ => error()
      })
      */
      case Test(e:Expr, typ:Type) => 
        cinterp(e,env,h, a => typ match{
        case IntT => a match{
          case _:IntV => k(BooleanV(true))
          case _=> k(BooleanV(false))
        }
        case BooleanT => a match{
          case _:BooleanV => k(BooleanV(true))
          case _=> k(BooleanV(false))
        }
        case TupleT => a match{
          case _:TupleV => k(BooleanV(true))
          case _=> k(BooleanV(false))
        }
        case ListT => a match{
          case NilV => k(BooleanV(true))
          case _:ConsV => k(BooleanV(true))
          case _=> k(BooleanV(false))
        }
        case FunctionT => a match{
          case _:CloV => k(BooleanV(true))
          case _:ContV => k(BooleanV(true))
          case _=> k(BooleanV(false))
        }
        case _=> error()
      })
      case Throw(e) => 
        cinterp(e,env,h,x =>
          h match{
            case HNone => error()
            case HandlerV(he,henv,hk,hh) => 
              cinterp(he,henv,hh,y=>y match{
                case CloV(ps,b,fenv) if (ps.length==1) => 
                  val nenv = fenv + (ps(0) -> x)
                  cinterp(b,nenv,hh,hk)
                case ContV(cont) =>
                  cont(x)
                case _ => error()
              })
           })
      case Try(e1, e2) =>
        val newhandler = HandlerV(e2, env, k, h)
        cinterp(e1,env,newhandler,k)
    }

  def tinterp(es:List[Expr], env:Env, h:Handler, k:Cont, acc:List[Value]) : Value = es match{
          case Nil => k(TupleV(acc))
          case a::t => cinterp(a, env, h, x=> tinterp(t,env,h,k,acc :+ x))
        }
}
