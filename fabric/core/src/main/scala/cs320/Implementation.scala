package cs320

object Implementation extends Template {

  def typeCheck(e: Typed.Expr): Typed.Type = T.typeCheck(e)

  def interp(e: Untyped.Expr): Untyped.Value = U.interp(e)

  object T {
    import Typed._

    case class TyEnv(
      tpoly: Map[String, (Type,Boolean,List[String])] = Map(),
      tdef: Map[String, (List[Variant],List[String])] = Map(),
      tvar: Set[String] = Set()
    ) {
      def +(x: String, t: (Type,Boolean,List[String])): TyEnv =
        copy(tpoly + (x -> t), tdef, tvar)
      def +(x: String, m: (List[Variant],List[String])): TyEnv =
        copy(tpoly, tdef + (x -> m), tvar)
      def +(x: String): TyEnv =
        copy(tpoly, tdef, tvar + x)
      def contains(x: String): Boolean =
        tpoly.contains(x) || tdef.contains(x) ||tvar.contains(x)
    }

    def subst(t1: Type, a: List[String], t2: List[Type]): Type = t1 match {
      case IntT => IntT
      case BooleanT => BooleanT
      case UnitT => UnitT
      case VarT(a1) => if (a.contains(a1)) t2(a.indexOf(a1)) else t1
      case ArrowT(ps, r) => ArrowT(ps.map(subst(_, a, t2)), subst(r, a, t2))
      case AppT(t, as) => AppT(t,as.map(subst(_,a,t2)))
    }

    def mustSame(left:Type,right:Type):Type =
      if(same(left,right)) left
      else notype(s"$left is not equal to $right")
    
    def same(left: Type, right: Type): Boolean =
      (left, right) match {
        case (IntT, IntT) => true
        case (ArrowT(p1, r1), ArrowT(p2, r2)) =>
          (p1.length == p2.length) && (p1 zip p2).foldLeft(true)((x,y) => x && same(y._1, y._2)) && same(r1, r2)
        case (BooleanT,BooleanT) => true
        case (VarT(s1),VarT(s2)) => s1==s2
        case (UnitT,UnitT) => true
        case (AppT(name1,targs1),AppT(name2,targs2)) => (name1 == name2) &&
          (targs1.length == targs2.length) &&
          (targs1 zip targs2).foldLeft(true)((x,y)=>x && same(y._1,y._2))
        case _ => false
      }

    def notype(msg: Any): Nothing = error(s"no type: $msg")

    def validtype(ty:Type,tyenv:TyEnv):Type =
      ty match{
        case AppT(name,targs)=> 
          val targs2 = targs.map(validtype(_,tyenv))
          if(!(tyenv.tdef.contains(name)))
            error()
          if(!(tyenv.tdef.getOrElse(name,error())._2.length == targs2.length))
            error()
          ty 
        case VarT(name)=> if(tyenv.contains(name)) ty else error()
        case IntT => ty
        case BooleanT => ty
        case UnitT => ty 
        case ArrowT(ptypes,rtype) => 
          ArrowT(ptypes.map(validtype(_,tyenv)),validtype(rtype,tyenv))
      }
    
    def recTyEnv(recdef:RecDef,tyenv:TyEnv): TyEnv =
      recdef match{
        case Lazy(name,typ,expr) => tyenv.+(name,(typ,true,List()))
        case RecFun(name,tparams,params,rtype,body) => 
          val types = params.foldLeft(List():List[Type])((x,y)=> y._2::x)
          tyenv.+(name,(ArrowT(types.reverse,rtype),false,tparams))
        case TypeDef(name,tparams,variants) if !tyenv.contains(name)=> 
          val semienv = tyenv.+(name,(variants,tparams))
          variants.foldLeft(semienv)((x,y)=>
            if(y.params.length == 0)
              x.+(y.name,(AppT(name,tparams.map(x => VarT(x))),false,tparams))
            else
              x.+(y.name,(ArrowT(y.params,AppT(name,tparams.map(x => VarT(x)))),false,tparams))
            )          
        case _ => error()
      }

    def recvalid(recdef: RecDef, tyenv: TyEnv):Unit =
      recdef match{
        case Lazy(name,ty,expr)=>
          mustSame(validtype(ty,tyenv),tcheck(expr,tyenv))
        case RecFun(name,tparams,params,rtype,body)=>{
          if(!(tparams.foldLeft(true)((x,y)=>x&&(!tyenv.contains(y)))))
            error()
          val ntyenv = tparams.foldLeft(tyenv)((x,y)=>x.+(y))
          for(n <- 1 to params.length)
            validtype(params(n-1)._2,ntyenv)
          validtype(rtype,ntyenv)
          val nntyenv = params.foldLeft(ntyenv)((x,y)=>x.+(y._1,(y._2,false,List())))
          tcheck(body,nntyenv)
        }
        case TypeDef(name,tparams,variants)=>{
          if(!(tparams.foldLeft(true)((x,y)=>x&&(!tyenv.contains(y)))))
            error()
          val ntyenv = tparams.foldLeft(tyenv)((x,y)=>x.+(y))
          variants.foreach(variant => variant.params.foreach(y => validtype(y,ntyenv)))
        }
      }
    
    def tcheck(expr:Expr, tyenv:TyEnv):Type =
      expr match{
        case Id(name, targs) =>
          val targs2 = targs.map(validtype(_,tyenv))
          if(!(tyenv.tpoly.contains(name)))
            error()
          if(!(tyenv.tpoly(name)._3.length == targs2.length))
            error()
          subst(tyenv.tpoly(name)._1,tyenv.tpoly(name)._3,targs2) 
        case IntE(v) => IntT
        case BooleanE(v) => BooleanT
        case UnitE => UnitT
        case Add(l,r)=>
          mustSame(tcheck(l,tyenv),mustSame(tcheck(l,tyenv),IntT))
        case Mul(l,r)=>
          mustSame(tcheck(l,tyenv),mustSame(tcheck(l,tyenv),IntT))
        case Div(l,r)=>
          mustSame(tcheck(l,tyenv),mustSame(tcheck(l,tyenv),IntT))
        case Mod(l,r)=>
          mustSame(tcheck(l,tyenv),mustSame(tcheck(l,tyenv),IntT))
        case Eq(l,r)=>
          (tcheck(l,tyenv),tcheck(r,tyenv)) match{
            case (IntT,IntT) => BooleanT
            case _=>error()
          }
        case Lt(l,r)=>
          (tcheck(l,tyenv),tcheck(r,tyenv)) match{
            case (IntT,IntT) => BooleanT
            case _=>error()
          }
        case Sequence(l,r)=>
          tcheck(l,tyenv)
          tcheck(r,tyenv)
        case If(cond,texpr,fexpr)=>
          mustSame(tcheck(cond,tyenv),BooleanT)
          val b = tcheck(texpr,tyenv)
          mustSame(tcheck(fexpr,tyenv),b)
        case Val(mut,name,typ,expr,body)=> typ match{
          case Some(ty) => 
            val a = mustSame(validtype(ty,tyenv),tcheck(expr,tyenv))
            val ntyenv = tyenv.+(name,(a,mut,List()))
            tcheck(body,ntyenv)
          case None =>
            val a = tcheck(expr,tyenv)
            val ntyenv = tyenv.+(name,(a,mut,List()))
            tcheck(body,ntyenv)
        }
        case RecBinds(defs,body)=>
          val ntyenv = defs.foldLeft(tyenv)((x,y)=>recTyEnv(y,x))
          defs.foreach(x => recvalid(x,ntyenv))
          val t = tcheck(body,ntyenv)
          validtype(t,tyenv)
        case Fun(params,body)=>
          val types = params.map(x => validtype(x._2,tyenv))
          val ntyenv = params.foldLeft(tyenv)((x,y) => x.+(y._1,(y._2,false,List())))
          ArrowT(types,tcheck(body,ntyenv))
        case Assign(name,expr)=>
          if(!(tyenv.tpoly.contains(name)))
            error()
          if(!(tyenv.tpoly(name)._3.length == 0))
            error()
          if(!tyenv.tpoly(name)._2)
            error()
          mustSame(tcheck(expr,tyenv),tyenv.tpoly(name)._1)
          UnitT
        case App(fun,args)=>
          tcheck(fun,tyenv) match{
            case ArrowT(ptypes,rtype) if(ptypes.length==args.length) =>
              (ptypes zip args).foreach(x => mustSame(x._1,tcheck(x._2,tyenv)))
              rtype
            case _ => error()
          }
        case Match(expr,cases)=>
          tcheck(expr,tyenv) match{
            case AppT(name, targs)=>{
              if(!tyenv.tdef.contains(name))
                error()
              val t = tyenv.tdef(name)
              if(!(targs.length == t._2.length))
                error()
              if(!(cases.length == t._1.length))
                error()
              val casetypelist = cases.map(x =>{
                val v = t._1.filter(_.name == x.variant)
                if(v.isEmpty)
                  error()
                if(!(v(0).params.length == x.names.length))
                  error()
                val ntyenv =(x.names zip v(0).params).foldLeft(tyenv)((x,y)=>{
                  val substituted = subst(y._2,t._2,targs)
                  x.+(y._1,(substituted,false,List()))
                })
                tcheck(x.body,ntyenv)
              })
              val fst = casetypelist(0)
              casetypelist.foldLeft(fst)(mustSame(_,_))
            }
            case _=>error()
          }
      }
    
    def typeCheck(expr: Expr): Type = 
      tcheck(expr,TyEnv(Map(),Map(),Set()))
  }

  object U {
    import Untyped._

    type Store = Map[Addr,Value]

    def malloc(s:Store):Addr = {
        s.foldLeft(0:Addr){
          case (max, (addr, _)) => 
            if (max>addr)
              max
            else addr
      } + 1
    }
    def eLookup(id:String, env :Env):Addr =
      env.getOrElse(id,error());

    def sLookup(addr:Addr, s: Store):Value =
      s.getOrElse(addr,error())
    
    //Env for recursive def
    def recEnv(recdef:RecDef, env:Env, s:Store):(Env,Store)=
      recdef match{
        case Lazy(name,expr)=>
          val naddr = malloc(s)
          (Map() + (name->naddr),s + (naddr->UnitV))
        case RecFun(name,params,body)=>
          val naddr = malloc(s)
          (Map() + (name->naddr),s + (naddr->UnitV))
        case TypeDef(variants)=>{
          variants.foldLeft((Map():Map[String,Addr],s))((x,y)=>{
            val naddr = malloc(x._2)
            (x._1 + (y.name->naddr),x._2+(naddr->UnitV))
          })}
        case _=>error()
      }

    //store for recursive def
    def recStore(recdef:RecDef, env:Env, s:Store):Store=
      recdef match{
        case Lazy(name,expr)=>
          s + (eLookup(name,env)->ExprV(expr,env))
        case RecFun(name,params,body)=>
          s + (eLookup(name,env)->CloV(params,body,env))
        case TypeDef(variants)=>
          variants.foldLeft(s)((x,y)=>{
            val news = (eLookup(y.name,env) ->{ 
              if(y.empty) VariantV(y.name,List())
              else ConstructorV(y.name)
            })
            x+news
            })
        case _=>error()

      }
    
    def cinterp(expr:Expr, env:Env, s:Store) : (Value,Store) = 
      expr match{
        case Id(name) => 
          val a = eLookup(name,env)
          val v = sLookup(a,s)
          v match {
            case ExprV(expr,env) =>
              val (v1,s1) = cinterp(expr,env,s)
              (v1, s1 + (a -> v1))
            case a => (a,s)
          }
        case IntE(v) => (IntV(v),s)
        case BooleanE(v) => (BooleanV(v),s)
        case UnitE => (UnitV,s)
        case Add(l,r)=>
          val (IntV(v1),s1) = cinterp(l,env,s)
          val (IntV(v2),s2) = cinterp(r,env,s1)
          (IntV(v1+v2),s2)
        case Mul(l,r)=>
          val (IntV(v1),s1) = cinterp(l,env,s)
          val (IntV(v2),s2) = cinterp(r,env,s1)
          (IntV(v1*v2),s2)
        case Div(l,r)=>
          val (IntV(v1),s1) = cinterp(l,env,s)
          val (IntV(v2),s2) = cinterp(r,env,s1)
          (IntV(v1/v2),s2)
        case Mod(l,r)=>
          val (IntV(v1),s1) = cinterp(l,env,s)
          val (IntV(v2),s2) = cinterp(r,env,s1)
          (IntV(v1%v2),s2)
        case Eq(l,r)=>
          val (IntV(v1),s1) = cinterp(l,env,s)
          val (IntV(v2),s2) = cinterp(r,env,s1)
          (BooleanV(v1 == v2),s2)
        case Lt(l,r)=>
          val (IntV(v1),s1) = cinterp(l,env,s)
          val (IntV(v2),s2) = cinterp(r,env,s1)
          (BooleanV(v1 < v2),s2)
        case Sequence(l,r)=>
          val (v1,s1) = cinterp(l,env,s)
          cinterp(r,env,s1)
        case If(cond,texpr,fexpr)=>
          val (v1,s1) = cinterp(cond,env,s)
          if (v1==BooleanV(true))
            cinterp(texpr,env,s1)
          else if (v1==BooleanV(false))
            cinterp(fexpr,env,s1)
          else
            error()
        case Val(name,expr,body)=>
          val (v1,s1) = cinterp(expr,env,s)
          val a = malloc(s1)
          cinterp(body, env + (name->a), s1 + (a -> v1))
        case RecBinds(defs,body)=>
          val (v1,s1) = defs.foldLeft((Map():Map[String,Addr],s))((x,y)=>{
            val (nv,ns) = recEnv(y,x._1,x._2)
            val ov = x._1
            (ov ++ nv, ns)
          })
          cinterp(body,env++v1,defs.foldLeft(s1)((x,y)=>recStore(y,env++v1,x)))
        case Fun(params,body)=>(CloV(params,body,env),s)
        case Assign(name,expr)=>
          val a = eLookup(name,env)
          val (v1,s1) = cinterp(expr,env,s)
          (UnitV, s1 + (a->v1))
        case App(fun,args)=>
          val (v1,s1) = cinterp(fun,env,s) 
          val (vs, ss) = args.foldLeft((List():List[Value], s1))((x, y) => {
                val (nv, ns) = cinterp(y, env, x._2)
                (nv :: x._1, ns)})
              val (nvs,nss) = (vs.reverse,ss)
          v1 match{
            case (CloV(params,body,fenv))=>
              if (!(args.length==params.length))
                error()
              val paraToValue = params zip nvs
              val (newe, news) = paraToValue.foldLeft((fenv, nss))((x, y) => {
                val a = malloc(x._2)
                (x._1 +(y._1 -> a), x._2 + (a -> y._2))
              })
              cinterp(body, newe, news)
            case (ConstructorV(name))=>(VariantV(name,nvs),nss)
            case _ => error()
          }
        case Match(expr,cases)=>
          cinterp(expr, env, s) match {
            case (VariantV(name, values),s1) =>
              val filtered = cases.filter(x => (x.variant == name))
              if (filtered.length == 0) error()
              val Case(variant, names, body) = filtered(0)
              if (!(values.length == names.length)) error()
              val nameToValue = names zip values
              val (newe, news) = nameToValue.foldLeft((env, s))((x, y) => {
                val a = malloc(x._2)
                (x._1 + (y._1 -> a), x._2 + (a -> y._2))
              })
              cinterp(body, newe, news)
            case _=> error()
          }
        }

    def interp(expr: Expr): Value = 
      cinterp(expr,Map(),Map())._1
  }
}