
object BigTestString {
  val s =
    """
      |list: {
      |	:{
      |		size: stksize
      |		{nil node} nil $size >= 1 .if dup >>last
      |		fold $node >>next
      |		__parent__: $proto.list
      |	}
      |}
      |
      |node: {
      |	:{>>next >>value}
      |}
      |
      |proto.list{
      |
      |	// Retrieve value element
      |	head: {
      |		$next.value
      |	}
      |
      |	tail: {
      |		$last.value
      |	}
      |
      |	get: { n =>
      |		$this
      |		$n repeat {$.next}
      |		$.value
      |	}
      |
      |	set: { n v =>
      |		$this
      |		$n repeat {$.next}
      |		$v >.value
      |	}
      |
      |	insert: { n v =>
      |		$this
      |		$n repeat {$.next} >>a
      |		node($v $a.next) >a.next
      |		if($n = $size {$a.next >last})
      |		$size incr >size
      |	}
      |
      |	remove: { n =>
      |		$this
      |		$n decr repeat {$.next} >>a
      |		$a.next.next >a.next
      |		if($n = $size {$a >last})
      |		$size decr >size
      |	}
      |
      |	prepend: { v =>
      |		node($v $this.next) >next
      |		$size incr >size
      |	}
      |
      |	append: { v =>
      |		$size = 0 .if({
      |			node($v nil) dup >next >last
      |		} {
      |			node($v nil) dup >last.next >last
      |		})
      |		$size incr >size
      |	}
      |
      |	open: {
      |		$next {n => $n.value $n.next} until {dup = nil} pop
      |	}
      |
      |	sort: {
      |		$next $size repeat {
      |			$:next dup >>n swap >.next
      |			p: $this
      |			$next {dup >>p $.next} until {$:value > $n.value}
      |			$p.next >n.next
      |			$n dup >p.next
      |		}
      |		$this $size repeat {$.next} >last
      |	}
      |
      |	deleGet: { i =>
      |		this.get($i)
      |	}
      |
      |	sort: { // works on: k: list(2 5 3 4 1)    // beware of j possibly not binding to 1
      |		(reverse(range(2 $size)) { i =>
      |			v: this.get($i) this.remove($i)
      |			j: 1 {$j incr >j} until {$j > decr($i) .if(true {this.get($j) > $v})}
      |			this.insert($j decr $v)
      |		} until {stksize = 0})
      |	}
      |
      |	join: { sep =>
      |		cat($next {n => $n.value $sep $n.next} until {dup = nil} pop pop)
      |	}
      |
      |	string: {
      |		cat('[' this.join(', ') ']')
      |	}
      |}
    """.stripMargin
  val s2 =
    """
      |// Dragon 1.0
      |
      |// stack
      |swap: {(. .)}
      |dup: {x => $x $x}
      |pop: {x =>}
      |//pip: $native.pip
      |rot: {(.. .)}
      |pull: {n => (pip (n - 1) .)}
      |sendback: {v n => ($v pip $n)}
      |//stksize: $native.stksize
      |reverse: {stksize range(1 .) .map({})}
      |
      |// memory (native: getv, setv, bindv, hasv, remv, lsv, getp, setp)
      |root: $this
      |makev: {tbd}
      |hasm: { o k =>
      |  $o is root then false else {
      |    hasv($o $k) then true else {
      |      hasm($o getp $k)}}}
      |
      |// boolean
      |proto.Boolean{}
      |true{ setp($this $proto.Boolean)
      |  cond: {a b => a}
      |  and: {a => a}
      |  or: {true}
      |  xor: {a => a.not}
      |  not: {false}
      |  string: {'true'}
      |}
      |false{ setp($this $proto.Boolean)
      |  cond: {a b => b}
      |  and: {false}
      |  or: {a => a}
      |  xor: {a => a}
      |  not: {true}
      |  string: {'false'}
      |}
      |
      |// number
      |proto.Number{
      |  sign: {$this > 0 then 1 else {$this = 0 then 0 else -1}}
      |  abs: {$this < 0 then {0 - $this} else $this}
      |  to: {b => $this abs($this - $b) repeat {dup + 1}}
      |  xmod: {l h =>
      |    d: abs($l - $h)
      |    $this dup > $h then {
      |      {- $d} until {dup <= $h}
      |    } else { dup < $l then {
      |      {+ $d} until {dup >= $l}
      |    }}}
      |  string: $sys.intern
      |}
      |
      |// list
      |proto.List{
      |  head: {self.len()}
      |  string: {cat('[' self.join(' ') ']')}
      |}
      |
      |// combinators (native: map, cloop)
      |filter: {f => {$f then $n} siter}
      |fold: {f => {stksize > 1} while $f}
      |repeat: {n f => $n {n => $f} niter}
      |apply: {x => x}
      |until: {f c => {f c} cloop}
      |while: {c f => {c >>x $x then $f $c} cloop}
      |
      |// global funcs
      |sys.g: {fn =>
      |  {a b => hasm($a $fn)
      |    then {$b $a $fn getv id}
      |    else {throw($fn ' is undefined' cat)}}
      |}
      |('add' 'sub' 'mul' 'div' 'mod' 'pow' 'divi' 'to' 'and' 'or'
      |  map {n => bindv($root $n sys.g($n))})
      |
      |int: {n => $n - ($n % 1)}
      |abs: {n => n.abs}
      |str: {n => hasm($n 'string') then {n.string} else {native.str($n)}}
      |cat: {map $str fold {a b => a.concat($b)}}
      |join: {sep => map {$sep} pop cat}
      |open: {c => hasm($c 'open')
      |  then {c.open}
      |  else {($c lsk map {dup $c swap getv})}}
      |
    """.stripMargin
}
