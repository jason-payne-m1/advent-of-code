package advent.of.code.twentytwentyone

import scala.collection.mutable

object Advent2021_10_2 {

  val syntaxScores = Map(')' -> 1L, ']' -> 2L, '}' -> 3L, '>' -> 4L)
  val chunkMap =  Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')
  val chunkMapReversed = for ((k, v) <- chunkMap) yield (v, k)

//  val input = Seq("[({(<(())[]>[[{[]{<()<>>","[(()[<>])]({[<{<<[]>>(","{([(<{}[<>[]}>{[]{[(<()>","(((({<>}<{<{<>}{[]{[]{}","[[<[([]))<([[{}[[()]]]","[{[{({}]{}}([{[{{{}}([]","{<[[]]>}<{[{[{[]{()[[[]","[<(<(<(<{}))><([]([]()","<{([([[(<>()){}]>(<<{{","<{([{{}}[<[[[<>{}]]]>[]]")
  val input = Seq("[{<(<[<<(<{[[<[]{}><()[]>]]<(([]<>)<(){}>)({[]{}}([]()))>}>{{{<(()<>)[[]()]><(<>())[[]<>]>}{({()","<<[{([[{{[[[({()()}[<>[]]){<<>[]>([]<>)}]{[{{}{}}<{}{}>]{{[]<>}{[]{}}}}][{{{(){}}<<>[]>}}]]}{","([<{([<[([({[{[]{}}{{}{}}]}{[{<>{}}{(){}}]})[{[[(){}][(){}]]{<<>[]}{{}[]}}}]])]><({{{[[{{}{}}]<{<>","{{((<([<[<[((<{}>{()()})<[{}[]]{<>{}}>)<({[]{}}([][])){{[]<>}{{}<>}}>]>]><[[[[[<<>()>(()[])](<[][]>{<>{}}","{[[<[[((<[{{<[[]{}](()[])>}{<({}[])[[]<>]>}}{[(<()()><()>)({[]()}[<>()>)]}]((<{[<>[]]{{}()}}>[{<()<>>{[]","({{((<[{{{([{[(){}][<>]}([<>()][[]()])]{<{{}<>}<{}<>>>{{()[]}{()()}}})<<[(<><>)<[]()>][((){}){[]{}}]>>}}","{<(<{[[([[(<[(<>[])]<{()}<()[]>>><{<{}[]><[]>}(<<>{}><()()>)>)]]{((({{<><>}{{}{}}}{<[]()><{}<>>})<[{(","<[{(<{<((<(<{(<>{})}<<[]>(<>{})>>){{(<()[]>[[]<>])}[{[[][]]<<>()>}{[{}()]<(){}>}]}>{(((<<>","(<[{[<((((<[[[{}{}]{{}<>}]({[][]}[(){}])][{(<>[])<[]<>>}<[(){}]>]>)))<({{[<([]<>)<()<>>>[{","{{<<[<({[{([(<[][]>{[][]})<<<>[]>([]())>])[(<<[]<>>[{}()]>{([]{})})]}](<[<(<<>[]>)>{(<[]{}>[<>[]])<<[]()>((","[[[{<{[[[([[<<<>>[{}()]}[({}[])<()<>>]]]{<<<[]{}>[<>[]]><{[]<>}{[]{}}>>{(<{}{}>{<><>})({[]()}{{}<>})}})]<<<[(","(({([<{{{{<<<({}())[()<>]>{[[]()][()[]}}>>}<({({{}<>}[[]{}])(({}{}){()()})}{[(<>{}){<>{}}]{[<>[]]({}<>)}})>}(","{(([{(([<[{(<{[]<>}<()<>>>[([]{}}(<>{})])}({[<<>[]><<>{}>]}<[<()[]>[<>{}]]{<()<>>{(){}}}>)]>([<((<{}()>","({{<{{{{{{({<{<>{}}>(<<>[]>[[][]])})]{[{{{(){}}[()<>]}<[<>][[]{}]>}{[<{}<>>{<>}]{{[]{}}(<>{})","{{(([{([{([<([[]{}]{<>[]})>{(((){})[()()]){{{}<>]<()()>}}])(({{{<>{}}{[]{}}}[((){})[<>]]}<{(()[])[","{<[((<<(([{{<{[][]}>{[()<>]{()[]}}}}]))<<((<[<[][]>[[]()]][({}){[]<>}]>))>>>>))<(<<([<<[{<<><>>[(","[([[(<([(({<([{}][<>[]]){<{}{}><{}[]>}>(<({}[])>(<()()>))}{[[{{}()}{<><>}][{()[]}<()()>]>[{<<><>>{","[{[<{(<[([[[{[{}<>]({}())}(<<>>[[]<>])]{({<>()}{()()}){{[]()>{[]()}}}]]{[[{({}()){[][]}}((<><>))][[","<{(<[<<<{{[{{<{}<>>[[]<>]}[{()<>}[()<>]]}{[{{}[]}{()}]{<[]<>>([][])}}]({[[{}]{()<>}](({}{}))}>}{<[<({}()){()","<(<[<([({[<([<[]{}>{[]<>}]{{[]{}}{{}{}}})<<{()<>}({}{})><<[]()>{()<>]>>>(([{[]()}({}())]([[]()]<{}()>))","{[<<([[<([<<(<[]<>>){<<>()>(<><>)}><<{<><>}[{}[]]>([{}()])>>])>][{({[<<[<>]{<>()}>><<[<>()]{{","<<<[<<{[<<(({<{}<>>{{}<>}}[<[]<>>({}[])]))><{{<[[]()][(){}]><{{}()}(<>)>}[((<>{})(()<>))<[(){}][[][]]>]>>>]}","(([{{[{{<<{{[({}<>)(<>{})](<(){}><{}{}))}}([{({}())<<><>>}{<(){}>}]{{[{}{}]{(){}}}<([]<>){()()}>})","({{([{{({[{<<({}[])(()[])>[{{}()}[{}{}]]><<{()<>}{{}}>>}<[((()<>)([]{}))[{()()}(()<>)]]{[{(){}}{()","<[<{<[{({(([((<>{}){()[]}){{{}{}}{[]<>}}]{{({}<>)<{}[]>}<(()<>}(()[])>}))(<{({<><>}[[]()])<[{}()](","[[{[({{<{{{{[<<>[]]({}{})]<([]{})(<>{})>}{{({}{})[{}{}]}{<<>[]><{}>}}}{<(<<>[]>{{}{}})[(()<","[([(<{{[<[{[<(<>())<[]()>>{{{}}<[][]>}][[{[][]}<{}[]>]({{}()}[[][]])]}{{{<{}{}><{}<>>}{[()[]]<()[]>}}<<{[]{}}","[[(((({<<[<<({{}()}){<()()><<>[]>}>{((<><>)({}())){<[]{}><()[]>}}><[{<{}>}({{}{}}[<>()])]{{({}{})}[<","((<<[{{[[([[<[{}()]((){})>[{()[]}<<>{}>]]([<{}()>][{{}[]}[()]])]{({[()()][[]()]}(((){}){{}{}}))[{(<>())}","{[[<{<{[{{[(<<<>[]>([]())>{(()[])[<>{}]}){<(<>())(<>{})>{<[]<>>[{}[]]}}]({<<<>[]><{}()>>([[][]])}<[<{","[(<[<{<[([[{{{<>[]}[[]{}]>({[][]}[[][]])}{[(<>[])[[]]]({()()}({}[]))}]])<[<<<{<>{}}{[][]}>((()){()<>})>>","({<[<<[{[<({<{{}{}}{<>[]}>{{()}{[]<>}}}[<[{}<>][()[]]>])((<([]())((){})><[(){}]{{}()}>)[{<[]{}>[{}{}]}[","<[[([([(<(<[[(()[])]{[[]<>]({}{})}]>)>[<[{<<<>()>({}{})><([][])<{}{}>>}<([()[]]<(){}>)[([]{})[{}<>]]>]><[{({","[<{([([(<{<<{[()[]][()}}<<{}{}>>><<{[]<>}((){})>[{[]()}]>>}[(({{<>[]}[<>()]}<({}{})[{}[]]>)<[{<>{}}([][])","[<{([((<{({{[<<>()>{[]{}}](<[]()>[{}{}])}})(<{([<>()]{[][]})[{{}()}]}<((()[])(())){([]<>)([]<>)}>>{((({}()){","((<{{{<<[[<[[{[]<>}<{}[]>]<<(){}><<>>>]<<[[][]>[[][]]>{{<><>}{[]{}}}>>{{{{<>[]}{<>{}}}[[[]<>]<{","[<<[{[<({[[{{{{}[]}[<><>]}{<<>[]>[{}[]]}}<<(()[])[<>()]>[[<>{}]]>]]}{<({<([]())(<>())>[[{}[]][<>{}]]}){([","([<{({({{<[({(<><>){(){}}}<{[]{}}{[]())>)({<(){}>[<>[]]})]<[([{}{}]<{}[]>)[[()[]]]]{[{{}<>}]<(()<>)","[([{<{<{(<<(({[]{}}({}()))){{(<>())(<><>)}(([]{})<{}[]>)}><[<[{}{}]({}{})>]{[<[]()>(<>{})]<({}()","((<{[(<<[([<{([]{}){<><>)}[[[][]]({}())]>[[{<>()}[()<>]](({}<>)([]))]][<((()[])<()[]>)[[<>()]]>])<<{[","{[[<{<<[{{(((<{}[]>(()))[<<>()>{[][]}])){<([[]{}]{{}[]})<{{}()}{()()}>}}}[{[[<(){}>{{}()}]","{[{[<<[{[{([(<[]()>(()<>))]<{[{}<>]<{}()>}<{{}{}}[()[]]>>)[{{<{}>(())}<(<>)[<>[]]>}(<{[]<>}[()()]]<{<>{}}","[{{[{{[<{[(<{[()()][<>[]]}<([][])(()<>)>>({{{}()}[()<>]}[<[]{}><[]}]))([<<<><>>({}[])>({()[]}[<>])][([(","[({<[[<{<[(<(<[][]><[]()>){[()()]{()()}}>)[<[<()>(()[])][(<><>)]>((({}())<<>[]>))]]<[{[<<>{}>[{}<>]](<{}{}>((","<{<(<((<<[[<([<><>][()[]])(<<>{}>[<>[]])>]({<<[]>(()<>)>(((){}){<>[]})}{[[[]](<>())]<<(){}><<>[]>>","[<{(<{{<(({(<<[]()>><<()<>>>)<{<{}()><()()}}(<[][]>{{}{}})>}[{[{[][]}[()<>]]{([][])(<>[])}}[{[{}()][","{{(([[[{[{({([()()]([]<>))<[{}{}](<>())>}{{{()<>}{()()}}({()[]}[{}{}])})<([([]{})<()[]>]{<<>()>})>}[[","(((<[{<[{([[{[[]<>]{<>{}}}[[{}<>](<>{})]]<<[(){}][[]{}}>{[[][]]}>][{{[[]{}]([]{})}[([])<<>>]}","<[<[[([[[[{{{([]()){(){}}}<[[]{}](<><>)>}}]]]{([[<[(()())(()[]]][[<>{}]<<>[]>]>(<[<>]<<><>>>({{}{}}([]{","({(([<<[{{[(<({}<>)<[]<>>>(<{}<>>{(){}}))]{(({{}[]})({{}<>}((){}]))[({{}()}[()<>])<({}()){<>()}>]}}}[<","<[<({[[<<([[((<>())[<><>]){[<>[]]<<>>}]]{([<{}[]>{<><>}])})<<[<([]<>)(()<>)>(<{}()>([]<>))]>[","({({{[[<<([[({()[]}[<><>]){(()<>){<><>}}][{{<>[]}(<><>)}{[[]<>]<[]>}]])>([[<(({}[])<()>)(<()>[<>{","<([{(({<[{([<[[]<>]<()<>>><(<>())({})>][[(<>[]){[]{}}]{(<>[])<()[]>}])({[{{}()}{[]()}]}{{[[]{}]({}<>)","[([{({(({{<<<<(){}>[<>[]]>([<>()][<>[]])>[{(()())[()<>]}<{[][]}[<>{}]>]>}<[<([[]()][<>()])((<><>){[]})>","<({[{{[{{[([{({}()){{}{}}}<(()())[[][]]>][<[<>()][{}[]]><<<><>>>])([[({}{})(()<>)]{<<><>>([]<>)}]{{<()[]","<{{<{(([(<<<{{[]()}[[]()]}>{{[{}<>]<()<>>}[{{}{}}[{}[]]]}>>([([<()()>[{}<>]])([<()[]>[<>()]]{([]{})}","{<({<{[{{({[<{{}()}[()()]>{({}<>)}][<<()()>{[][]}>{<<>()><{}{}>}]})}{(<{<[()()]>}<{({}){()()}}(<[]<>><","(({{({{{<[[({((){})[<>[]]}[{<>{}}[[]()]])]{<<(<>){{}()}>[<<>>{{}{}}]>([(()[])][([]())<<>()>])}][<[[","[{[[[{<<<[<{({[]()}({}{}))[{<>}{<>[]}]}>[((<<><>>[[]<>])[[[]<>]<[][]>]){[[{}()][{}<>]]({[]","<<[{({[[[<[[[<[][]>]{{[]()}(()<>)}]{[[(){}}<{}()>][{<>{}}({}{})]}](({([]{})<()()>}[({}())]))>(<{(<(){}>)}","({<([[(<(({{<{{}[]}[[]<>]>}[<(<>[])<{}{}>><[[]<>][[]{}]>]}<[({<>{}}{{}()})(<[]()>(()<>))]([<{}()","((<[{[{[{{<[{[<>{}]}[<[][]><()>]]{{<()[]>}<[(){}]{{}<>}>}>}<(([[[]{}]<[]<>>]{[{}{}]<()[]>})[[{[]{}}{{}","[([(<([<(<<{(<{}{}>{()<>})[([]())[[]]]}[{{[]<>}((){})}<{{}[]}([]())>]><{[[<>()]]<({}())[{}<>]>}{[({}<>)","[{[<([<{<{{[[{{}{}}{<><>}]<[[]()]{[]{}}>][{({}{}){[][]}}<<{}{}>([]()]>]}[<<({}<>){()<>}>({(){}}<{}{}>)><([{}","<{((<{{<{({(([<>{}]{()<>}))(<({}[])(<>())><{{}()}[<>[]]>)}]}(<{{(({}<>)[{}{}])<<<>[]>>}}((<<<","<[<({<({{([{<<<>[]><[]{}>>(<()()>({}[]))}(((()())<{}[]>))][[[<(){}>[()()]][[{}[]]]]])}([(([{[][]}(()())][{()","[({{[<((<{{{<[[][]]({}{})>{(<>[]){[]}}}([{()[]}{[]{}}])]}{[<<[(){}]<[]<>>>{({}<>)([]())}>]","{[{(<({<[[(<<({}<>){[]{}}>(({}{})(<>[]))>({[()[]]<<>{}>}<[[][]]({}())>))(([[<>[]]([]{})][<{}>>){{[[]","{<([[(<<[(<{<<<>{}><<>>>(({}[])<()<>>)}<{<[]{}><()()>}[{(){}}]>>){<<<{{}{}}{()<>}>{{()[]}{()}>><<<<>[","<{[{(<{[<<(<<([]{})(())>[({}[]){{}()}]>)(<{[[]()]}>[{(()<>)(()<>)}{[{}<>]{()()}}])>[(({<<>[]>({}[]}}{([]","{{{([{[([[[{[{(){}}{[]()}][<<>()>]}([[[][]](<><>)])]]]{(<[(<<>[]>[{}<>])(({}[])({}{}))]{<([]{})<[][]>>((","({{[(<<<[({<[{[]<>}<{}<>>]<<()>[(){}]>>[<<<>{}>{{}[]}><({}{})>]})][(<{[(<>())]}(([<><>){[][]})","([[([{[<<{(<{<[]()>[(){}]}[(<><>){()<>}]>){{[<()()><<>[]>](<[][]>{[]<>})}<[{<>()}{()[]}]{{<><>}[<><>]}>}}{<<","<[<([<[(({<{{<(){}><<>[]>}{{<>{}}((){})}}<<[{}]({}[])}{<()()><()<>>}>>[[[{<><>}([][])](({}<>)((){}","<<[<[[<(<{(<{[[][]]{(){}}}<{{}()}<()[]>>><{<{}>}{{<>{}}[()<>]}>)}><{[<{(()[]]<()()>}<{(){}}","([([(<({{(<<{[[]<>]({}())}{{[][]}<()[]>}>>)}})(<{{<({<()[]>([]{})}[{{}<>}[<>()]])><[({<>()}{<>[","<(({{(<(<[{<[{{}[]}[<>{}])(<()<>>({}{}))>[<[<>[]]{{}}>[({}<>)[[]{}]]]}<[[{[]<>}{[]<>}]<(()()){<","{(<(([{([<({[[<>[]]{<>[]}]{{[]<>}{()}}})<[<[[]()][<>{}]>{[<>{}]<()<>>}]<(<[]>[[]()])([<>()](()<>))]>>])","{{[([[<<([(<{{{}[]}<()[]>}({[]{}}[[]{}])>(<{(){}}<(){}>><[[]{}]>))])>{{<<{<[()[]]<[]<>>>}{{(()","{{<[{[<([({<[{[][]}<<>()>]{{[]{}}({}())}]({{[]<>}}(<()<>>(<>[])))}([<[()()][[][]]>((<>{}){()<>})]","{[[{<{<<{((<<<<><>><[][]>>([()<>]<{}[]>)>{((<><>)({}()))([{}{}][<>])})<({({}[])({}())}<[()[]][{}<>]","<({{{(<{({<<(<[]<>>{{}})>[[({}<>)(()())]{[[]{}]({}{})>]>({<<()<>>[<>[]]>((()<>)([]{}))}[[([]<>)<[]<>>]{{{}<>}","{<[{([([{<[{<[<><>]([]())>((<>[])([][]))}]>(({[<[]>[[]<>]]{{{}[]}{[]{}}}}){(<{{}{}}([]<>)>)})}","(<[{{<([<[({<{{}{}}><{{}<>}([]{})>}<<[()()]{{}()}>[[[]()]({}{})]>)[[{<()[]><[]()>}<[<>()][(){}]>]]]<{(<<[]{}>","<{([[<{<([({{[<>{}]({}[])](<[][]>{{}[]})}{{{<>[]}<{}()>}<[{}[]]{{}()}>})([<[[][]]{<>()}>[<<>()>({","<{[{(<<{[<(<{{()<>}{[]()}}[([][])<{}[]>]>[[[{}<>}[()[]]][({}[]){()<>}]]){[<[{}[]][[][]]>(<{}>([][]))][[","{[{<([<<({<<(<<><>><{}[]>)[(<>()){[]{}}]>(<{{}<>}[[]<>]>([{}{}]))>}([{<(()[])[()<>]>{{()()}}}[[<[][","<{(<[{<<[[[({([]{})([]<>)}{([][])([][])})[[<[]{}><<><>>]<[{}<>]({}[])>]]<{[[{}{}]({}[])]<{[]<>}(()<>)>}[({<","(<[{([[[{[<([<{}[]>][(<>[])(()<>)])<[{[][]}([]())]>>][{([(<>())[()[]]])}({({()[]}[{}{}])}{[<[]<>>([]()","<({{[([[<<[(({{}}{{}()})<<()()><<><>>>)<[<{}[]><()<>>][<<>{}]([]{})]>]>>{<((<<[]>(())>{{()<>}}","[[[[<{[[({[{{[()[]]([]())}}<<{{}<>}(<>)>({{}{}}<[][]>)>]<{(<<>()>{(){}})[{()()}<<>>]}([{{}]<{}<>>][[()()](","(([[<(([{<({<<()()>>{<()<>><{}{}>}}{((<>()){()[]})<([]{}){()[]}>})>(<[[<[]()>{[][]>][[[]]<<>","{[<[({<(<<[[((<>())(<>[])){(<>[])(<>[])}][({[]{}}{{}<>})[<(){}>((){})]]]>>[<<{<<()[]>[[][]>>(","{<<(<([<<<((({[][]}{<><>})[{[][]}([]{})])<[<{}{}>({}())][((){})[[]()]]>)<[{(()<>)(()[])}](<[")

  def main(args: Array[String]): Unit = {
    val inputIncomplete = input.filterNot(isCorrupt)
    val incompleteScores = inputIncomplete.map(incompleteScore).sorted

    println(incompleteScores)
    println(incompleteScores(incompleteScores.size / 2))
  }

  private def incompleteScore(navSubsystem: String): Long = {
    val stack = mutable.Stack[Char]()
    navSubsystem.toCharArray.foreach(c =>
      if (chunkMap.contains(c)) {
        stack.pop
      } else {
        stack.push(c)
      }
    )

    var score: Long = 0
    while(stack.nonEmpty) {
      score = score * 5L + syntaxScores.getOrElse(chunkMapReversed.getOrElse(stack.pop, '0'), 0L)
    }

    score
  }

  private def isCorrupt(navSubsystem: String): Boolean = {
    val stack = mutable.Stack[Char]()
    navSubsystem.toCharArray.foreach(c =>
      if (chunkMap.contains(c)) {
        if(stack.isEmpty || !(stack.pop == chunkMap.getOrElse(c, '0'))) {
          return true
        }
      } else {
        stack.push(c)
      }
    )
    false
  }
}