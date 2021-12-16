package advent.of.code.twentytwentyone

object Advent2021_8_1 {

  val input = Seq("badc bd dbeaf cfdbge dfb cfbdea efbag edcfgab dcafe degfca | eacfd acdfbe cbdegf fcbaedg","    cd fdbac egcfab gbadcfe cfgdeb cbadfe deca cdf dfabg abefc | dcf cfbad gbafced fcd","    cg agecfb cbg eabgfdc egdc fdgba bafecd cbdfe bfcdeg cfgdb | efdcb adcfeb fbdcg gbc","    bfceg gfadb dbcfgea bgaef efad abe bcdgfa ea fbdgea agecbd | eadf ceadbg abfge fecdbga","    gdcafe eacb adc gbfda afdceb edgbcf badfc ecgbafd ac fdbce | ebfcd cefdab bdfgeca egbdacf","    aefbgd fbdc fbecg egdcb edcbag bf gcefa fbedcg bagfced fbg | afgce cfbd bdcf geafdbc","    ebgacd edacg gcdeaf cabfg dbc bd cdbgaef ebad fecgbd acbgd | dgbac db caegbdf gcbfa","    defbga gead ge efagcb gdafbc cegbdfa egf bfdce bgdef dgabf | gef eg bfcdag ge","    bae adcgfbe cdfeag ab dcgeb bfdage agecb cagfe abcf bfcaeg | bae aeb agfceb afceg","    ae adefcb gaceb gadceb edag efadcgb badgc cfgbe cadbfg cea | ceabgd fbeacdg aged aged","    aegbdc dbegfac gcbaf df febd egdafc deabg gdf gdfeab adfbg | bcdegaf dfeb bcegad df","    aegcdfb becdaf gdecf ag fag dcfgba badg fcbad cdgaf acefgb | abcdf cefdab ga ga","    gbcfad cefadg cegad aefdb gedacb gcfe fc cfbgead cfade caf | degac egcf geacfbd afc","    bcfad afcdbe fd ecfba cfd afde cfeabg gcbfed dacbg fdgabec | cafbged edfa fcd dabgc","    afdc adbfe bgecfad ecagdb dbcafe dfe fd agbfe bdace gdbfec | fadbe efbdgc fadbe edcba","    bdface geba ecbdag acegd afcdg ge eadbc bfecgda edg fgbdce | eg abge fadgceb abge","    fagbde fcegbd gec efcdg fdgbe ebfcag gc adcfe cgdb cagbdfe | defcg cbgdfea gcbd ecfgd","    acfbg debgf gfcdba edcbfa fbdag fgbeca cfbagde gadc da bda | dacg acbgf cedfgab caefdgb","    ca dfegca edcbgf gfcde aedbg gfdeacb dcbfea facg dgcea ace | fedcg fdagec cea ac","    acdf gcfde debcga ecbgaf cf ebgdf bagfdce cedag cef gedafc | dgcbea fc dcgeaf cdaeg","    ebagf cabgfed ed cbgad afcbge afdbge edg eagbd cedgfb aefd | fcgdbe de cgbeaf ed","    fbcdeag cafbd fgcbea fdgacb daecfb cg gca defga gbdc fdcga | abfcged agc acfgd cbfaeg","    dacfe ed ebdcag gcadfe dfbcaeg dce dfeg fbcadg bfeca acdfg | fcbdga gcbade aegdcf fbcgda","    cfdegb cbef fgdec gdfbca gdecbfa egfdca bc gbc cbegd aedgb | befc bc abegd ebcdfga","    agcefdb fdbg bf bdafe gdebaf eafdg acdbe gcebaf bef edcfga | bf fdbg efdab gefda","    gcb bdcgef dcbfg bg edbg facegb cbedf edgafbc dfgca edacbf | gb cdebfa deagfcb bg","    eafbg gfad afegbd caefbg befdgc ebagd gd bdg cfbdage acdeb | gd geafb cabed dbg","    bcedfg dc dgc aedfbcg acgbed bgfead gedbf bfdcg cedf facbg | gabfed cdebag dafegb cd","    fa adbceg afecd cafdbge eacgfb daegcf aef bcdfe fdga daceg | acedf afe fae afdg","    cdfaeg agcfb bcdgea dcafeb cef fabec ef dfgcbae defb eabcd | gdabce fdeb ebdf ecf","    cdefg ceafg gebfd cdg dc efbcdg cdgbea cfedbga cbfd fdagbe | fbcd afdgbe fabedg dcfb","    cdafgb egdb fecgabd cbfdg fcbedg feb acefg begcf be dacebf | be be ebfdca abgfcd","    eafgcd facegb acb cdbfaeg fagec dgcabe baefd cgbf cb afecb | bcafeg fcgb gfdeac eagcf","    ecgdfb deb edfgab eb faedcg agfed bfaed dbacf gbae efgdcba | fabde eb ecdgfb eabg","    aefbg bdegfa gacdb gefc fbc fc bfdcae gefbac cbfag dbgefac | ecfg fc cfge efgcdab","    gcfbed gcbae dcaef cbegad bdag bgeacf adefgcb deg adgec gd | agcbde bgad gdbfec gcfabe","    dfacb egbcda dac agdcfb fecab adecfgb bgadf cd fgeabd cgfd | gabfdec acd dc dc","    bfecg dcgeab af gadf aedbg caefdbg bgfade bafeg abf eabdcf | ebcfg gafd ebacfd fgbdace","    ge fdeab gcebfda fadcg fgead abcdef eag ebdgfa acgbef dgeb | cdagf cfdgbae fdeab eag","    dba dgfceb fbcegad bagde bcdeag bcedg bdafce agfbe ad cgad | bdcafe agdc ebdcg cdga","    edfg fedac fbegcad acgfe fabcd ead bgefac de daecgf bcaged | bafcd ade bacefg fdbac","    gdecbf edabc ef fcgead fcadg bdcfga fec ceadf dgaefbc agfe | faedgc ef gabfcd fe","    deafg eb egbaf cadegf fcdeab degb bcgaf bfe bfcdega gefdba | abgfe eb gfdaeb efdcga","    fabcd beadf efabgc fc cafdgb gcfd adfcgeb abgdce cfb dgcba | dcgf bdcgaef gdcf cfdg","    ebac acedf ceadfb cfe dbcgaf ec efcgdab degaf abfcd edgbcf | ce edgaf dgbceaf fce","    ecbag fg gacdfe edafgcb cgefa eacdfb deafc bdagcf gcf dgef | fged gedf cefdga cabge","    dgebc bcg cg gdeabf cgfeba gdfc gefbdc adbec bdefg fgcdbae | begdf cfgd eadbc gbfde","    aefdb ecgb dgaeb agdbce ega fgdacb bcadg cbdeafg ge edacfg | age fcbdaeg gea bcfegda","    cdeabg fcgda bdagc acfb dfa af cdeabfg efdgab fbcgda decgf | bcagd cfba gedfc af","    ceadb caebgfd bdfaeg cbgaf ge ega gadcbf gfce ebgcfa gebca | aecdb febgcad bgcaf fadcgbe","    daegb gfdae ceagfd bgfade feagcb fbgd bg cbagdfe ebdac ebg | gefbacd dbgf geb adgbfe","    ecadfg acdfb aefgc ecbfdag dce cfbega fcdae edga gbecdf ed | fcdba ced egfcba dega","    defgbc cgdea efag fdbcgae acbdf gcdaeb fe dfcega efc defca | fage fec dcafeg ecagfdb","    bg dbcafg gdafecb dgfcae defcg gebd cefgb bgfecd fbaec fbg | gbf dfgec bedg cfdgae","    ebgda ebd adce cdgaeb edfcgb bcagd agdcebf ed agbef gcbfda | efagb gbefa dbcfgae eadc","    ebcfdg dbe de cdabgf fgdab dgae aecbf abfgced edbaf eagdfb | bface cdafebg gadcefb fedab","    agc dabcge agbcd cabe dbagf cfgeadb gecafd gbcdfe ac gbced | cbegd beac dgbce ac","    fdagb bdeacf fga eagd gcfdb gecafb ga feabd abfgde dcbfega | bdafe abgcfe gbecfad abdgefc","    fbecd dbaef bdgc debafgc daecgf ebfcgd ebc abegfc bc gfced | fadecbg cb cdgeaf bfade","    fagbe aebfgcd gef bgadf cbagfd deafbg cfdeag ge begd beafc | bcfadeg edbgaf fcgaed ge","    gabfc gabde fbdga bfegca agfbcd df cdfagbe dcfg caefbd dbf | decfagb bdf gfcd fd","    bdafeg gf gbadf gefb cgefda daecbfg bacfed cdagb fdg fdaeb | daegcf eagdfb adgcb bagfed","    dafegb efdac bda fgedbac egfcdb bgfa fbaed bgdef debagc ba | ba bgcead bad ab","    cfged cbegdf cb ecgdb adefbcg fdcabg cdb cdagef efbc gedab | bc gdebcf gecdb bagdcf","    fbegcd gcedba eba acegf dbfa efbgd ebfadg ab fgceadb bgfea | fgbae gbedcf degfb agdebc","    ceadf fdacbe ef edgafb dfe bgaedc cadfg bcef cfebgad ecabd | cafde fabedc debac eacfd","    cd dgafe cbgfae decfg cfebg cfd gdbc febgdc fbegacd fceadb | dc beagfc aebcfg gcfeabd","    adcefbg caefgb bafd dface bdcfe cegbd fbe gdfcae dceabf fb | efb ebcgd gbdcaef fb","    fbdg gebac degbfc gd dfeagbc decfb gcd dafgce faecbd bdgec | gfdcae gfdb dg afgdce","    fb fgedca fbe gfcde gcbfde dbceaf bgeca eafbdcg dgfb fbegc | fbe cbdegf bfe gedfc","    acfbe fcbaged bgdcf cabgfd ag cagd fcbga bcfegd gfa eadbfg | afgcb gbdecf efdbcg fag","    fbegc dgca dfc fagbedc bfadcg dc ebdfca fadbg cgbfd gfdbae | dabefg egdbaf gaebdf dc","    cdbgf bdfaeg ca fgcab dgacfbe feac facgbe ebgcad bca gfbea | dafbeg cab cdagbe debfga","    gfbe ebcfag bcedaf be egfac fegdca gebca eab cbgad gfacdbe | dabcgef bcagfed gcfea bea","    bdefacg fbgedc efdagb beg geabf eg dafbce dage gfabc eabdf | gead gcafb ecfdab egfbdac","    gd gfecbd dacefb dgbf cdegf acfge agbfced becdf aedbgc gdc | gdcef cgbead gd gefacbd","    cgbfea cafed dbgefac gacb bc afbge fdabge efbac bcfgde cbe | gbac fcdbge agcb aecgdfb","    abgefc cfeba gcdba fgdcea gf fcbgaed eadfcb gbfe fga bacgf | efcba fabecd dgcbfae eagcfd","    gcdbfe fdec gcfdb bdfeg de efgba cbgdafe afgcbd cgdbea ged | cedgbf fced agfcdeb cgfbed","    dbacfge cafdb gbcfea aegc afbec ea bae bgcedf befgc edgafb | dfgeabc bfgcde faceb fbecag","    bfcgad bfegd fg dcgeb egaf bfedcag efabdc dfegba gfb abefd | gf abdgcf dcbfag fgb","    degab dgebac bfdeacg cadeb fbdeac bag debgf cgda abgecf ga | dceba fdbeg edbac dbacfge","    abdef gcbead bgce cgdab deg dgcbfa aedbg fbacdge eadcfg ge | cgfdba gcafde agfcedb fdacbge","    defabg cabdg gcfed cabe gbeacd fgadbc eb geb aedcfgb bdgce | eb dbegc dfecg dbecag","    egdabcf edcgb bcedgf cgb bgfe fcgabd bg acgfed cbaed dfegc | cgb gbc egcfad degbc","    fe agebdc gfbead fcdea abecgfd bafcde beacd cbef def cgfad | dabgfe fde adfcg bfgead","  ba fcabed bad dgacf dfgecab dcgba gdecb bdgcfa abgf caegfd | gdecb dgcfba gcdafe afbedgc","  bdgef cdbgfa acbed gedcfb efdgba cg abcgedf cgb cgdbe cfeg | edabc ebdca edbcg ecgf","  edfba bcfgead cbged defbcg ag gfadce gcaebd edabg cabg dga | fedagc bcag bdegc gaebfdc","  edab fed egcfb ecgbfad fegbd fagdcb degcaf afgebd gabdf ed | def gdefb fgedb edcagf","    fc ebfgda ebcfd cabed gcadbf baecfdg cgfe fdc egdfb gdecbf | defbg aecbfdg cadbegf adgfbe","  bgdfc fgc fdegb dgecfb fcabd egbafdc gc dgce gabefc gedbfa | fbdgce gc dbfegca cg","  bgedc ged gdfcea dcefb cdabg bgfe defbgc bdecaf dcgbfae ge | badefc gbfe cbegd fdagbce","  cdefga cgdafeb cbe becdg gdefc cfbg cfdbeg cb febdac edgba | edbgcf abecfd ebgfcd fgbc","  de aegcf agedcb afegbdc edbf fbdga facgdb gdaef dbaegf dea | cdabgf ead afgbcd edbf","  cfagde eagfbc cbegf cg fcaegdb ebgfd gcf dafebc efabc acbg | bacfe fgc gcf cg","  gadfce fbcaeg bdag eba bedcgfa dcfbe ab ebdgca ceagd ecabd | eabdc badec cdaeg dagb","  aedb ebcagf fdeac be bdgfc eafbdc abcfegd aefgcd feb ecdfb | fecbd adeb be bfe","  dg fgdecb dfecb fdcgeba gedcb gceab fdge cgd cgdbfa baedcf | ecbgd cagfdeb dcbfeg gd","  fgcead bfdcag gecdf dfeacb dce aged gcbfe dcfgeab ed fgcda | cfegd de gcafd gefdc","  dbec eafbdg dafegc eac gbacf decagb bcgae bdgea gbcefad ce | becag edgab cbadgfe edbc","  gefcab agbdc febd bcegd de ecbfg afgdce ebafgdc dfebgc egd | de gdecfba edbf cdegb","  abdcefg fbdega dcfag bd fegba badgec fgadb edbf fgaceb bdg | acdfg fagbce adcfg abgfd","  ag agdb bfeag fbgdace bcfeda dgafeb egafdc begfc dfeba eag | ecbfg agbd dabg bfeacd","  dagc dae fdaec edcafg fbadeg cgefa abcgef ad dfecb fdgcbae | eda eagcbf agfec debfc","  ead beafd dfagb faceb bfedca facegd aefcbg cbed ed gfcbaed | abgdf eda agbfce ead","  afdgb eabcgf dfgbe fecdgba dagceb gfa cafgdb af cdfa abdcg | abdfg dceagb decfagb fagcedb","  dbcfga cb acb gcbafde fcbg egbfda dceaf gbfda dcfba cdbgae | fcadgb cdefgba agbfd cgfb","  gceb fcgba cefdba caebfdg egfadb eagfcb cb beafg acb gfacd | abcedf bca gfcda bcge","  eabdc caebf ad dgabcef fcaedg cfegba deacbf dabf dca begdc | dbgce dbfa fgebadc acedb","  fbgce ec gcea dafcgeb fgbed fec adgbcf abcfeg fcbga cfbade | cbdfag ebgfc fgcbe agbfdc","  afcg afebcg becgf fgadeb bdgaefc fa afe acedb abecf gdfebc | fgecb eacdfgb ceabf cgfa","  fb gadeb aegdfb abgfe fab dfgb adfecbg bcaegd ebdafc cafeg | becdfga bf fbadec bgfd","  ab dgafe fdaeb adb agcedf ecbfd agfb caedgb bdfage egfabdc | fedbc dfecag afdcge becfd","  bgedf dgecfb fae gbaec fa cedafg dcabfge gedfab gafbe fbda | fcgdea geadcfb efbdg fa","  cgfaed fdebg fcgabd fcegb bd dfaeg gfbeda fgdceba edba dgb | db ebcgf fgcbe edgfa","  fgc egcafb dcgb fagdb bedafgc cdbgaf fedac cg gacfd dfbage | febgad cgfaeb fadbg gc","  fdaebcg deabgc bfdeg dfcaeg efcgd cdaf cfbeag fcg cf cgeda | bgefcad bgdfe dafc ebagfc","  egcfad dbecf cbgf fbe aecbd bcfdge dfbgea aebgdcf ecfdg fb | cgfb gecfd gefacdb fbcg","  cdb adcf gbcdae bdceaf dfaeb cdfeb febgc fgedba cd cgdafeb | eadfb fcgeb cbedga bdacge","  gd dacg gbdafc gfbdc bdegaf fbgca cbgdefa cbafge edfcb dfg | gbcafd dgac dgcafb cbfgae","  cbf dafeb fbceag bc egbdfc edbgafc fbced ecdfg cdgb gdafec | bcf dbfea ebagcf gadfcbe","  dcebgf eagfc aebfcd edcbg baefgdc fd cdefg dgaebc fde bfgd | ebagfcd acgefdb fagec gbdaec","  afgdc bagec bdfg fb ecgdbfa fab dfecga bcgadf dcafbe bgafc | afgced fb dcafg feabcd","  gef febag ebagc egcfbd fg cafg abfde abcfge afcgebd ebcgda | egf gaebf dfeba gadbce","  ag cdeafg gdbfce daceb acgf efgbda adg cegda gaedbfc efcgd | debac eadbc bgaecdf cgbaedf","  acefdbg debgac ecdgf ead facebg eafgb ad fedbga bfda eafgd | adbf eagfd eabcfgd ecagfb","  cd bafegc fgcaed fcaeg dgc fdcag agdbf fedc gbadce egdabcf | fedbcga bgafd fcde cfed","  decg egf ecgbfa degfa edbagfc dgcabf cdfag eg fcaedg faebd | gcadef efadg egf eg","  feacbg cgfadbe efb efbga dfagb eb aefgc bgec cgedaf dcebfa | eb be be fbe","  begc eacfdb dfacg agcfe ce eagbf eac eafdgb aefbcg dfgaecb | ce ce ce gbce","  dfeagb dgaeb dbagefc gbcfae degf decba cbgdfa bgfda eg gae | ge agfdcbe bdace fdeg","  cgdab cegdbf cfbad bg aegb caedfg eacgbfd dgb dgaec gdbaec | gbea eacgd gb cgaebd","  ag ecdgbaf edcba eagcd aeg gcfa dfcgae egcdf gfdeba fcgbde | fgeadb dacge fdagceb egafbd","  fdbgec bge fdacbe bfdga edfgb cbgaed gefc cefdb ge baegdfc | ebfgd gbe begdf eg","  cedafb gdab fgced cabdf fgbaecd dcbgf gb bgc gabfdc ebfgca | bcadgf cbfda cdgfab agdb","  fdcbg bgface ecdabf ad fgcbaed gcefad dac efgac cfagd daeg | dcfabge edga ad gcfea","  bce gaecdb aefc bedfgca acbfge aebgf fbceg defgba dgfcb ce | ceaf ce bfgce ce","  gefadc gefadb agd ebfcdag dbeg dfagb abfed gafcb dg adbcef | fbgad bged gd cfebda","  dcfge egdbf adcgf bafecgd ecd gdfeac bdecaf ec cfgdab cage | acge dacgf facdg cgea","  bagdec gecdfba gdabe deb eabc be acdgfe afgdb deagc gdecfb | aebgd cgdea adbge eabc","  gebca bedgaf ac gcbeaf bcagedf bafge egcfda bedgc cabf cea | ecbdg efcgab ebagc bgadfec","  edacbg gcfad efdabg bc cgfdbae bdc cfedab efbc beadf fadbc | dbc fcbe fbedacg cabfd","  adfgce acgfb bgfadc dgbca fc gfc fgacdbe fagbe bcdf ebagcd | cbagdfe gfc fgbac cgbaed","  gdbfec bea acdb ba bcged edabgc gcfaebd afebcg gedba gdeaf | ba fgcebd efgda gadfecb","  gd gcda bgd gafbcde bdcfag bcgdf cedfb bgefda eacfbg cfbga | fgcab fdgcba bgacf bfedga","  fgcadeb abdgc gce bagec fcbea ge aebcgd gead cedfgb bagdfc | gbdace baefc fdcagb cdgab","  fbca aedfcg eagcbd fadbec bdgef fec cf bgcfade dbcef cbade | fgedb bgefadc fc cf","  dbgfa feca gfbced cf abfgce fcb cafbg eabgc acdbge edafcbg | abgdf dbcgef geabfc fcb","  dbaf fag bdfgea ecbgfa dgbfe fa edacg agcfdbe cefdbg fdgea | fgbaed cdegbf dacfbge gdcfbe","  abc acefbd gacedbf gbcde dgabf ca fgcedb cega dgbca cgeadb | ca bcged abgfd bca","  ceabgd cgfeb ea aedf gcbdfa cadbf acdfeb abfec eac dgcefba | cgdbafe cea adfe aec","  dbcfgae agebc ebgd cebdfa ebacdg deagc be fdecga gbacf bce | deagcb agecdf caefdg cegad","  dcaeg gea gdaf befgac cefgd facbdeg dcgbfe ga dagfec adecb | gadf gaedc age cabedgf","  bead dagfbe gda bagfec ad egfba ecgfad fdcgb ecfabdg dfgab | agefdc dga bgfea dafcgbe","  abcgd gecbfda cegd badfge bcaed fcaeb ed adcgfb gcabed edb | acefb acbgde deb ed","  cgefdab dgbcfe gbe beca be fdecga bgcafe bfgda abgfe fgace | bfgda eb gbe bdgaf","  age cefbadg bcfea cdagbe begca ag gedfca cbfdeg ebcgd dbga | efbac ega fecab dgba","  cgedf fc defbgc cdf fceb dgebf bafgdce eadbgf ecgad gcfadb | cf deabgfc efgdbc fgbcda","  bdgeacf bagdef gb becg bcfage cafbg bga fdbac caefdg eafcg | bcedafg cbeg bg gb","  dfceb gcdab ecbad gfbdea eab gcbeda bgcdeaf ea abfgdc cgea | baedc egca dgacbe cedfb","  gdaeb dgfbc cefg fe cfbgaed gdfeb fadcbg defgcb abcdfe dfe | gdcfba eagdb fe cfeg","  abfcedg afgcde acgfd ae bdecga efbcd dea fdgabc feacd gfae | afdcge gaef aefgcd geacfd","  beadfg cfdgb fdb agfcbde db abcgfd ecdgf cbagf bafgce bdca | efgcd dcefg cdgfe bd","  fagbe fedbcag afbgcd agdfbe age adgcbe ea fead egbcf abgdf | gae cbegf afgdb ea","  badfcg fagdc gaf fedcgba dagb acefd dbgcf cgfdbe ag gbecfa | ag bagd bdfgca fbadgc","  cfebg gc fcbea dcbafe gbc gcedafb cegdba cagf fbgde baecgf | cg agcf befgc bgc","  fegb ecdaf ecb gacefdb eb gdacbf cdegba abgfc afceb acegfb | ebafgcd afdgceb ebc be","  adgb cgbae bg abecdg gcefa ecbad dbgfec gbe cbfdega baecfd | badg badg dbag aegbc","  ab bfa gbedcaf adbgfe bfecg acefb fgdcae bcad fceda ebafdc | gebdaf edcfag ab ba","  ceadbg cd gfdeca ebgfd gdfec dce fegbca ecfag fadc gdeabfc | gfaec gface fdac ecadgf","  fbagd egdafb bdg adcefg gbfac daegbcf eadgf bd bfde dgceab | dbfe bd agfde agdef","  ebg bgdfe acebdg dcfage abfdge agefd dgbcf eb bdfegac afbe | cdfbg aefb dcagef edfga","  bfcd efc bfdcge gbdec bagfe agdceb cf dacgbef bfgec eadgfc | cfe dcbf cgebdf dbecag","  cda cefdbg fgeca fdgbeca acgdf bdcgf da deafcb fbgcad bdga | afgbcd cfega gbfcda gdebcf","  dcaf cagfb gdacb bgdfca fgaecb ebdag cd fdgcbae dbc bcfegd | cfdegb dc abcgd cbfedag","  gabedc dgfab ecdfbg adegf afbc dbagc bacedgf bf fdb fdcbag | cfedgb dgcfab gbfcead dcbga","  bcagde efd bgfcd fdecb aefb fe dcfbae gacfebd caebd afecgd | ebcdf afeb ef befa","  abfc gcdaef agecdfb cb gacedb cgb gcbdf bdgfac gedfb gcdaf | gfceda bc bdfgaec bdgef","  geabc gcebad cgbeaf begdcf gfabe fe bfgda gfe afce ebcfagd | febgac fcea fbcged baecgd","  aedfb gbfe acbgde fe fcedgba aef fcadge bacdf fegdab ebdga | fcgdeba afdeb ef fae","  gbcfd adfgcb gba dgcbafe agbfde ag cfag fecdbg gbdac edcab | fgcbd bgcad agb bdefga","  agfde efgcba gaefc bdcfeg cfe bafc bgcedaf egbac agcedb cf | gebacf ecgafb gfceab cefag","  febdc begcd ecgbdf dbacfe bdfagc adgec gbd gb afbdceg gefb | dbg gacbfd egcdfb gfebcd","  decfbg dagfb afcgbd gdcfb cbaedf caefgdb da gdca gafeb abd | adbgcef dgafb abd cfbgeda","  begcfd bgeaf dcfaeb cadfeg gcdb cadbgfe cg ecfbd cfbge cgf | cg gfc gdcb gcf","  ecagdf dfcebg fgbea bg cbeadgf egb ebcafg edfab gbca fegac | bafeg aebfd gcab fbecdag","  dgacf abfecg fcgadeb cgd gdbace cgbfa egfda dbfc cdbfga dc | cfbd gabdce dbgcaf egfda","  afedcgb afge bfead gf gdfbe fgabed dgcbe dfceab dgf fabgdc | feag cdabef bgdacf gfea","  adebgc de bfgacde beafdg egabc egcabf deg cead cbdeg dcgfb | ed eabdgf eadc dbafge","  fcdbgae gbacf ebgdfc bcadeg bdgea fagdbe gce aecgb acde ce | cge ecg cabeg efdcgba","  aefgcb fedagcb cefbdg ef dcbeg bacdge fegd edbfc fabdc cef | ecbgfd gdef bacdeg bcdeg","  geadc gecfad gfcadb eg agdcf bgafce gdef acdbe gea dabegcf | ecfbag gebcaf baecd dfeg","  cdeba bd aegcd bfadec ecfab bafd agbfce bfdcge bdc cgebdfa | abcfe agdce db adecg","  fbadg faedgc bgdac bcegd bcadge agc gdefcb cbae gdfbace ca | defgcb ceagbd bdgca afedcg","  acgfd cbdage agcbd dfa edcfg fa dbcfeag fabc dbaefg gdbcfa | af gedabfc af ebgdfac","  dafgbc dfegc dc cgfae cgeafd cdf cagefb ceda bfged fdegcba | aecd cade deac fbged","  cbegda dgfeb gefcb aefbgc fdgceb agdfe dgb bd fdbc bdgfaec | dcgbeaf db gfbed efdag","  aefbd cdbeagf gd fdgc fgcdab afbcg bdg egfabc bfgad aebgcd | gdfc gadcfeb cfgd fgacb","  eb gcbaed aeb fgdecba begd eagdc cadfb eabdc cfgade gacfeb | egdca dcbea bdfcgea be")
//  val input = Seq("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |    fdgacbe cefdb cefbgd gcbe","edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |    fcgedb cgb dgebacf gc","    fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |    cg cg fdcagb cbg","    fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |    efabcd cedba gadfec cb","    aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |    gecf egdcabf bgf bfgea","    fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |    gebdcfa ecba ca fadegcb","    dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |    cefg dcbef fcge gbcadfe","    bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |    ed bcgafe cdgba cbgef","    egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |    gbdfcae bgc cg cgb","    gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |    fgae cfgab fg bagce")

  def main(args: Array[String]): Unit = {
    val inputCleaned = input map { in =>
      val indexOfPipe = in.indexOf('|')
      in.substring(indexOfPipe + 1).replaceAll("^\\s+", "").split(" ").toList
    }

    val mappedToCounts = inputCleaned.map(_.count(input => (input.length == 2 || input.length == 3 || input.length == 4 || input.length == 7)))
    println(mappedToCounts.sum)
  }
}
