using System;
using System.Linq;
using System.Collections;
using System.Collections.Generic;

namespace adoc07
{
  public class Node
  {
    public string Name { get; set; }
    public int Weight { get; set; }
    public int? TotalWeight { get; set; }

    public Node Parent { get; set; } = null;
    public HashSet<Node> Childs { get; set; } = new HashSet<Node>();

    public Node() {}

    public Node(string line)
    {
      string[] split = line.Split(" ");
      Name = split[0];
      Weight = int.Parse(split[1].Trim('(').Trim(')'));
    }

    public void CalculateTotalWeight()
    {
      if (TotalWeight.HasValue)
        return;

      TotalWeight = Weight;
      foreach (Node c in Childs)
      {
        c.CalculateTotalWeight();
        TotalWeight += c.TotalWeight;
      }
    }

    public override string ToString()
    {
      return $"{Name} ({Weight} -> {TotalWeight ?? -1}) Parent: {Parent?.Name ?? "null"}, Childs: [{string.Join(", ", Childs.Select(c => $"{c.Name} ({c.Weight}, {c.TotalWeight})"))}]";
    }

    public static string[] GetChildsNames(string line)
    {
      if (false == line.Contains(" -> "))
        return new string[0];

      return line.Split(" -> ")[1].Split(", ");
    }
  }

  class Program
  {
    static void Main(string[] args)
    {
      string test_input = @"
pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)
";

      string input = @"
apcztdj (61)
ulovosc (61) -> buzjgp, iimyluk
awpvs (88)
ykbjhi (14)
gxvketg (49)
gpmvdo (78) -> jjixrr, zacrh, smylfq, fdvtn
nwpmqu (6025) -> aytrde, grokih, pjqaa, nzzved
keiakhg (50)
vkrlz (22)
myzkj (40)
zkzpbbp (46)
eaumqtc (288) -> tvmvvxn, ckcpovl
htlovrb (1662) -> atxtq, ewsqqum, mjwviex
aoxky (53873) -> orzwmml, tcvtul, ygszhn
uhfiqwc (81)
gziopi (189) -> hboommj, glbipl, evojkum
rjksjq (66)
rwyzrl (1434) -> geiethw, nclvb, riphudi
gmusjd (98)
vjmwzo (76)
xymsu (153) -> kbgmlab, yjjewyt
mgiumh (64)
cnfcuww (40)
pgeoixp (211) -> qgrtg, svtcvi, rutmxs
zwwpva (89)
otgnir (87)
vxftbc (60)
xxpfmp (32) -> cnqemd, jmujchg, yfolyk, zalahl
tcrcyis (1098) -> tyealpm, dirvqby
ffjshhj (65)
powwpec (23)
gudpriq (171) -> zqxyn, bwxgrhf, dclnnlg
nzrbmfg (7)
fgptt (210)
hpcafa (91)
urepv (2628) -> qzcqhsk, iqeynjk, fakqhi
qzcqhsk (516) -> ujjksl, qccwy, cddbi, cvpoqqk, nnhlcn
riphudi (35)
nwtvzap (101) -> iwumx, bpazmfd
xhxctp (84)
dkpmgrq (94)
rvftpif (5)
iqeap (27)
aplauwh (693) -> rjkzf, nfvns, snhww
iayrwd (81) -> beidb, lpjxpx, mtvxp, alcfp
nsuysii (86) -> aktzpym, gdckok
zbxbe (59) -> ahwtsi, uqjcaa
yhiwu (37)
qpcmzl (28)
jmujchg (48)
germn (218) -> mbdwm, kjxac
mxsvay (328) -> stuom, twgcf, mexagfy
sneog (20)
rkczq (180) -> mdfae, rrsfv, yycys
nnbfqh (819) -> guttanj, htmjx, rwlmsx
nxzafkd (60)
tptdvct (64)
mokvas (49) -> ygfcl, xxitunl
wcztr (133) -> chxurcq, hemhd, siwvncp
satie (61)
vcjpuw (43)
hzcmmh (90) -> ivbwdn, eknlt, nzfjwtq, hwcbsyu, gejaump, xymsu, ebtoke
jimigk (68)
vzfebn (196) -> elqlx, lkffdz
lcwpm (1946) -> jwjuylx, pjmwkz, geajywv, zyqfc
iimyluk (65)
zcgaxsi (49)
elhmwsx (92)
bxsxr (189) -> msowvgh, zjxlr, yylzxb
qgrtg (14)
gnupj (74)
aktzpym (72)
gvuwq (88)
iojjekn (160) -> gnhcs, homrc
gzdtco (88)
ospfv (5)
epovby (14)
jflns (39)
hmqam (161) -> tkiheq, flrtvno
koxyrpn (94)
witxvcj (44)
pseltr (70) -> bzqmgl, vlmzfaf
yrotnfh (68)
fyqlqkg (19) -> gicwtf, dlxubg, ysrwm, qjnela, djupte, rayzwrp, mgexekz
eipyu (84)
hacgqe (54)
nssvbj (5)
maugts (7) -> owizuw, dopwkoo, nnbfqh, rwyzrl, kkfio, gzpcl
uiclmg (14)
psyem (173) -> tbxxc, bodpwem, lfhmte, pyobp, znnjzzw, pcnbkxm, tolty
rskopr (68)
mhihjb (68)
bfkpjdl (150) -> mpbui, qcqpavg, lwmblwr
ezotk (50)
dyadgtm (7) -> zcvzrja, rifmmf
npipd (20)
ozqsvif (231) -> stketdd, jpedzjk, vrpoaq, hbollnl
lxshx (67) -> peocao, vqsxe, tstuc, pqwzgz, ycuajn
kdersld (98) -> nrarwj, ajyyjfx
ontcbns (111) -> jptxyhp, zwwpva, ndoecxy
vyzksry (99)
sxygbpo (27)
owayyaw (5346) -> ykvqh, ruejmc, oqrvpt, qmvebxb, macxvi
paqzzv (60) -> nujgng, krnjk
qhutc (168) -> ykbjhi, uiclmg
ofndvr (46)
vfgomle (56)
znkru (32)
eymsjfd (31)
dvetl (124) -> kdccrl, rtsji
pvdbhh (96)
ggilocf (55)
wjjmh (36)
zqxyn (14)
qweyxa (32)
tiirxay (9)
avsse (292) -> wchlqxi, griwu
fmtox (6) -> mrflo, ikpyx, tsfrlzw
qczgclr (90)
tajeklj (151) -> kmrbuvq, hiaoc
kxtzbj (49)
dopwkoo (990) -> hnrwa, nwtvzap, ruitlxx
gyqrut (1431) -> pykcza, kxxhn, tipdlf
eknlt (177) -> erphv, dhahfgv
pnpge (49)
teeunu (4569) -> hirvz, mkpgx, dnflzcz
kkfio (207) -> iayrwd, abofflv, cpsmrch, ndtcui
wkdxdem (78)
dnslk (50)
civkkh (38)
bzqmgl (91)
gxtnep (43)
hemhd (54)
nzzved (110) -> vexun, sekcqad
tnejqkf (47)
jenxqco (14)
tlsdvhf (79)
kcltvch (65)
tqyheu (18)
csoko (1055) -> hnwpfl, rstsr, icnwzox
jmgsk (54)
rayzwrp (244) -> pydrzyk, hbomk
dmqhi (31)
wsctytx (26)
kjxac (65)
vrryj (177) -> nssvbj, gzlfqg
vmvdhds (33)
iuuve (90)
wbugvc (938) -> hpmynx, cznyy, yjqbsxx, yzcddkw
hnrwa (78) -> fbzro, iempnd, ojrsv
inojzm (100) -> zxlfwi, homdmyc
cvvex (75)
thumfdk (365) -> hcvvfm, oyuklmp, vtnddpz, egzpkj
wkqnq (8)
ulcnxii (59)
kdccrl (96)
hpmynx (112) -> kdmprsg, jvdawi
iempnd (35)
rqzkom (305) -> bvdqhfs, ubiuve, vqlqq
stuom (189) -> tffhcft, lzwplw
bjqzgqj (75)
ygfcl (69)
qakcvf (117) -> udvdho, gxtnep
vxlonb (88)
tjlsmrz (18) -> pmznzoa, dtifm
golth (22)
xdmqa (18)
xsilcvz (92)
oudxhh (81)
homdmyc (25)
zyrztc (15)
mmngri (60)
vqsxe (220) -> prxex, jhggy, zkmdfh
lyeoeff (74)
mcnek (54)
ckcpovl (55)
vqmeer (359) -> ijbth, phoic
zhgql (35)
zalahl (48)
toccxvy (58) -> ryecm, embmhv, wutkvkd, qlxioka
sgvbpct (13)
ubaaa (2460) -> ivkvhf, tjzut
yxmycw (1258) -> mokvas, lzcant, vrryj
mxtqhi (286) -> jwpbg, kizufjn
ofqav (13)
gqpqzrh (49)
jgjbcux (23)
vojblx (143) -> ytcgcq, bltsiq
qrbiq (7)
qnofu (64)
tcvtul (372) -> vxlonb, xcvdoi
spcpfe (255) -> tseydw, cfdumg
ohyhw (153) -> xttop, hnocmo
dqpcn (310) -> witxvcj, qshxib
btxts (816) -> klgsp, thumfdk, kbothlh
ezptzx (86)
dtifm (68)
gicwtf (254) -> beocjuy, kgfums
wvawj (59)
cddbi (255) -> ekrym, cysyc
rimlwoh (64)
wutkvkd (25)
hqlni (74)
vwzein (18)
nxrlaw (74)
nhrla (34394) -> idfyy, nwpmqu, owayyaw
whotly (17)
prxex (5)
mkpgx (468) -> thunt, lujir, xjoid
qmklslb (19)
lokvk (67)
xecai (12)
mucnt (77)
daaldt (68)
rtatr (142) -> xkjzrg, btufku
hpouwfs (22)
nclvb (35)
dnirbtx (49)
pykcza (52) -> gkbpz, vzbywuf
djplc (18)
hflkio (88) -> elhmwsx, iovcix, twffuag, sygcnhv
nkdtsf (33)
cvzvo (139) -> gfrwxw, gjphrw
ilgee (25)
zyajdwf (273) -> eymsjfd, dmqhi
rwlmsx (212) -> djsbpg, qrbiq, fnsceun, tdymtd
uhwtkvo (66)
hzzlebr (20)
mundgs (93)
ytjaofr (149) -> hmfvzi, bzplleg
epbevf (26)
ptydckb (47)
hjuzxv (74)
eljbay (138) -> rntsalv, llpxo
kvnom (87)
qvdrme (205) -> ffjshhj, kcltvch
zbwrrsc (68)
jmifnxw (42)
pfojn (42)
wgdrxl (85) -> notot, jogicnh
wxzenj (224) -> swojvy, sidmmmy
iekjt (91)
quhxdb (28)
pydrzyk (50)
hboommj (21)
vbiyre (65) -> kdrmsgh, lngffo, dqyifwc
sqehayt (1007) -> vyzpzww, hmqam, qakcvf, dyadgtm
keuqd (30) -> zrypv, zogeax, yyjfus
tyjzoq (24) -> utvyxj, pwuhv, ombqq, inattlb
xbfmp (37)
aytrde (56) -> hfvkmf, vyzksry
oufikpd (23)
qpfybmv (189) -> znkru, cjlvlm
hgsdq (288) -> bvxiqzm, jmifnxw, pfojn, bltkr
nopjkom (1447) -> ayxuibs, krcntq, hbbhm, rehezo
wtnmryv (92)
mexagfy (235) -> leoti, uqwbtx
bmsje (87)
jkoyzxm (32)
egzpkj (12)
hirvz (774) -> cmhoj, eljbay, inojzm
mxdluii (90)
unuwu (41)
ekuohb (24)
xzqgkqn (87) -> mbqdugj, lujurf
rzehh (98)
dqqyiwm (61)
oqialc (192) -> uhptzfg, quhxdb
jwjuylx (63)
ebwifj (14)
grkauy (39)
hvizfxx (83)
pyxypca (64)
gdckok (72)
yccfitx (69)
eisyowo (83) -> jjzbzsb, vbiyre, oqialc, uzxjxjx, tgjdv, ydhoryg, kdersld
mjwviex (79) -> gnupj, lazcc
grokih (202) -> wsctytx, qvksmrd
vpfoaw (36)
wtxndn (94)
lazcc (74)
pxzzw (55) -> dhccilz, rvzppdg, qcwhzgo, htlovrb
jjzbzsb (88) -> cnfcuww, gtitbbk, myzkj, rfsrdn
hrjil (71) -> ftmftoa, vdekve, lizqvr, rtkydv
ddblq (28)
eedxiav (257) -> jdeqa, tbmflh, zpjxsi, kgamsy
ywxionz (239) -> gdkll, xecai
ateol (34)
hlcahpm (90)
rntsalv (6)
zcvzrja (98)
kbbyj (87)
btufku (80)
hygdtn (7)
yxbwlf (98)
ivkvhf (9)
mevvf (60)
jdeqa (28)
jhdmh (956) -> qkilfj, xzzar, gkqrml
qlpft (57)
delev (30)
ndoecxy (89)
lfhmte (334) -> qmklslb, ekcuavb
fakqhi (1370) -> kqhggwy, zvpaj, zbxbe
mzvjxgp (41)
xuqhpz (49)
rkfknhv (28)
fbzro (35)
vrmevdy (35)
mysslj (49)
aobgmc (341) -> tajeklj, rkczq, ohyhw, zceovs, gudpriq
cocplr (67) -> opczxnj, ulcnxii, hjqvntn, wvawj
fnsceun (7)
yjqbsxx (122) -> ybtkkgv, fgyjop
atxtq (181) -> oufikpd, czacja
qvksmrd (26)
gqmjvky (137) -> xvhnaf, cwspi
bpusqb (84)
ftmftoa (101) -> nlixy, mckkn, rpbawy
aqshqpo (56)
xsnmq (46)
bltsiq (80)
towwl (54)
anhldnx (2024) -> kvnom, jbprgm
ksioqg (8)
nnunune (227) -> jmgsk, mefyd
jvdawi (56)
icnwzox (236)
qiikh (127) -> zmveluw, ycncnr
pjmwkz (63)
pvqid (50)
fivcv (64) -> pyydxvi, tzoefdo
zacrh (44)
smylfq (44)
qcwhzgo (2040) -> aetlth, kqbrwt, ualcokx
uwauv (196) -> daojy, sfyqi
mjfbr (910) -> ruuovw, kghzy, psyem
efcff (79)
kzflj (19)
shethb (91) -> qsgpsfr, fmtox, pseltr, gziopi, ohqlwmr
yylzxb (6)
ybtkkgv (51)
olxmnkc (79) -> wtnmryv, encean
cysyc (8)
tkiheq (21)
xkjzrg (80)
vtnddpz (12)
kgfums (45)
rrlwo (41)
rujggz (68)
beidb (63)
xcvdoi (88)
xghng (17)
vmoqov (43)
pekzuyh (29)
fheklq (66) -> crdlymr, vgsuq, lqspkzw, wbugvc
wodhzc (18)
rlzbv (92)
ycuajn (185) -> iizggi, ilgee
zjrpxp (73)
sogepf (66)
gbecv (78) -> hlcahpm, mxdluii, qczgclr
xzvbcms (92)
wcklpgi (14)
wuhossn (96)
dlrfwee (35)
rfsrdn (40)
akkrsv (108) -> ontcbns, mrydp, flqjd
uqjcaa (54)
ixcxv (49) -> ruvrw, rwqjiz, towwl
kdrmsgh (61)
gtxlyzz (32)
udvdho (43)
lelvcai (28)
ivbwdn (121) -> hpocx, bwpqytq
bvdqhfs (6)
wrxzti (62)
yoofo (79)
oqrvpt (323) -> pyjzb, wkqnq
bfxdtkg (93)
yrmmga (36)
tvmvvxn (55)
hlhpm (187) -> xqnan, ykubvpo, bxsxr
ovalfky (66)
wpner (54)
gljri (96)
jpedzjk (153) -> byphua, nxzafkd, ccpek
ydhoryg (65) -> apcztdj, wfuxy, satie
zxsgnx (105) -> pnumlg, gouyeqw
zyqfc (63)
evxjdeo (57)
uspfk (54)
rwqjiz (54)
griwu (72)
mcgaqx (68)
iqzkp (103) -> rkfknhv, lelvcai
edmsh (77)
uavzab (17)
seleq (87)
wsozlg (7)
dgvvi (93)
svbmkbx (196) -> golth, casbq
szoomk (26)
bcima (78)
cyasn (4978) -> hlhpm, fmmhxab, chirgx
hfvkmf (99)
fjonxe (73) -> xbfmp, yhiwu, kxchm
wnoer (81)
zaouq (96)
gkqrml (109) -> umitnao, bkcgx
ppythr (73)
qozshd (14)
namzem (66)
cjlvlm (32)
ohcfrxm (85) -> xhxctp, wxpmbu
iqwspxd (897) -> gqmjvky, wyzmc, mbzme
chirgx (46) -> gpmvdo, fivcv, aelsfhi
pyobp (78) -> gmusjd, pqfnj, xbhcwe
kspoky (9)
gtkqi (56) -> cquptt, cdeji
twgcf (49) -> lokvk, cpxwa, yovtgg, nzzwed
plsic (60)
pmyqol (66) -> ayixid, efcff
hnmba (22)
ktbtfdb (56)
yyjfus (85)
rvzppdg (45) -> airnuh, mfvqvf, begmhqg, cykvv, wipzuag, fmstmh
vebyb (30)
yasoqri (41)
gjphrw (85)
caqjohx (25)
oqiynm (9)
lydynp (73)
dclnnlg (14)
nocugh (81)
tqzbg (15)
elqlx (81)
mckkn (67)
lujir (152) -> pvqid, keiakhg
mywhdsm (47)
oafxzu (27)
fgumszu (98)
hnwpfl (89) -> gqpqzrh, nlrwlj, gxvketg
xrvno (49)
kjdjj (225) -> keuqd, yompk, grxqt, ocxbgp
vyzpzww (163) -> npipd, hzzlebr
oqzcbx (81) -> wuhossn, ebhag, zaouq
twffuag (92)
nujgng (82)
xbhcwe (98)
bvugv (199) -> xdmqa, qqszul, wodhzc
vgsuq (1510) -> oanoax, swcxes, qmelelp
cwflw (27)
yraktfn (27236) -> pxzzw, igwew, jjzlrku
iqeynjk (17) -> dccvh, cvzvo, kcjujcr, phqplx, spcpfe, dygrnnf
vgowu (30) -> mgiumh, pyxypca
jjzlrku (635) -> fuwbaq, lcwpm, oahpf, anhldnx
zkmdfh (5)
dvskig (22)
zaitni (83) -> iqagnh, ixckwlm, kfvvzd
ikthp (51)
jvpob (76)
cpxwa (67)
sphwh (88)
mlbjc (81) -> fylao, ggilocf
rwmci (63) -> qnofu, rimlwoh
fluuo (51)
exvmk (78)
jgdqi (3125) -> mxsvay, hrjil, fhtktd, qqvgeq
aplal (124) -> ezotk, mithbv
gouyeqw (27)
iqagnh (47)
xfjafp (78)
gvaie (153) -> cpwdhw, bjqzgqj
ijbth (5)
zduooao (93)
tjlrb (78)
emeyn (35)
zblcbd (34)
xxtki (88)
nxgjqaf (1807) -> ubaaa, hjxhx, uyofsmv
xqnan (87) -> ckbuzb, rutqvq
xyquxix (70)
pwuhv (72)
sygcnhv (92)
qjlfwtm (19)
vkkgwbr (8)
casbq (22)
zeisn (95)
rplqlc (63)
ewsqqum (99) -> mojwgp, zuxgf
mojwgp (64)
dexul (230)
kdmprsg (56)
amifh (43)
notot (63)
byphua (60)
vfjayw (88)
gfrwxw (85)
vrpoaq (41) -> mwzpa, pfhjtw, rthxl, ppythr
wngqx (26)
obgmlzf (67)
daojy (17)
vxnwq (61) -> kjhrii, ulovosc, ytjaofr, vrgblbn, rwmci, mlbjc, vblppqb
yzcddkw (40) -> hvnzfbd, hqnonqc
iiswvjq (62)
yeerc (68)
ycncnr (42)
cmrcpf (294) -> onbptkd, qxsxzth
zbtlfr (35)
paqggr (84) -> ettsdu, iekjt
prfbji (14)
bodpwem (244) -> jkoyzxm, gtxlyzz, qweyxa, bbqzlo
ccpek (60)
buzjgp (65)
ahwtsi (54)
ckbuzb (60)
mrflo (82)
alcfp (63)
engay (35)
fxeka (18)
ohqlwmr (112) -> tdcxeia, maaid, fiygpep, zhgql
iizggi (25)
qcqpavg (364)
siwvncp (54)
lpjxpx (63)
ryecm (25)
xrsaryj (115) -> qcrxve, iuuve
dterxr (244) -> hfxmz, mpble
kcjujcr (207) -> eyavk, ikthp
sjommm (98)
lneqx (456)
dccvh (201) -> uipvt, iqeap, gbuilbn, oafxzu
duzorty (20)
mdfae (11)
pmavka (48)
ekrym (8)
logpgb (2713) -> wamxgvn, ozqsvif, olnpj
pxrxqs (78)
thunt (94) -> tlsdvhf, yoofo
ogkucp (84)
xroaal (43)
czacja (23)
xbaytg (5)
embmhv (25)
ahsef (41)
cfbjnxb (799) -> ujpka, fjonxe, slacopj
itusrhj (277) -> gscng, pekzuyh
vhruv (34)
fdvtn (44)
flrtvno (21)
fiygpep (35)
fhtktd (1103) -> xxtki, pgnulg
dirvqby (72)
codroxx (49)
fswirnb (98)
rvzqqr (290) -> wafsr, pwhrvu
peocao (235)
drleu (70)
ygszhn (86) -> lnuqdd, tjlsmrz, yomawy
ixbxszv (43)
nzfjwtq (67) -> douggpq, ezptzx
wfmxgw (98)
gzuqs (80)
ujpka (84) -> bhpvfr, dnslk
ebjpdvd (19)
mykkoc (91)
pyydxvi (95)
prybce (62)
smpmp (9)
nfvns (183)
rvqubk (81)
ezvdo (74)
gdkll (12)
tzoefdo (95)
vzdmsv (224)
vcusbhv (196) -> ospfv, wfvtfos, yjdsnqu
beocjuy (45)
zjxlr (6)
dlxubg (344)
kfvvzd (47)
zrypv (85)
aonpk (25)
icsrp (14)
pjthjam (286) -> ofqav, sgvbpct
fgyjop (51)
rstsr (68) -> xnleu, gfvjqj
xgrjpl (92)
rlzobr (89) -> ctifv, ffevqd, kbbyj, otgnir
vbvctl (87)
lqspkzw (90) -> rvzqqr, avsse, rocllr, cpiscj
mbqtji (392) -> paqjozd, wsbpne, tncexxs
vuumtry (63)
zpjxsi (28)
cykvv (247) -> yeerc, qfyrdo
chgpk (281) -> sxygbpo, hkyqakf
wchlqxi (72)
lzwplw (64)
daonbjx (790) -> ohcfrxm, yinhnv, bvugv, pgeoixp, qpfybmv
ffevqd (87)
gnhcs (78)
yjdsnqu (5)
vjugmg (73) -> qekol, jjdcuql, atdrk, rqzkom
swcxes (60) -> bzwhd, ekuohb
hmfvzi (21)
wwcrqwq (54)
iezlg (71) -> maugts, nxgjqaf, liibv, qrvmp, nwyqp, mjfbr
erphv (31)
vpxbz (48) -> qaalu, gtutr
sbsustu (91)
uvwnhot (14)
mbdwm (65)
cwspi (15)
fnvxe (104) -> bwmckc, vxftbc
qmxjyg (56)
hkyqakf (27)
uyofsmv (1214) -> ukdbm, iojjekn, dterxr, dvetl
glbipl (21)
yjjrrjj (93)
xnleu (84)
dyeffd (64)
mpbui (52) -> bcima, yowna, hvpzbd, smzzu
hvnzfbd (92)
vguwn (40)
tdcxeia (35)
dhccilz (471) -> wxzenj, tyjzoq, azrdj, mxtqhi, pjthjam, zzjicx
mfvqvf (355) -> rcdic, ebwifj
qsjue (81)
kgnbx (56)
efsdxm (74)
niojmd (95)
uzxjxjx (68) -> plsic, mmngri, mevvf
drbjnk (99)
hqnonqc (92)
opczxnj (59)
iwumx (41)
jwpbg (13)
aetlth (7) -> tnejqkf, mywhdsm
ujjksl (169) -> ixhsq, fluuo
ygqjcm (74) -> uqpxyi, uxmfcyr, oudxhh, iktols
oyjjdj (678) -> svbmkbx, lxpkrf, vayyag
pnumlg (27)
hbyxgv (94)
ivhzxr (14)
geajywv (63)
kjhrii (135) -> ddblq, ehkhs
rehezo (56) -> vuumtry, rplqlc, zcakvl
gunfz (80)
airnuh (369) -> hygdtn, nzrbmfg
ruejmc (283) -> icsrp, ivhzxr, uvwnhot, jakny
ikpyx (82)
zxlfwi (25)
ocxbgp (285)
qfsbvqe (39)
htmjx (48) -> tptdvct, smhzr, dyeffd
ukdbm (44) -> rskopr, rujggz, mhihjb, jimigk
lqtuij (66)
jogicnh (63)
vqlqq (6)
bkcgx (80)
fxsmo (85)
icoiqa (256) -> ljscsoa, sxqnium
ehhrom (334) -> aqshqpo, kgnbx
tgrfyrx (6071) -> urepv, ippmv, yvirt, mofdlro, teeunu, jgdqi
kttzsv (49)
hpmag (88)
jjixrr (44)
rjkzf (147) -> fxeka, pealz
rtsji (96)
wfuxy (61)
dzjef (22)
gfvjqj (84)
lslsl (28)
lizqvr (101) -> zaeyze, ppbgib, obgmlzf
rcdic (14)
ykiwb (105) -> drbjnk, vwokfc
dygrnnf (153) -> pxrxqs, xfjafp
csbdh (28)
tffhcft (64)
eilgcny (62) -> ldfybwl, gzuqs, gunfz
hfxmz (36)
spqwtg (1089) -> foaubeo, ehhrom, fcuhw
gtitbbk (40)
wsbpne (15)
gpcxuo (81)
cpsmrch (333)
fhmkuo (266) -> zeisn, niojmd
ckifyf (23)
ppbgib (67)
titychs (25909) -> fqmyi, logpgb, cyasn, fheklq
hlyfdr (26)
zmveluw (42)
kqhggwy (167)
cakob (96)
wipzuag (187) -> rerviwo, sjommm
hvpzbd (78)
vexun (72)
fuwbaq (13) -> ytzrx, pwyiwkf, rlzobr, mbqtji, jpnfrsb
fqmyi (3349) -> wxirji, cfbjnxb, shethb
xbgmzxd (144) -> wsozlg, lzqtms
paqjozd (15)
jakny (14)
susknu (81)
olnpj (87) -> rhsdbfh, vqmeer, oqzcbx, eedxiav
hjrzrli (62)
tjzut (9)
kizufjn (13)
iduxx (9)
wfvtfos (5)
dhahfgv (31)
oahpf (630) -> pmyqol, zaitni, xxpfmp, aplal, vzdmsv, paqzzv, fnvxe
utvyxj (72)
andixsk (39)
nglumw (49)
cdeji (59)
ynxfzsu (73) -> amifh, xroaal
hpljf (37)
oanoax (52) -> csbdh, tfdjhaa
jbxcafx (97) -> qlpft, evxjdeo
xaplszi (689) -> qgbtnil, icoiqa, vzfebn
sbtyi (91)
wnhmooe (37)
qekol (35) -> gljri, cakob, pvdbhh
lkffdz (81)
hjxhx (42) -> germn, ycrxr, cmrcpf, tbrxke, gbecv, wnakg, epdqjdp
ualcokx (35) -> nkdtsf, vmvdhds
hwofq (37) -> izjbi, dqqyiwm
bzmra (34)
gnuywz (30)
rutqvq (60)
kvgrf (38)
rpbawy (67)
uunjwu (70)
nlrwlj (49)
ettsdu (91)
pyjzb (8)
ehhlk (75)
vdekve (292) -> xbaytg, rvftpif
ycrxr (312) -> twvlr, djplc
pdewbw (74)
xzzar (239) -> tqzbg, zyrztc
hwcbsyu (167) -> wjjmh, uwwyv
msowvgh (6)
jptxyhp (89)
ulkzj (39)
ytzrx (275) -> uspfk, wpner, wwcrqwq
lunivx (78)
tfdjhaa (28)
lzqtms (7)
qsgpsfr (96) -> utneufs, exvmk
qmvebxb (43) -> hjuzxv, nxrlaw, pdewbw, hqlni
qkilfj (99) -> mohya, fxsmo
twvlr (18)
fejmhpg (190) -> kvgrf, civkkh
lzcant (149) -> gnoywf, ebjpdvd
fcuhw (390) -> qpcmzl, ujdil
qlxioka (25)
tyealpm (72)
ndbzyu (68)
djqtfp (68)
wlbua (48)
xuhchjm (80)
mwzpa (73)
dqyifwc (61)
tovvj (8)
rthxl (73)
bhpvfr (50)
geiethw (35)
yinhnv (22) -> mucnt, edmsh, apgzt
abofflv (105) -> vjmwzo, jvpob, ddpmrsd
ayxuibs (89) -> kgxseg, tjlrb
ebcygk (42) -> cehqdz, qmxjyg, pwpmx
ebhag (96)
pqwzgz (189) -> jgjbcux, powwpec
pmznzoa (68)
fylao (55)
bbqzlo (32)
chxurcq (54)
egdccd (54)
yghyn (72)
zuxgf (64)
apgzt (77)
ujdil (28)
lwmblwr (208) -> ingwpue, jfwho
oyvaqjr (80)
maaid (35)
pcnbkxm (93) -> bfxdtkg, zduooao, mundgs
gscng (29)
tolty (336) -> vwzein, tqyheu
nzzwed (67)
ruhyv (29)
wxpmbu (84)
izjbi (61)
jbprgm (87)
jpnfrsb (290) -> codroxx, xrvno, pnpge
hcvvfm (12)
yowna (78)
mithbv (50)
qcxfwey (81)
pgnulg (88)
bxucoyp (70)
sxqnium (51)
bzwhd (24)
tbrxke (250) -> kxtzbj, zfuqmcg
hnocmo (30)
hbollnl (225) -> qrbvsw, mcnek
pfhjtw (73)
mrydp (324) -> vswcvmu, cwflw
envbl (113) -> ehhlk, cvvex
swojvy (44)
gzlfqg (5)
vblppqb (164) -> qhwkoc, oqiynm, kspoky
rocllr (88) -> tvixzc, seleq, bmsje, vbvctl
vlmzfaf (91)
jfwho (78)
llpxo (6)
pwhrvu (73)
kxxhn (126) -> ahsef, rrlwo
evojkum (21)
cpiscj (436)
qhwkoc (9)
grxqt (233) -> mtzvap, wngqx
rhsdbfh (185) -> rlzbv, xgrjpl
mbqdugj (62)
aelsfhi (58) -> rzehh, fswirnb
douggpq (86)
ysrwm (206) -> zkzpbbp, xsnmq, ofndvr
fjtgkp (78)
tsfrlzw (82)
kghzy (1262) -> gvaie, ykiwb, wztfz, vojblx, cocplr
gzpcl (1291) -> qgzaj, wrxzti, hjrzrli, qklbf
ykryfdg (1297) -> aoqukac, gtkqi, cjhcn
kqitg (203) -> gnuywz, wwolc
bwmckc (60)
yovtgg (67)
irducr (56)
encean (92)
tseydw (27)
tyhtbv (28)
macxvi (179) -> oyvaqjr, arqvl
qklbf (62)
hpocx (59)
qccwy (85) -> yjjrrjj, dgvvi
mehtsc (84)
djupte (20) -> wnoer, qcxfwey, rvqubk, gpcxuo
meuyumr (810) -> vpxbz, mhpbfn, qhutc
crdlymr (1705) -> ixbxszv, vcjpuw, vmoqov
suhcj (14)
lujurf (62)
pjqaa (118) -> bzmra, ateol, zblcbd, qwtlvuj
ddpmrsd (76)
foaubeo (70) -> hbyxgv, totzxaf, dkpmgrq, wtxndn
ykubvpo (133) -> hpljf, wnhmooe
ljscsoa (51)
wztfz (269) -> whotly, evpryza
qaalu (74)
zzjicx (146) -> hvizfxx, xlukbym
ykvqh (305) -> uavzab, xghng
tbmflh (28)
bpazmfd (41)
bwxgrhf (14)
vswcvmu (27)
nnhlcn (201) -> emeyn, vzyxst
vzyxst (35)
qrvmp (426) -> xaplszi, jhdmh, hzcmmh, kkhvw, csoko
kmrbuvq (31)
uqpxyi (81)
tstuc (59) -> sphwh, hpmag
kbothlh (149) -> uhwtkvo, ovalfky, lqtuij, sogepf
xvhnaf (15)
qzsjiw (40)
krnjk (82)
rifmmf (98)
cnqemd (48)
ombqq (72)
xmvxrop (48) -> eaumqtc, dqpcn, ygqjcm
pwpmx (56)
jhggy (5)
ruvrw (54)
qcrxve (90)
ldfybwl (80)
eyavk (51)
smhzr (64)
mtvxp (63)
gmcebvw (26)
hpbbqmn (84)
wafsr (73)
homrc (78)
znnjzzw (8) -> sbsustu, sbtyi, clzgzgc, mykkoc
umitnao (80)
tncexxs (15)
qwbkv (92)
tipdlf (61) -> dnirbtx, kttzsv, mysslj
wnakg (166) -> hpcafa, emecx
fxzvbzr (51) -> qqbdtow, xuhchjm
tgjdv (80) -> ogkucp, bpusqb
hiaoc (31)
onbptkd (27)
yhovo (78)
zogeax (85)
qqbdtow (80)
leoti (41)
yomawy (154)
qfyrdo (68)
rerviwo (98)
liibv (8767) -> toccxvy, xbgmzxd, vgowu
uxmfcyr (81)
cfdumg (27)
emecx (91)
hbbhm (137) -> hacgqe, egdccd
zvrld (46) -> xsilcvz, evxjdo
ixckwlm (47)
ruitlxx (67) -> jbiaxln, ekfwsv
wwolc (30)
ixhsq (51)
rutmxs (14)
sekcqad (72)
haeee (400) -> qozshd, prfbji, jenxqco, wcklpgi
uqwbtx (41)
phoic (5)
kqbrwt (43) -> ruhyv, xxpauyc
btzbl (78)
ziock (49)
yycys (11)
tvixzc (87)
frucbsg (69)
ruuovw (41) -> hflkio, lneqx, fhmkuo, haeee, jvefqe, hgsdq
wamxgvn (511) -> ywxionz, olxmnkc, kqitg, envbl
qrbvsw (54)
evpryza (17)
zjxyreo (34) -> gvuwq, awpvs
sfyqi (17)
gtutr (74)
ctifv (87)
yjjewyt (43)
mofdlro (789) -> aplauwh, tcrcyis, bfkpjdl, akkrsv, lxshx, xmvxrop
hjqvntn (59)
wxirji (553) -> pmvdtmm, fejmhpg, paqggr
qgzaj (62)
qqszul (18)
kgxseg (78)
iktols (81)
orzwmml (478) -> engay, zbtlfr
gkbpz (78)
inattlb (72)
jjdcuql (183) -> xyquxix, uunjwu
kkhvw (1133) -> zjxyreo, fgptt, ebcygk
rrsfv (11)
atdrk (285) -> kzflj, qjlfwtm
nrarwj (75)
tdymtd (7)
bvxiqzm (42)
ehkhs (28)
cvpoqqk (211) -> delev, vebyb
owizuw (62) -> ixcxv, qiikh, wgdrxl, xzqgkqn, vcusbhv, fxzvbzr, jbxcafx
kgamsy (28)
cmhoj (12) -> frucbsg, yccfitx
kbgmlab (43)
epdqjdp (244) -> epbevf, hlyfdr, szoomk, gmcebvw
vrgblbn (191)
ndtcui (239) -> umymwd, ptydckb
xjoid (154) -> xuqhpz, nglumw
mefyd (54)
cpwdhw (75)
jwasx (246) -> tyhtbv, lslsl
pwyiwkf (281) -> wkdxdem, yhovo
xcndr (8)
yvirt (960) -> nopjkom, spqwtg, fyqlqkg
xttop (30)
nwyqp (1965) -> yxmycw, sqehayt, eisyowo, ykryfdg
clzgzgc (91)
agsrs (185) -> qsnjuqh, wcztr, xrsaryj, ifqsqyz
ojrsv (35)
gnoywf (19)
wyzmc (97) -> dlrfwee, vrmevdy
tesql (72)
ejsitta (83) -> mehtsc, hpbbqmn, eipyu
ekfwsv (58)
xxitunl (69)
jvefqe (339) -> ulkzj, jflns, grkauy
vwokfc (99)
mtzvap (26)
vzbywuf (78)
cquptt (59)
fmstmh (383)
qgbtnil (86) -> daaldt, ndbzyu, yrotnfh, djqtfp
qshxib (44)
nclav (91) -> vhruv, vjnejlj
azrdj (176) -> zbwrrsc, mcgaqx
cehqdz (56)
zfuqmcg (49)
tkhjz (73)
pealz (18)
hbomk (50)
oyuklmp (12)
klgsp (89) -> uhfiqwc, susknu, nocugh, qsjue
uwwyv (36)
mhpbfn (196)
krcntq (165) -> duzorty, mcvuku, sneog, bnetz
dkvqe (222) -> qzsjiw, vguwn
ingwpue (78)
smzzu (78)
bzplleg (21)
uhptzfg (28)
qwtlvuj (34)
arqvl (80)
bnetz (20)
yfolyk (48)
flqjd (84) -> yxbwlf, fgumszu, wfmxgw
igwew (5332) -> kjdjj, agsrs, vjugmg
vjnejlj (34)
svtcvi (14)
lnuqdd (154)
evxjdo (92)
vvtfwwb (74)
snhww (39) -> yghyn, tesql
cjhcn (124) -> aonpk, caqjohx
iovcix (92)
mgexekz (212) -> namzem, rjksjq
ajyyjfx (75)
bwpqytq (59)
nvmqchp (23)
asosowy (56)
guttanj (208) -> tovvj, vkkgwbr, xcndr, ksioqg
utneufs (78)
zcakvl (63)
begmhqg (87) -> lyeoeff, efsdxm, vvtfwwb, ezvdo
pmvdtmm (220) -> ckifyf, nvmqchp
fmmhxab (13) -> zxsgnx, iqzkp, ynxfzsu, hwofq, nclav
zaeyze (67)
djsbpg (7)
rqwgj (15) -> nhrla, titychs, tgrfyrx, aoxky, iezlg, yraktfn
lxpkrf (196) -> vkrlz, dvskig
nlixy (67)
gbuilbn (27)
vayyag (56) -> qwbkv, xzvbcms
qsnjuqh (61) -> btzbl, lunivx, fjtgkp
aoqukac (50) -> iiswvjq, prybce
ekcuavb (19)
tbxxc (372)
ippmv (21) -> btxts, daonbjx, zebuqzh, gyqrut
zvpaj (69) -> zcgaxsi, ziock
bltkr (42)
dnflzcz (16) -> dkvqe, eilgcny, rtatr, jwasx
jbiaxln (58)
qmelelp (80) -> epovby, suhcj
mpble (36)
ebtoke (143) -> pmavka, wlbua
pqfnj (98)
kxchm (37)
cznyy (224)
ytcgcq (80)
phqplx (85) -> asosowy, irducr, ktbtfdb, vfgomle
umymwd (47)
zceovs (25) -> koxyrpn, emhnlu
rtkydv (83) -> zjrpxp, lydynp, tkhjz
ayixid (79)
xlukbym (83)
stketdd (157) -> gzdtco, vfjayw
yompk (162) -> unuwu, yasoqri, mzvjxgp
slacopj (44) -> bxucoyp, drleu
emhnlu (94)
mbzme (101) -> hpouwfs, hnmba, dzjef
mohya (85)
totzxaf (94)
ifqsqyz (223) -> vpfoaw, yrmmga
lngffo (61)
xxpauyc (29)
qxsxzth (27)
zebuqzh (45) -> ejsitta, chgpk, zyajdwf, itusrhj, nnunune, qvdrme
ubiuve (6)
mcvuku (20)
uipvt (27)
qqvgeq (359) -> zvrld, nsuysii, uwauv, dexul
gejaump (212) -> iduxx, tiirxay, smpmp
sidmmmy (44)
qjnela (266) -> andixsk, qfsbvqe
idfyy (51) -> vxnwq, meuyumr, oyjjdj, iqwspxd, aobgmc
";

      string irek_input = @"
tqefb (40)
lhrml (164) -> ecblhee, sdjshz
ykntwjm (16)
fbebcq (233) -> ilzfg, vqbvnf, idyiyg, tifpswp
rqjpza (1043) -> xszbzi, zafhcbb, qoouyiw
zazkyd (203) -> vzylhou, ndvkjn
ndfxn (48) -> brxmlaa, nlbvsaj
pfjpsxf (1714) -> uchxwm, ohpvb
tnuvu (395)
ccxsuk (12)
rrhbmgi (98)
vfkeogg (58)
xfbvid (86)
muburl (57)
xxpnqpc (224) -> ksuydd, pmxdc
ilkrxa (9)
lkjkrj (81)
lcuhsxu (75)
flcyx (32)
nuuvgid (23)
ltweydr (11)
pafuvtn (72)
simlgvw (88)
npjbfxb (94)
ilzfg (31)
ingzgv (199) -> brxtyug, nfexc
cejlti (341)
boslv (10868) -> ygeeepa, asiigv, jiorqgn, fnbpxoq
dqkxvd (112) -> wkbqh, dnirvcs
sczzyjo (167) -> euaku, ytgdbn
uwnwx (171) -> czmxofi, tmmbh, engsa
gxlcwkl (34)
suwehi (15)
gkpirl (162) -> vbrwu, tnjkbcd
dpahgkv (52)
pbebtfc (114) -> bubcg, vaiqef
ymnissn (47)
mltftcq (21)
rejewfv (71) -> rfuqci, oyuep, lfveq
grbnbad (199) -> mltftcq, gepue
kuwuy (97)
rzhwl (1936) -> vmlmrk, ushmwu, tziitox
dbqujc (87)
texjaoa (104) -> mpkijdc, lyqbnl
wpxaup (975) -> jijmyu, ejjdfwk, jsrmk, ofshfcn
exqxh (31)
fmtzdw (37)
vmbeqe (33)
fxaom (41)
ewowmhn (63) -> xyioev, qbepv, rqbld
lgpsy (73)
fjgns (149) -> tmbuahz, wvtlu
cyyjrf (56) -> gmobt, hlsvyhx, ngwdz, hrrzu
dntxe (99)
nafrtm (2458) -> gpkgkv, vuqcpir, jvegsv, xhorr, rzsxtev, xksfmz
lebuxmo (14)
fbfqvq (29)
zcegp (98)
clfvfbd (167) -> wnbbrab, bdmmi
wfaus (13)
gepue (21)
lmrhhj (27)
roprp (6)
itimbmg (71)
fepdsz (16)
phcizz (93) -> olmzxu, efkixrq
vuqcpir (1823) -> uljiu, qignfgc
sbvopw (49)
vebuhzt (136) -> eyfds, nhylqxq
rdssp (44)
lipbj (68)
mjfnukc (83) -> sbvopw, kgclit
ltfxsl (97)
wkbqh (81)
xbnbi (32)
vgffruy (98) -> yaueae, lpflvwn, qirikp, xzjcbwa, kshtume, voanano
lcxeysi (275) -> pfqyoho, mnjhqkb
nadgwb (87)
lycks (6)
nyghbkz (10)
racbcat (78)
hhtpv (19)
kjonnd (128) -> vfuywa, zttbwuj
dnpbek (219) -> cbwrev, nrtmf
wrrdy (55)
khumo (37)
xhoytye (85)
wpulnl (98)
uxmjz (38)
oqpxd (56)
iztdlnf (7)
mofllk (131) -> lyfsi, rybovf
fmzpc (21)
yysvm (89)
boropxd (4285) -> slzaeep, hiotqxu, qppggd, iahug, cwwwj, upfhsu, jjlodie
hqbunkc (88)
dteoa (52)
dhugaw (145) -> gdbut, gkbmdh
izvfjt (89)
nfomnyt (9)
gbyblhe (75)
mjkgsg (97)
vkgbz (79)
ldfun (87)
ttfhrp (82)
vjaffbt (391) -> cdqpv, pavbfb
jgrigjx (24)
upjvowk (87)
dfmrhz (31)
qqrqid (82)
kbqpd (30)
wqtcnyz (63)
kwfghyi (7)
ajacjz (174) -> mthaau, ifogeft
pglsa (129) -> wimpgm, wtcuxsp
eakfc (45)
jlhly (16)
vbrpgbe (96)
ydpxkx (8)
zpqlfyn (80) -> skxcsro, uelcs
pbonnr (208) -> kpxdh, izgkl, nquecpp
obxki (9)
drenb (85)
vozeer (10879) -> rejewfv, fnkxzn, xxardb
yixmif (84)
psdcvl (202) -> lgbltl, oriskzl, zazkyd, synlyuo, mwtqk, oryvzmp
spnkurd (66)
rxcyz (80) -> nbonyx, hmrbytx
kjbps (23)
qkbflxg (94)
hywytz (162) -> amrvw, gzxpx
sknxjo (59)
cnnajhy (80)
uiqzhqu (84)
omtrskm (17)
pnbzj (60) -> vbrpgbe, cuupma, yddiq
jiybk (49) -> nnmcrj, vsyhi
cnyyygq (54)
tekqjru (6)
qgnkn (66)
qvstpyq (89)
ntgdq (48)
phltmve (13)
anprhpf (70)
epnfp (39)
lxdlq (48)
yuiqntx (55)
oyuep (270) -> xsdlmg, xvbezu
hobchva (97)
qtwaod (228) -> tdxve, dsoipkq
draxijw (67)
evkyf (15)
sipdbpd (82)
vlxqvi (266) -> dzbqu, qnmgp
eelrlu (36)
opilymx (35)
buzsiye (16)
sabkxwy (189) -> zddnqld, gomrr
fwwvg (88)
teafht (87)
zzlgja (27) -> boclo, eywfs, tskeqa, gbyblhe
ajjyaog (327)
worqvps (32)
illzp (29)
tifpswp (31)
jzeeng (88)
havmool (10)
wnbbrab (61)
dwtgak (1742) -> yolqpau, paetkc, wjpvpg
ieejg (255) -> islbpsv, fcfujgr
pjrtj (64)
idqfawa (1327) -> osfsey, mfhtd, owbhv
dntzv (9)
ihmixw (93)
upvzg (45)
cvmlcyx (65) -> lkjkrj, dslabn
cjhxpn (9)
hjhaae (44)
axstuln (9)
tcymb (93)
hmrbytx (46)
jwrcrra (37)
ltfsqn (95)
vktrdf (79)
xyioev (67)
wdjpqq (43)
lukls (78)
rhinivh (56)
yxxrv (49)
rodfe (116) -> svxrmhl, fpqfvd
wtcuxsp (31)
ewlryhh (5)
lqorouz (721) -> kgxmev, zrrklpo, lazovhv, cajloqp, jstihz
anendx (90)
ghbmnod (61)
pkhhbp (452)
owaqfb (84)
uwgomw (260)
wxcas (256) -> qrzofif, vdexe
tvzbk (66)
jmpyaib (69) -> rhinivh, uplalh
ffzpfp (64)
clxasb (66)
dnaclwb (12)
ixdxwhj (89)
jzklwrz (1218) -> ocspnqg, zrmtgk, dfxox, qpbqzwi, uwgomw, hobrps
pmkcwxd (81) -> ksizuia, ayrmwy, uutzk
utsyk (5)
zttbwuj (43)
vzhrt (9)
poywgwi (419)
eelwu (83) -> ttfhrp, sxagng, lgpqna, qqrqid
mgmmqpf (50)
ecrkcs (66)
nmlmm (97)
akzwfo (11)
zxymd (206) -> gyxopp, ychlsfn
lxeeq (26)
pkrbgb (88)
hnfjb (13)
koqnz (95)
aowql (5)
engsa (81)
gmobt (96)
nexgspx (78)
humrsd (35)
bwpvsv (8)
tuxctb (97)
iqefm (211) -> vftol, haphukg
llmhaad (156) -> bkzfd, irxbhi
pdyjzd (142) -> zfjuv, hqbunkc, ghkyui
sjvdfk (89) -> zvefixb, ftomb
hfhqvb (2026) -> sqsyxpe, iaeemxc, vuzuq
puzsftr (67)
gachpm (41)
fhifg (1924) -> lkfcgw, magml, tjrdifv, sefzkr
nzwdyjh (48)
idfja (47)
kiueoo (181)
yhmclb (87)
riuhtpn (101) -> ydapn, wuqhqm
tsasgm (13)
mjnjzet (153) -> mopevhs, hvyscq, ugtpfof, qnbxo, qerhwfp, xojyah
tfwcyid (293) -> ajqaniw, accqde, fwzkvc
hvigowx (97) -> hujizfz, eoyva, sknxjo
bdudb (50)
jocdav (91)
egoaw (5)
fuaaot (91)
suslpgz (116) -> zvldmx, bjtca
htxwgxj (36)
alsrt (189) -> dvpjab, ipwdq
ndgra (135) -> ukzms, hrrxs, cilwhdb
cyzobwd (21)
nozqe (42)
boclo (75)
niiwne (50)
otsaip (35)
ljpyq (32)
wvtlu (93)
oryvzmp (227) -> mcihl, buhmvt
oijissd (23)
uzowuwt (44)
vmvnrq (97)
qwqenwn (96) -> rrhiz, pqeso, evliz, uvvsl
tmmbh (81)
accqde (92) -> kdbbkg, vzcyoxl
oufzuw (89)
hqgbfvn (68)
rxmxfno (76)
ozmoirq (107) -> oitkdg, xsbjwz
mefsxl (5015) -> ixlerg, uoujbtv, khjbclb
qirikp (91) -> koqnz, mbora, ltfsqn
jlpfu (144) -> avrfz, ywuqf
dkuds (97)
inggl (261) -> qgudtwk, myfcqxe
tixcp (14140) -> qoyhxl, jgvsp, zwtnf
kyspusd (25)
vdxdq (68)
qiqvce (72)
ehgnylr (8)
nrtmf (15)
xavay (8) -> fibcigx, omvflhe
socuv (6510) -> tjvry, ybqehu, mapirt
sdhtcgx (388)
ogksb (78)
tuytxy (65) -> nteuv, jsdzajv
hyvhosf (25)
kpdms (44)
mswjlev (107) -> jfdud, knpefcs
ckrqr (30)
jeejn (82)
idgrh (20)
dnirvcs (81)
jbutmk (109) -> qgnkn, tswgpqg
lwuxii (25)
uhmypt (39)
swkfz (92)
ugzxbjw (72)
ccyflwf (111) -> spnkurd, slbun
xxcpmu (89)
gworrlc (14712) -> aaizfj, nqayozt, xftjd, whpjefc, dnpbek
qdkcyj (40)
erzpzd (66)
alpoep (51) -> wammii, svpwz
nteuv (89)
uwmocg (72774) -> npwjod, fmqxggg, tkaax, tbpyoxy, mefsxl, hhxofiu
odpuf (15)
sxagng (82)
fcxag (57)
kebxaum (31)
ahxse (331) -> qwtss, stqsnlz, bgibn, vjrrptz, ehqap, pdawj
wvyht (26)
ockjo (29)
aiqug (46)
ejehv (1135) -> owxcxd, nvbococ, pmkcwxd
invkz (94)
yrdsxpn (12)
gvjwj (23)
itfiqj (163)
gimjz (201) -> vaxierg, feope
ekzro (71)
zbscr (378) -> myfkdkl, wtpgz
jmrry (87)
xfwju (45) -> wnteyab, zxmpvm
jpsoy (1570) -> sewtbk, mofllk, txhcyqf, tkntd, oqqnazv
efmrpp (78)
odqkxi (6) -> gvjuwwn, quhyxh, dqkxvd, misjd, vmnys, rtgpmf, lhrml
blafijn (262) -> lwuxii, qfmofym
yqcwdh (80) -> rfmbo, frvqvlo
hhjgskh (7)
idxgock (23)
ipgrte (66)
zpxbc (118) -> cletkls, krvtfsj, budohp
yjqdgq (30)
vgfve (9)
uclzjin (73)
pqeso (116) -> xgxea, gvjwj, grksx
fmqxggg (6539) -> jzlquwl, oipjur, slvkou, pjpfvqe, gcjdpx, ebtkonx
ytjuerk (23)
vpgxh (85)
mnepu (75)
zwjuqr (26)
mtjcqtg (52)
djzbom (89)
iwgtul (79)
hwvkbv (47)
olmzxu (10)
buplvox (42) -> xzxsnus, azpwos
fscqnmn (77)
xeblqw (55)
lazovhv (153) -> deagdif, jasamk
mopos (62)
sewtbk (123) -> zddbivo, esyatlt
ifhntf (8)
uxxwj (215) -> gkuqwak, qqesw
hqesnp (58)
ncdnc (34)
psmlok (55) -> hbabeiy, xcfvrz, lujwkxl, tgtwgnl, ueacv, fqorpj, ennlusa
awnos (97)
ojlgsc (74)
kshtume (300) -> jatvuw, uxmjz
qidhm (4140) -> svgfqs, zyuuri, ylysm, psdcvl, pfjpsxf, pduiy
pfsgds (86)
rzsxtev (419) -> kqiupp, gxgnzi, glveid, iyqzno, fhfcmv, crxpc
jbhoa (52)
pcciu (40)
urvwum (170) -> ytmnrjw, fkcwv
bkzfd (96)
jldxskr (14)
hatujsc (93)
droaqx (29)
fohnt (22)
czwrn (1100) -> ykoyzh, wuavkqo, tvgfdb
kvmjx (850) -> mjryqy, evofre, gfkxqmk
sqdyn (61)
weelykw (46) -> vqmckky, hzxgz
yiipi (27)
dhltapo (98)
qsetl (310) -> isesfdw, fsdvpk
ctpac (68)
ycnqav (5)
uplalh (56)
xpgzx (48) -> zkpghh, eawuhm
hlabv (1742) -> qxvfq, vpesyi, untiezr
ifogeft (21)
vpqmdqf (55)
lzfuzm (32)
gszkqio (79)
gomrr (76)
mzmyto (45)
zcohp (41)
haazrx (21)
eodye (116) -> eaxlxoy, sbovaf, fkawsxk, uvfhx
xxzzr (85)
ybqehu (16) -> qtwaod, uwnwx, vzgkr, aywwpfz, kpbdkq, hbjrjw, sxlkxs
vaale (90) -> idfja, hwvkbv
grqfud (823) -> wopvp, ccyflwf, usmhoj, hlnram, oyyfhxh, bneqef
hrrzu (96)
ceibzne (23)
jmnhj (8)
esyatlt (38)
vhgtr (24)
ffakxgn (58)
dcdxxv (21)
zvefixb (69)
shwph (30)
vuzuq (23) -> tbhdon, suakgk
nmber (37)
pjpfvqe (8) -> arhfcx, hgqsp
arugmz (67) -> eflbhmn, gekwq
tvunonh (89)
ndpweps (99) -> wuuxzqy, jyuvjzz, xdgbnoj
fxrkpax (55)
voanano (320) -> zfavz, smttd
szwdc (8)
bsqbqtw (31)
xvjmz (155) -> aktlwea, jxszqd
azzpq (153) -> ssngqqp, lebuxmo
qndtc (43)
dvsntva (37)
vzgkr (294) -> rguht, lnxxnux
phqtbfn (38)
zhiacn (16)
oseerig (410) -> haazrx, aqcscz
fagsd (303) -> hrciraz, lnnhuld
mpjvq (34) -> pfsgds, xfbvid, iebfp, xmhokd
bgibn (75) -> vmbeqe, ikdpgy
wqbwqi (66)
vpesyi (204)
tvfhovv (20)
tjnerh (2780) -> ahhqrp, rlnee
sjyymi (78)
xgxea (23)
nqayozt (69) -> mruoew, vogdr, ihquo, ngbto
diwwjq (43)
msrlpql (68)
wnteyab (67)
tzumnfw (1029) -> nttyljj, rwmqvs
wammii (44)
ggttj (237) -> ckrqr, kbqpd
zddbivo (38)
ulymdk (84)
qbjpr (41)
wfnrlgo (12)
wxeki (6)
jijmyu (274)
socgh (50)
eiceooz (412)
jauxc (17) -> tuxctb, utnjrpg
klflx (19)
ueacv (192) -> ygzvz, wgbupli
yzfbc (45)
rocyko (218) -> rgzrfak, trabc
djblaef (79)
tdxve (93)
fpwfhqa (317) -> ocxbu, bqzya, wevkbue, roprp
dslabn (81)
eqzjl (185) -> albxmd, fdetm, ctjyra
hjwug (13)
vfuywa (43)
rtjax (93)
tuuqm (128) -> sokar, wtjzsm
xuxuy (224) -> qkbflxg, tvlhxbd
dzbqu (28)
hldjtya (48) -> xxzzr, navzdpq, vgbxwsh, xhoytye
vohmteg (10)
uxuwgmz (8379) -> lhopv, vexiqv, tjnerh
yitoepf (20)
vpmyvzz (11)
sinfhwq (219) -> tsdpmhr, xcaqylz
betkfui (84)
vqanko (399) -> ygcph, dbqlqa, oawaasr, ajacjz
lkfhfri (23)
zpdcajt (44)
ebtkonx (158) -> fmdxpvz, fdgvmi
milnj (9)
ytacid (51) -> upjvowk, sgmtal, ldfun
uzasvyd (66)
cbjyf (15)
clbkb (15)
bqzya (6)
oiyohma (24)
dohxzvo (752) -> vuxcbe, lczpqwp, pglsa, jntictz
qbnwlwe (67)
myfcqxe (67)
uqcjwp (52)
srofdd (90) -> rtjzffy, qbjpr
cprxbbf (1657) -> dpqsg, myaznzp, yonamnd
ibfxt (25)
lhcix (265) -> jxmvpr, lagoq, bwpvsv, ehgnylr
wuluv (27912) -> atgrdn, hdpqtg, qidhm, socuv, boslv, fcakejv
wuuxzqy (45)
sfevya (11)
jjlodie (46) -> jtxvw, cjwnq, mtpbt
vounq (42) -> ounsg, rbhzdm, serls, ygqzh
syzptux (14)
aptrqbk (20)
gznxb (15)
eevia (89)
emdolt (93)
kukkmrx (223)
nmdit (71) -> qcaxee, cmtpu
whfwtio (60)
zwxkyn (85)
mamhdgl (43)
zbtgzh (52) -> zeien, gszkqio, iwgtul
pfqyoho (57)
oawaasr (158) -> jpadrxu, ylepv
gahopg (64)
dobuzav (11) -> tmzvfia, ihmixw, emdolt, hhqks
eieqwo (26)
qvvgle (20)
mcponby (11)
dqabk (41)
zxcjlk (2139) -> llmhaad, oykcbpj, pnbzj
pmosxof (128) -> kgiswh, flspx, qszzvq
bjkfl (96)
fcpmz (21)
maykdnd (86)
idyiyg (31)
vzcyoxl (22)
gwfkzhk (43) -> qbnwlwe, draxijw
qmnjlw (95)
azpwos (54)
slzaeep (1023) -> edshwfr, emuysfg
yusdv (45)
cohgcil (41)
bkjhdgy (72)
nqjsdi (20)
oivlxtz (9)
ocowqvd (7522) -> vgffruy, ykjng, hlabv
lujwkxl (54) -> bkcozk, bcxne, mvhybn, ipgrte
wuqhqm (41)
vaziim (27)
wcgfkq (630) -> zermehr, tfaxln, zvtsnz, rukmtyd
ciwydtl (65) -> ezzzomx, fmele, luxtn, uclzjin
ndvkjn (36)
kunzaag (2592) -> mgvovnb, ftntagd, ggmjft
sopjux (81) -> tddqw, iumspgx, ojsntix, uxuwgmz, ehwofnh, zrtrhph, rujdblh
ifengok (20)
hlsvyhx (96)
amzaq (59) -> jmrry, plexk
ehqap (141)
pzoji (47)
tebbb (25)
oglut (40)
zwtuhb (187) -> opilymx, racquh
brxjw (120) -> bkjhdgy, pafuvtn
hlfvf (53)
gkctmh (31)
glveid (267)
fodfvds (77)
ngffuwc (123) -> jjkyj, xczaehv, xpuly, qqdrox
noksmmp (21)
vhzoa (84)
txhcyqf (91) -> cnyyygq, iuuhcc
aauxa (69)
upfhsu (27) -> jukhdvi, lilufvg, rodfe, hvigowx
brxtyug (71)
ciizgft (55)
ebcls (73)
jsrmk (7) -> yukbvwy, tvunonh, yysvm
cletkls (82)
yaqlguh (9)
jylvi (535) -> kjonnd, fisyh, ayuwm
opkren (49)
mukkakg (33)
xjkzvpn (66)
wbydzcm (686) -> ahtqrem, iwbzsq, jafvw, cyyjrf
gdbut (24)
zcptsvh (1651) -> qmwbv, kiueoo, biner, mjfnukc, anlopt
msajn (129) -> qasbblg, yuiqntx
pvowck (43)
deagdif (96)
cdblpel (97)
ojdlcls (78)
ozackw (51)
lkfcgw (40) -> ckpsa, jqkrk
xhzdo (98)
rlnee (55)
zprakvq (213) -> ixruqck, vohmteg, havmool
jcpbsc (36)
knnmsai (24)
cilwhdb (21)
kkeqyo (47)
kzjdhi (48)
ytvsrny (59)
jfdud (68)
jpyfzy (31) -> xjdxfb, hclhewh
djwizu (46)
vrghbl (94)
qjdzqm (144) -> saflt, whfwtio
mruoew (45)
ptvuev (302) -> yplyevo, dptzp
xnjod (73)
xxardb (1219) -> ifhntf, pulfop
qaosty (9)
qydug (7) -> xhyph, ogksb, efmrpp
igfvkh (45)
hablmj (310) -> jyjvxw, ilkrxa
ugtpfof (302) -> uhmypt, sdhvw
kbclwl (98)
dhhcmdj (1815) -> glwvlc, escef, weelykw
uembj (90)
mvrpr (97)
ahtqrem (358) -> cohgcil, pkejtl
ryxauy (28) -> slaiad, njogs
xhpbaer (32) -> zgjabut, oivlxtz
ygqzh (40)
jxszqd (43)
pgozvs (44)
lislxi (80)
olosbi (28)
fawadg (73)
ssngqqp (14)
xjhfgv (81)
vgtvj (52)
gkuqwak (14)
mnrsu (96) -> dvnzgo, slkkm, xxcpmu, izvfjt
ppmjk (289)
aqviuy (6)
kqiupp (157) -> vbysnql, xyfclte
xzjcbwa (376)
svgfqs (1150) -> lgpgt, ndpweps, qwqwp
fibcigx (82)
vjrrptz (60) -> vwzfk, vaziim, fwoavaz
yolqpau (641) -> nhefqs, pfudc, gdpmx
suzkkr (2330) -> ezilows, ndfxn, vwledx
xvbezu (59)
zgltwud (74)
xojyah (84) -> ojlgsc, ghbhqnl, ztngdq, wwtvhb
tfqel (49) -> vmvnrq, mvrpr, awnos, ezbax
wwwep (20)
tpcmp (87) -> mwnczjf, ylkwbm
ytmnrjw (71)
fqorpj (174) -> cltigxi, ntgdq, lehkqty
vuydl (30)
xtcpynj (31920) -> tixcp, wlkzwch, sclfvp, nafrtm, vozeer, ocowqvd
mvice (298) -> ftnzxce, mkuow
ahotpd (80)
ylkwbm (46)
nxtwjc (17)
eolzkkc (24)
juahc (264) -> vrghbl, bdlkkll
cmuclkp (93)
gfhrh (50)
txbkss (72)
ehpzl (87) -> nzwdyjh, lxdlq
dbqlqa (38) -> csmgexg, djzbom
eiynck (41)
eaxlxoy (84)
izllv (121) -> hohmn, oufzuw, ixdxwhj
ohgyus (42)
adkszk (70)
rxbohqw (75) -> qubxyhi, lipbj, kvfwsoc, vdezdp
ylepv (29)
myfhxk (38) -> cyenkm, elqgxm, uxjzpl, ujdhpxh, ukzcbat, ykphap
jaxczdp (25)
fgjcea (19)
ezcmxxy (245) -> yjffdl, esroty
adzhm (55)
mtpbt (152) -> lrxhito, fvdoc, aauxa
hvche (95)
aywgi (5)
jixdvf (7598) -> dohxzvo, tsjzvs, kvmjx
tjvry (2251) -> mxxvvt, arugmz, umttqu
cptoly (9)
nvdducc (65)
vommsj (42)
ozacwy (66)
qhbwsiu (67)
dgfqg (93)
pmczhb (68)
axbupt (21)
svxrmhl (79)
wxruhqa (16) -> dqbhi, zykjenf, fyvrp, ylxgn
kvztqrg (73)
kpxdh (32)
cdqpv (14)
lilufvg (192) -> qjool, eiynck
lgbltl (110) -> orlkabb, rguvpi, rgewan
iodna (37)
wevkbue (6)
ucntsl (50) -> iakcl, vlpbqn
pfudc (47) -> wdlpc, eoeac
rewfxf (87)
bgcjcd (62)
uggjwfl (1393) -> kvsnv, ksledpc, lzbzre
hvjal (99)
gfkxqmk (44) -> nrufh, zymmhlb
qnmgp (28)
yzvarwz (68)
yyhsmd (26)
ukzms (21)
ytbmfpn (68)
evgddir (44)
jdnia (43)
fdozub (58) -> ytbmfpn, fprpmof, hqgbfvn, yzvarwz
brrvjn (15)
cwqou (69) -> uajllsw, diwwjq, pvowck
wcfgubc (32)
ffimin (22)
ikxcldw (23)
spqvn (12168) -> vwbwicx, vqanko, cqtuah
escef (184) -> akzwfo, ltweydr
bneqef (141) -> iaxvd, aocbwq
hszuim (2409) -> yrdsxpn, nlehenc
bowxedw (144) -> qdoevfi, epnfp
lrxhito (69)
vwzfk (27)
fsdvpk (48)
oyumlz (90)
zfavz (28)
lrhclhk (56)
godqs (64)
krvtfsj (82)
qmtfv (37)
ygcph (126) -> irsrtbm, krvnq
vaiqef (42)
hrciraz (43)
vkzhxfv (20)
omvflhe (82)
wwtvhb (74)
kloow (11)
ydxsq (6)
mgboos (385) -> lxeeq, wvyht
iaeemxc (155) -> pnxmraq, aqviuy
cyenkm (1226) -> gkpirl, alvji, orofe
sdhvw (39)
tjqiwyw (50)
bphpr (25)
qwqwp (160) -> dvsntva, qmtfv
ehyzqx (70) -> havmcsf, xhzdo
ygzvz (63)
qqesw (14)
llpifh (45)
konmvry (135) -> kwonpn, mklrsr
vwledx (142)
showzx (51)
iefjz (6)
gvajepy (7)
svpwz (44)
oitkdg (35)
wpgdv (168) -> wcfgubc, akwdkl, skvcg
mxxvvt (69) -> rxmxfno, hjibopk
movvd (66)
ydndqy (98)
uelcs (71)
glcbz (87)
pvqmab (78)
nezlnj (90)
zzjgcli (85)
vmdwgiy (45) -> itpjbr, mxtcb
hrrxs (21)
atgrdn (90) -> hfhqvb, cprxbbf, eqrmhiv, rzhwl, mzbvwk, bncbco
rruyk (14)
nquecpp (32)
mvjii (41) -> rffho, rjuizk, dbqujc, nadgwb
tmejjp (45) -> rvgcgd, leccfrx, pyfps, dnqxg, uxpirup, mvice, kwikqd
oxpur (53)
tkjff (96)
yqigck (264)
oriskzl (20) -> jyffs, jncodb, zzjgcli
bmckl (78)
rlfno (208) -> okian, cptoly, vzhrt, akbciz
jasamk (96)
lqnskaj (84)
tgxpafk (107) -> hivkyyy, vdxdq
whpjefc (231) -> oglqg, jpyaa, lycks
ezilows (80) -> osaofz, bdnrzjl
eywfs (75)
vrbpoms (35) -> nezlnj, pysmz, wctwj, oyumlz
nblsqm (43)
efkixrq (10)
pyaco (84)
phzap (70) -> xnjod, bydub, lgpsy
xhidor (64)
fphhjt (32)
qlbapm (6)
xqyyotm (58)
sxlkxs (314) -> vclbj, tjqiwyw
wyujf (15)
gabcawj (159) -> gxlcwkl, vnrwmwp
csmgexg (89)
suakgk (72)
xcfvrz (226) -> vcjhxmu, ikefepo
xppxn (191) -> btsvt, duckgf, hnfjb, hjwug
mgetjw (57)
togddow (53)
unjxp (10)
hkcuo (89)
irsrtbm (45)
mtypps (48)
ayuwm (214)
rgzrfak (52)
epaztgn (74)
myytdk (98)
enbtzt (62) -> xxrcw, teuku
vgqxpp (38)
qfmofym (25)
chihy (108) -> mxbid, iayol, ceibzne
lratbc (73)
zafhcbb (865) -> rlfno, xpgzx, hwssny
afejk (23)
ftdkcfr (163)
ocxbu (6)
vwbwicx (285) -> yjsminx, wfnzaoj, aaavm, ftdkcfr, itfiqj, xzmnjz
fwoavaz (27)
mwmguqn (19)
gwdrk (72)
jfakaii (209) -> azxmib, dmayk
zjlqjvv (91)
fdetm (89)
bncbco (1876) -> fdbnn, fiqgcz, tcyqi
zjthm (59)
beqyys (57)
lagoq (8)
ftimasv (1482) -> ibszdcc, mohlj, srsuyk, zprakvq, xppxn, mswjlev, tuytxy
ytxlw (51)
knpefcs (68)
mozuwfi (23)
nhttnio (78)
zrrklpo (345)
zcszxf (368) -> fmnil, mjoctb
rqbld (67)
rrohj (49)
zxmpvm (67)
gzsltcw (33)
swoxpas (49)
mpkijdc (47)
reyaoo (185) -> dpahgkv, uqcjwp
odvasfk (26)
xfvxqr (27) -> bmckl, nexgspx
llpllm (63)
wimpgm (31)
ocstme (1659) -> terrhqm, jdpncg, jlpfu
tkaax (7058) -> msajn, czsnue, kfgafaq
ngbto (45)
sfopk (65)
pklfpox (781) -> tnuvu, irtey, inggl, nlztkoh, vrbpoms
navzdpq (85)
wohmy (46)
fumcav (205)
buhoxvf (184) -> jcalbf, anendx
irtey (395)
vpdbtl (62) -> dgebx, nmber, iodna
uyflsu (87)
fprpmof (68)
bzeyrs (80)
fgfhom (22) -> gbjzyvh, fgxaf, tvfhovv
rthyaa (65)
nttyljj (74)
hdpqtg (10989) -> nzbmqoj, ppxmvic, ngzrfo
eaemp (7) -> rdssp, evgddir, sdfdasw
uhjzmx (21)
tswgpqg (66)
cajloqp (327) -> milnj, dsycw
vexiqv (2359) -> ncsglzf, gwfkzhk, ozmoirq
eoyva (59)
vuxcbe (17) -> tznvgv, yiqiioh
sgmtal (87)
ghkyui (88)
rwelv (86)
uoezmkf (66) -> kiismq, rycova, nozqe
yjsminx (19) -> txbkss, psisfe
ngbnryz (85) -> okvaid, djblaef
tcksxcj (12)
ksuydd (20)
ztngdq (74)
bnbvm (44)
cbwrev (15)
rwmqvs (74)
glwvlc (74) -> wqbwqi, hjjfc
sdvzj (12)
fiqgcz (217)
wobzkg (99)
wtpgz (17)
tkxots (9)
vvbijk (212) -> eznjfl, zjthm
cagiues (101) -> ffakxgn, derfr
vczbfs (8)
pxgfxbt (19)
zrovx (52) -> nvdducc, sfopk
xtojoy (70)
svypeua (94) -> dpcpy, phxuazm, txdyn
xmhokd (86)
vftol (62)
magml (150) -> pyklnj, gfdyw
anqaz (72)
mwtqk (125) -> lcuhsxu, mnepu
zgjabut (9)
mfngva (6)
uiges (14517) -> tetbx, hhexmn, ytgqzq, dakenrk, uwscgd, yyauqx
lchstmy (319) -> hhtpv, fgjcea
lxyirze (38)
znqux (302) -> rocyko, guaqgw, operlbg, yzmeb, ukedg, vlxqvi, mveyx
rpnlu (34)
sifvsqc (56)
jatvuw (38)
jtwfdu (3727) -> pavabvy, lqorouz, wbydzcm, bqggi, vtzgyeg
pnejfr (122) -> humrsd, fxrtldx
kgxmev (215) -> rthyaa, zvzzst
iqvvnfr (40)
cfcdlh (96088) -> dwtgak, whuak, rqjpza, qpdufu
zbbyckg (14)
ezzzomx (73)
dgzcgr (70)
kvfwsoc (68)
xvbro (62)
hkqulxp (81)
xinyot (75) -> hdtplwx, tzehi, aeylapx, zcwclwc, dobuzav, prfyjj, kuycibx
dxtne (44)
yceabf (14) -> hnoakdg, zcegp, myytdk, ewfpgoj
mohlj (185) -> ockjo, fbfqvq
aqbybws (315) -> ydxsq, tekqjru
kptbzl (41)
gyxopp (61)
smttd (28)
yaeee (43)
bdmmi (61)
eedcta (45)
wmjxypj (151) -> ykntwjm, fepdsz
orlkabb (55)
qppggd (171) -> oxcth, enbtzt, ntidj, hmibku
gfrdgnq (135) -> aiqug, eelcez
fdgvmi (24)
nddzkn (271) -> hoiju, hroegp
ijhzbaa (14)
fqfdw (87)
uoujbtv (32) -> bowxedw, eberpkh, zqignj, qzhgmb
brsgom (111) -> hatujsc, cmuclkp
igjmm (132) -> qndtc, vjmhzau, nblsqm, fdwlwr
cibabc (181) -> neihdi, yaqlguh, cjhxpn, obxki
duckgf (13)
jgdqth (89)
ganlaq (1168) -> tuuqm, kwiiccw, ydnsh
ahaan (23)
cnvce (88)
ccaeaw (98)
cltigxi (48)
rduns (60) -> gfhrh, sxonkbq
gkbmdh (24)
ghbhqnl (74)
dqbhi (78)
tmbuahz (93)
jektqvn (6)
isxqmc (34)
hohmn (89)
bdnrzjl (31)
fwssgyj (126) -> ncdnc, isxqmc
qdnmni (482) -> sjvdfk, gabcawj, qxvpaqr, gfrdgnq, joutwz, jfakaii, cvmlcyx
qpkxs (20)
qygtd (6)
evofre (192) -> cbjyf, eifitp
wpwzlpm (98)
njhtc (80) -> gozszri, exqxh, gkctmh
hlnram (31) -> cdcdck, wnjdsq, nfpawkr, vybjgy
wctwj (90)
gvjuwwn (244) -> evkyf, dvispjs
fcfujgr (82)
mveyx (190) -> hjhaae, bnbvm, gsxxi
kvopnl (99) -> nddzkn, lcxeysi, mvjii, fagsd, sylbpy, uriyyc
wemcfwy (38)
azxmib (9)
qjvtm (82986) -> myfhxk, boropxd, jixdvf
mrucj (211)
dsycw (9)
mjjvkh (55)
tddqw (11164) -> tzumnfw, ahxse, iixhucd, flywvww, jylvi
nyqht (16) -> qmryr, kljfbe, fqfdw, rewfxf
fpenvio (262) -> gwdrk, ugzxbjw
zykjenf (78)
lgpqna (82)
icojter (76)
vjhja (43)
irxpuis (89) -> rjxxk, jbbpzze
nfgme (81)
pedazr (40)
mxbid (23)
weiik (74)
ixruqck (10)
afiqx (38)
zqignj (136) -> mamhdgl, fesyd
dakenrk (222) -> axstuln, tkxots
nrufh (89)
rjfiqcz (66) -> tcujev, ycmhi, ldehas
kgjmfr (1582) -> ooixr, amzaq, hrdvx
bavhssu (23)
ydwtc (219) -> gxpdbf, otsaip
eelcez (46)
jyffs (85)
xvwkwsk (211)
vbysnql (55)
fpuuwxw (184) -> aowql, ycnqav
fvbygj (419)
qskjee (24)
uxpirup (318) -> bbojh, ghobjvb
fnjqvc (150) -> jlhly, buzsiye
kvsnv (134) -> oaiipqw, wohmy
ejjdfwk (82) -> tkjff, ncfpaw
jpcais (21)
hmibku (44) -> njlxjg, cdblpel
fbmcsl (27)
zcwclwc (289) -> jdvwj, oqixvwe
qosoqk (29)
nnysyj (34)
pkejtl (41)
dplpd (82)
dvoylt (45)
tetbx (120) -> shwph, vuydl, dqxhlmf, yjqdgq
cuupma (96)
qdoevfi (39)
xczaehv (14)
yddiq (96)
neihdi (9)
qebjnbs (25)
vlzoz (32)
psisfe (72)
eppcdr (23)
kazqqf (88)
bqggi (1915) -> ilsvcl, czksrye, chihy
qfjnebk (86) -> hlfvf, qcyua
fvdoc (69)
lhopv (48) -> uuxwz, byxxgzt, fpenvio, pdyjzd, qsetl, rhhlps, yceabf
uwisk (32)
mkisdq (74)
ncfpaw (96)
hldolxr (241)
ounuwsx (78)
slkkm (89)
qzhgmb (222)
vppppj (141) -> gamtq, fbmcsl
yjxmv (8)
uljiu (99)
qmnykar (244) -> aurya, rpnlu
ylxgn (78)
elhde (30)
qljqbt (25)
csyuq (16)
phyop (11)
dbxgdw (46)
dsnwpy (330)
ljcjmur (38)
kskmxw (56)
rjxxk (53)
aaizfj (105) -> qiqvce, pwqisw
jyuvjzz (45)
hzxgz (80)
qcsqz (86)
igjhbnk (19)
eupya (67)
kuycibx (236) -> opkren, muztqt, swoxpas
jvegsv (701) -> wpgdv, xxpnqpc, yqcwdh, brxjw, yqigck
edcrp (141) -> lxyirze, hfqij
cmtpu (54)
iakcl (60)
cdcdck (53)
ujdhpxh (78) -> wxcas, sdhtcgx, vxbas, izllv, hldjtya
osdwzw (330)
mthaau (21)
lczpqwp (111) -> nvvnxlx, qdkcyj
lhurwx (55) -> wrwqsw, bgcjcd
yiqiioh (87)
qpbqzwi (84) -> pkrbgb, hxhkfs
zyneso (51)
dvpjab (76)
otdcfi (24)
jvaqrl (87) -> drenb, vpgxh
sxrjnm (128) -> msrlpql, pmczhb
misjd (92) -> fuaaot, jocdav
phxuazm (62)
txdyn (62)
fkawsxk (84)
pyfps (184) -> csvhlkb, simlgvw
nbonyx (46)
vgwkv (93) -> tuzpg, zzgvh
fxucung (63) -> hkcuo, qvstpyq
operlbg (322)
wymlcoj (205) -> rovaby, hyvhosf, ibfxt
osaofz (31)
pgmctro (45)
kmmng (68)
raaqp (34)
jhrxncw (20) -> jeejn, fylvsj
zfufrfx (21)
gfdyw (29)
texqz (53)
ngbourw (45) -> kmmng, vyiufa
ijhvxea (347)
qpdufu (3326) -> mbmsvws, qwqenwn, dnodp
vclbj (50)
wuwnzr (15)
sokar (62)
gcbrstj (97)
tjrdifv (98) -> sybqi, errqggj
tbpyoxy (5672) -> tfwcyid, szqdnd, lkmcvop
fmele (73)
xhorr (2011) -> egoaw, twbcuh
zwtnf (108) -> zhhhbv, qpkxs
bdlkkll (94)
dtjuno (53)
mlkbqhk (90)
racquh (35)
nvvnxlx (40)
crxpc (153) -> beqyys, empap
xxsxwrm (15) -> eonodgj, eulpqss
ykphap (920) -> ehpzl, jhahcgn, wmjxypj, izkcvii, riuhtpn, xfvxqr
letus (9)
abnzhwd (55)
waznyo (20) -> rrhbmgi, dhltapo, wpwzlpm, kbclwl
hroegp (59)
mapirt (2822) -> djwizu, dbxgdw
ykdkp (25)
tgtwgnl (272) -> hkjbbs, nuuvgid
yzmeb (74) -> bouvy, ziaty, uamdcy, xvbro
utnjrpg (97)
wacijzk (202)
nfexc (71)
snpwaur (82)
fwzkvc (66) -> lrkttbz, dwuakz
lliow (117) -> pbdoak, rwelv
tsjzvs (39) -> grrmgdv, sczzyjo, jauxc, cqhxddr, vnxrh, mrucj, xvwkwsk
ixlerg (852) -> nnysyj, shdht
nnmcrj (35)
xdgbnoj (45)
jyjvxw (9)
czksrye (51) -> uphktul, llpllm
thdermn (50) -> uzasvyd, zgnyiz
uajllsw (43)
xlgffbd (59)
ldehas (66)
ctbubmz (134) -> tebbb, qljqbt
qnovi (140) -> bykcfy, qosoqk
qcaxee (54)
dtgnqs (67)
jqmpf (90)
jlcvhl (6)
xhyph (78)
iyqzno (59) -> jbhoa, dteoa, qemad, mtjcqtg
zkpghh (98)
rsebscc (109) -> clxasb, ozacwy
zrmtgk (62) -> wobzkg, hvjal
vybjgy (53)
uvfhx (84)
yonamnd (58) -> knyhb, byhqi, vfkeogg, zoagmd
jukhdvi (116) -> vkgbz, vktrdf
hujizfz (59)
uokfe (12)
trwyel (41)
tskeqa (75)
ikefepo (46)
qumhu (24)
wtjzsm (62)
muztqt (49)
fqpfnc (876) -> wymlcoj, rhsqn, rpcczq, vhdbfe, novtxck, svypeua
dntft (7)
sdfdasw (44)
iaaws (68)
dqrwhri (178) -> iztdlnf, hhjgskh
ucjde (44)
kiismq (42)
grvkl (99)
avrfz (57)
byhqi (58)
oykcbpj (298) -> acwtk, mxogohe
njlxjg (97)
pdxgf (236) -> khumo, qlxowz
havmcsf (98)
ceixcl (12)
wurjtrm (33)
mkdxj (97)
ygoxtx (91)
fmnil (16)
nvbococ (118) -> bkhnhga, nmlmm
upstldy (21)
iojycd (158) -> frmmub, mjjvkh, xeblqw, wrrdy
nowyljq (184) -> getbk, eveoo
oqqnazv (71) -> pjrtj, godqs
tflcc (201) -> fmzpc, upstldy
bkcozk (66)
osfsey (25) -> ueikvt, ndpwr
whszfc (135) -> illzp, droaqx
npiost (58)
pyfyx (25)
ftomb (69)
irxbhi (96)
espkomt (97)
acwtk (25)
qmwbv (157) -> tcksxcj, ccxsuk
rgewan (55)
mwbzjrn (72)
kcawhph (23)
ukedg (262) -> elhde, aeeyxh
xtckrp (36) -> eelwu, kwwhiv, brzrx
lcobq (109) -> yafph, bczzzw
mfhtd (89) -> vpqmdqf, btxgac
qqdrox (14)
mxrvwib (36)
nxlzxzq (37)
yukbvwy (89)
bhfkje (15)
arhfcx (99)
sybqi (55)
xsdlmg (59)
lyqbnl (47)
slaiad (11)
qignfgc (99)
iuuhcc (54)
kljfbe (87)
itpjbr (98)
fxrtldx (35)
hhxofiu (80) -> tmejjp, srqipi, jpsoy
qoxcvs (12)
iayol (23)
vtzgyeg (59) -> ingzgv, khaxl, cejlti, iuuck, alsrt, fpwfhqa, sabkxwy
rbhzdm (40)
wuezook (82)
qjjjm (60)
marrb (15)
pyklnj (29)
hgqsp (99)
jiorqgn (136) -> dbdne, pnejfr, qfjnebk, uoezmkf, dqrwhri
duruha (25)
fisyh (46) -> dpygp, nglwlyl
dpqsg (290)
byxxgzt (386) -> nyghbkz, unjxp
omzvw (83) -> owaqfb, betkfui, xklmcmc, avemla
juaekax (21)
bczzzw (66)
dmayk (9)
jdpncg (90) -> oqpxd, sifvsqc, rrdin
iaxvd (51)
uriyyc (261) -> vlzoz, worqvps, flcyx, lzfuzm
agpuxcs (17)
flays (52) -> uyflsu, awhgs, apwqbdt, vmcdxsc
aijyga (49)
paetkc (854) -> pmttpt, ihvtb, ucntsl
zvzzst (65)
untiezr (6) -> erzpzd, movvd, ecrkcs
ndflhsj (1709) -> azzpq, ngbourw, ypccy, jmpyaib
asiigv (25) -> fbebcq, lchstmy, ciwydtl
wcmif (86)
bydub (73)
terrhqm (242) -> ydpxkx, pdebsm
dzdgx (35)
empap (57)
wuavkqo (68) -> davnh, mukkakg
pfbukuq (33)
zeien (79)
blntgdj (35)
rrdin (56)
aycnip (42) -> iyhuvj, kunzaag, ftimasv, zxcjlk, mlvpmj
oipjur (178) -> cipzed, ijhzbaa
efvwgf (41)
ctjyra (89)
jstihz (93) -> ulymdk, uiqzhqu, vhzoa
ehwofnh (18) -> kvopnl, kaihb, ocstme, hszuim, mjnjzet, dhhcmdj, ndflhsj
nlehenc (12)
dsoipkq (93)
qlxowz (37)
fppwvpe (84)
tmzvfia (93)
ufryj (412)
nzbmqoj (416) -> fjgns, igbiz, iqefm
kgclit (49)
bjtca (22)
xxrcw (88)
sclfvp (7741) -> psmlok, kgjmfr, grqfud
cjwnq (191) -> uihbo, lqnskaj
qubxyhi (68)
fhpik (42)
vaxierg (63)
ayrmwy (77)
qkshp (85)
feope (63)
zymmhlb (89)
hdfugh (44)
gsxxi (44)
alvji (226) -> pxgfxbt, klflx
tillos (262) -> zxqbpy, nntot
ngwdz (96)
xjdxfb (82)
ncsglzf (151) -> tsasgm, phltmve
anlopt (139) -> apbzweg, zcxsxky
nfpfpq (97)
zgnyiz (66)
flspx (15)
pofye (42)
xftjd (93) -> racbcat, zefywno
rukmtyd (192) -> wfaus, zeipd
fzivpz (64) -> espkomt, nfpfpq, mjkgsg, mkdxj
dmbdxs (78)
uaofl (27)
kdbbkg (22)
owbhv (151) -> klaqkg, qskjee
uajkvy (212) -> odpuf, wyujf, pprsdm
kikni (59)
myfkdkl (17)
dttxe (24)
iwejes (8)
qjamq (52)
ypccy (39) -> itimbmg, ekzro
djdiyl (51) -> zyxswnx, maykdnd
jalyzrg (189) -> socgh, nqdab
plexk (87)
eberpkh (62) -> bzeyrs, ahotpd
mwnczjf (46)
ibszdcc (151) -> bavhssu, ikxcldw, ahaan, afejk
iipyba (25)
bubcg (42)
vdlhr (88)
bkjpa (626) -> zbtgzh, jalyzrg, ppmjk, lliow, ydwtc
qerhwfp (122) -> vxbieux, wcmif, aktlne
skvcg (32)
tnjkbcd (51)
genete (23)
ndpwr (87)
aurya (34)
dqmcksd (64)
cjikbu (6)
eifitp (15)
aeylapx (383)
errqggj (55)
lpflvwn (296) -> iqvvnfr, pedazr
tvgfdb (88) -> kcawhph, genete
jpjbtm (96)
dujlhd (83)
ilsvcl (95) -> dqabk, efvwgf
novtxck (234) -> oijissd, mozuwfi
gxpdbf (35)
pxkxjy (20)
fylvsj (82)
cyxvk (81)
uhvtx (5)
tziitox (41) -> mtlutxf, lukls
jdvwj (47)
wdlpc (97)
xriycgo (121) -> fkuvh, showzx
dnodp (148) -> rxcyz, qfdlr, xavay, srofdd
saflt (60)
kmjbuf (51) -> kebxaum, dfmrhz
pbdoak (86)
qszzvq (15)
twdlx (88)
pduiy (1736) -> dgfjw, xqyyotm
frmmub (55)
mlvpmj (2598) -> vppppj, jpyfzy, irxpuis
diiqyx (26)
xuknyi (273) -> gvotglo, dnaclwb
tuzpg (13)
hhexmn (225) -> hcvfmh, aywgi, bcyusmh
qxvfq (204)
jncodb (85)
uihbo (84)
islbpsv (82)
omqxa (81)
aaavm (163)
ecblhee (55)
zrtrhph (6535) -> czwrn, jaiqco, tppwhd, wcgfkq, ynsjo, vhysl, nuwaia
ckpsa (84)
ykoyzh (12) -> sqdyn, ghbmnod
rycova (42)
izgkl (32)
fozofs (47)
dnrle (15)
hkqdsjx (360) -> wpzcj, tnrej
elqgxm (1472) -> thdermn, fnjqvc, zrovx
hcvfmh (5)
bykcfy (29)
hvqex (45)
grrmgdv (151) -> clbkb, pzbnrp, zaccgw, dnrle
haphukg (62)
jjrcs (45)
vmnys (100) -> npiost, shwsc, hqesnp
ghobjvb (21)
pvesfah (202)
hmdvz (38)
hjjfc (66)
cqgdvfr (13) -> kpgzhfa, uembj
igbiz (77) -> qcsqz, qtmszjy, hqqcmi
pafhmi (41)
brzrx (379) -> yjxmv, szwdc, iwejes, xrbipx
kfgafaq (95) -> anqaz, mwbzjrn
jxmvpr (8)
xklmcmc (84)
ykrdbmw (66) -> ptvuev, juahc, mnrsu, eqzjl, oseerig, ufcaif
jhahcgn (19) -> nzorq, sipdbpd
xksfmz (1213) -> wacijzk, pvesfah, anobpj, vounq
qxcds (27) -> qmnjlw, hvche
durpmst (28)
krtdc (266)
dpcpy (62)
sfdtqc (63) -> jqmpf, mlkbqhk
vnbuj (20)
opath (91) -> fdozub, dsnwpy, vvbijk, osdwzw, rxmvx, icmfzt
vhvmcal (6)
kpbdkq (260) -> lczyx, ncffowp
yyauqx (80) -> lislxi, rotbjjy
rguvpi (55)
iyhuvj (19) -> nopmvrz, fzivpz, nzskcw, pkhhbp, edpkt, eodye, lyxaz
zwtznsv (49) -> fwwvg, twdlx, skhjby, kfrar
shdht (34)
qoouyiw (1117) -> wwxctxk, suslpgz, rduns
jvhnuoq (44)
bouvy (62)
olvnqyu (23) -> kuwuy, gcbrstj
nakiug (78)
ajqaniw (48) -> uzowuwt, dxtne
mzbvwk (1081) -> dynfb, qydug, rsebscc, fxucung, hldolxr, jbutmk
axouzs (199) -> urvwum, blafijn, ytacid, ysbjlf, qmnykar, vebuhzt
albxmd (89)
slvkou (42) -> xcdycx, dplpd
pzbnrp (15)
vvyhqon (8)
pqrcl (171) -> ojdlcls, dmbdxs
lnuve (840) -> zjowzz, cllhod, uzzuddz, agzideh, mzfmw, chzno
ukzcbat (884) -> rxhmtg, iojycd, mpjvq
pmttpt (140) -> wuwnzr, bhfkje
mvlvwo (66) -> yogzh, xstglba, zwtznsv
yafph (66)
uhtmhy (84)
wnjdsq (53)
evliz (151) -> nxtwjc, agpuxcs
pdawj (39) -> ytxlw, ozackw
ftntagd (27) -> qkshp, zwxkyn
flywvww (598) -> cqgdvfr, dhugaw, whszfc
vhjri (92)
jnenzeu (171) -> wqtcnyz, krfwuk
zjowzz (67) -> ebcls, lratbc, kvztqrg
jgvsp (52) -> jeqlbxq, kzjdhi
ohpvb (69)
seztb (7)
oqixvwe (47)
mgvovnb (127) -> blntgdj, dzdgx
eyfds (88)
lkmcvop (35) -> zpqlfyn, twgajx, yafxw
isesfdw (48)
ywsjwhi (80)
mklrsr (54)
kxotrd (61) -> sjyymi, wrtarf
pavbfb (14)
serls (40)
gpkgkv (59) -> aqbybws, zzlgja, ajjyaog, qkgtzy, pqrcl, gimjz
ftnzxce (31)
qxvpaqr (183) -> dhgfms, gjbeou, phyop, sfevya
edpkt (368) -> ohgyus, pofye
joutwz (215) -> wxeki, jlcvhl
zvldmx (22)
vmcdxsc (87)
fvkqehj (37)
jjkyj (14)
fcakejv (4140) -> ykrdbmw, jzklwrz, kjfmxih, wlppp
kaihb (1131) -> kxotrd, qxcds, cagiues, edcrp, olvnqyu, cibabc
tcyqi (77) -> xtojoy, adkszk
whuak (62) -> idqfawa, ganlaq, odqkxi
nzorq (82)
jyjkwnc (49)
lczyx (77)
gynwzem (92) -> uhtmhy, yixmif, fppwvpe
dptzp (75)
erlwita (34)
gekwq (77)
vbrwu (51)
pqrvq (41)
fdbnn (61) -> pvqmab, nhttnio
mcihl (24)
ncmsneu (344)
fpqfvd (79)
vogdr (45)
ncffowp (77)
ushmwu (97) -> niiwne, bdudb
fmdxpvz (24)
vbukg (15) -> npjbfxb, vyfkoc, invkz
wlfzzwr (62)
mjoctb (16)
ipwdq (76)
hwssny (78) -> vpvvmqv, dujlhd
zfjuv (88)
qbepv (67)
xrbipx (8)
uafzt (78)
pwhdea (96)
izxkgl (81)
biner (121) -> qvvgle, aptrqbk, nmcxmr
szqdnd (284) -> xxsxwrm, eaemp, alpoep
ihvtb (130) -> pxkxjy, vnbuj
wjpvpg (53) -> tfqel, wfnrq, mgboos
pysmz (90)
stqsnlz (89) -> odvasfk, eieqwo
dtbgg (26)
zermehr (66) -> buwffdx, icojter
sefzkr (118) -> igfvkh, upvzg
vpvvmqv (83)
ufyxgmv (73) -> xjkzvpn, tvzbk
nlztkoh (383) -> jektqvn, cjikbu
vhysl (346) -> ezcmxxy, phzap, clfvfbd, reyaoo
fkcwv (71)
vhdbfe (214) -> wurjtrm, pfbukuq
ziaty (62)
rrhiz (113) -> pxknh, htxwgxj
aeeyxh (30)
ddiqs (89)
ugyebr (82) -> mgetjw, eecdh, fcxag, muburl
hobrps (112) -> epaztgn, mkisdq
tvlhxbd (94)
edshwfr (50)
qwtss (91) -> duruha, qebjnbs
horkr (38)
vsyhi (35)
klaqkg (24)
wirzgw (12)
oglqg (6)
uxjzpl (1247) -> zwtuhb, jvaqrl, uajkvy
qdoici (98)
gvotglo (12)
myaznzp (162) -> ffzpfp, xhidor
fdwlwr (43)
cqhxddr (211)
pmhmqfc (89)
zcxsxky (21)
nqdab (50)
emyiqu (67)
vyfkoc (94)
ohkkn (32)
zqnrzvk (15)
jpadrxu (29)
mxtcb (98)
ylysm (1333) -> pmosxof, njhtc, vpdbtl
ennlusa (138) -> eedcta, mzmyto, pgmctro, eakfc
sbovaf (84)
mwikxz (21)
hxhkfs (88)
gcjdpx (8) -> dntxe, grvkl
cpnwxr (34)
sylbpy (355) -> omtrskm, mhzpj
rhsqn (204) -> afiqx, hmdvz
umttqu (221)
gozszri (31)
leaohjx (84)
vcjhxmu (46)
zyybpa (50)
iumspgx (1713) -> fqpfnc, lnuve, oyrmwz, zcptsvh, znqux, grgkrf
yaueae (352) -> sdvzj, wfnrlgo
tpnnkuw (190) -> nxlzxzq, fvkqehj
jntictz (131) -> ifengok, vkzhxfv, tsdgx
ewfpgoj (98)
qasbblg (55)
dbdne (39) -> zyneso, svxpof, mtwbsrl
cwwwj (201) -> pdxgf, ugyebr, tillos
grksx (23)
kemdk (6)
ysbjlf (144) -> pyaco, leaohjx
nlbvsaj (47)
eyovcb (195) -> lhurwx, qwqzr, tpcmp, ngffuwc, nmdit, xfwju
uwscgd (48) -> bjkfl, jpjbtm
ojsntix (6025) -> xinyot, pklfpox, suzkkr, fhifg
eluqn (226) -> kikni, ytvsrny
bbojh (21)
dhgfms (11)
mtwbsrl (51)
mgene (12150) -> xtckrp, mvlvwo, eyovcb
iuuck (93) -> gwkdpj, ximfci, wlfzzwr, mopos
teuhjl (14)
orofe (52) -> oxpur, ullgsq, texqz, bssnhty
tppwhd (46) -> zpxbc, buhoxvf, nyqht, asgmujz
hnoakdg (98)
dqxhlmf (30)
luxtn (73)
kkzzctu (12) -> ygoxtx, zjlqjvv
eoeac (97)
qwqzr (179)
ydapn (41)
zzgvh (13)
iebfp (86)
cvmgc (98)
cllhod (286)
vlpbqn (60)
qnbxo (206) -> nxfjeuy, teafht
rjuizk (87)
twbcuh (5)
kavzymu (248) -> vgfve, letus
ohgok (98)
qfdlr (72) -> zyybpa, zyqsny
wlppp (2421) -> jiybk, ctebhmp, vgwkv
qkgtzy (204) -> trwyel, kptbzl, fxaom
sdjshz (55)
fzrfow (32) -> dntzv, qaosty
vmlmrk (109) -> zpdcajt, hdfugh
skhjby (88)
fgxaf (20)
ngzrfo (431) -> pbebtfc, ndgra, texjaoa, cwqou, qnovi
mbmsvws (662) -> yhmclb, glcbz
nglwlyl (84)
gamtq (27)
jtxvw (80) -> dgfqg, tcymb, rtjax
yjffdl (22)
xecjyv (74)
dpygp (84)
mqhwy (74)
bkhnhga (97)
yplyevo (75)
xzxsnus (54)
aktlwea (43)
oyyfhxh (84) -> togddow, dtjuno, twhnzo
xndzibu (40)
hbabeiy (300) -> nfomnyt, kuqmkzc
hoiju (59)
fnbpxoq (646) -> fscnkbm, ygzyhpx, buplvox
okian (9)
kwiiccw (9) -> cyxvk, hkqulxp, nfgme
lyfsi (34)
kpgzhfa (90)
lyxaz (388) -> ohkkn, fphhjt
xcdycx (82)
subedu (96) -> aijyga, hmsfs
yhhzefl (272) -> knnmsai, vhgtr, eolzkkc
shdbfhl (96)
xcaqylz (11)
iahug (397) -> waangt, nqijykz, pnhmvd
xyfclte (55)
knyhb (58)
nntot (24)
iixhucd (13) -> fwssgyj, kkzzctu, fpuuwxw, subedu, ycnfy, vkufxby
eflbhmn (77)
qqkvxv (52)
zaccgw (15)
vqbvnf (31)
agzideh (214) -> eelrlu, mxrvwib
znslgho (67)
amrvw (52)
tcujev (66)
xzmnjz (59) -> diiqyx, pahywto, ucozsnh, zwjuqr
euaku (22)
jqkrk (84)
ytgdbn (22)
buhmvt (24)
ygeeepa (32) -> hywytz, ehyzqx, kavzymu, krtdc
tfaxln (196) -> kloow, vpmyvzz
mtlutxf (78)
wgbupli (63)
pahywto (26)
hmsfs (49)
pavabvy (1241) -> grbnbad, lcobq, sinfhwq, vmdwgiy, xvjmz
tenzlo (12)
apwqbdt (87)
hfqij (38)
aocbwq (51)
kwonpn (54)
gzxpx (52)
vyiufa (68)
rhhlps (318) -> jvhnuoq, cfhbh
zvtsnz (176) -> jpcais, pgllqy
uuxwz (360) -> kjbps, idxgock
nzskcw (400) -> dtbgg, yyhsmd
ufcaif (408) -> ffimin, fohnt
hiotqxu (877) -> ohlhl, lpweyw, fgfhom
qahbhq (151) -> emyiqu, znslgho, dtgnqs, eupya
pdebsm (8)
wfnzaoj (133) -> zqnrzvk, marrb
rxhmtg (244) -> puzsftr, qhbwsiu
vdezdp (68)
krfwuk (63)
rffho (87)
slbun (66)
rybovf (34)
sxonkbq (50)
cqtuah (63) -> flays, hkqdsjx, zcszxf
czmxofi (81)
qjool (41)
lnnhuld (43)
pxknh (36)
dvispjs (15)
shwsc (58)
jaiqco (950) -> ctbubmz, vaale, jhrxncw
zyqsny (50)
btsvt (13)
rfuqci (64) -> xjhfgv, omqxa, obkparr, izxkgl
dfxox (146) -> horkr, wemcfwy, ljcjmur
nmcxmr (20)
kjfmxih (2163) -> nuomen, ufyxgmv, fumcav
ntidj (214) -> qoxcvs, ceixcl
quhyxh (274)
awhgs (87)
horeib (340) -> jcpbsc, pcdpxv
czsnue (183) -> durpmst, olosbi
fgcjopa (52)
lfveq (292) -> jgrigjx, oiyohma, qumhu, dttxe
ahhqrp (55)
rtgpmf (94) -> dvoylt, llpifh, yusdv, hvqex
qxkcxj (46)
gbjzyvh (20)
ksizuia (77)
frvqvlo (92)
okvaid (79)
mxogohe (25)
ullgsq (53)
vnrwmwp (34)
rujdblh (15833) -> ihtcuq, tvhkggh, igjmm, pbonnr
yogzh (305) -> mtypps, pmytxie
xhljlrn (6)
tbhdon (72)
uutzk (77)
tzehi (199) -> swkfz, vhjri
dynfb (131) -> abnzhwd, fxrkpax
cfhbh (44)
zxqbpy (24)
lzbzre (97) -> vjhja, obctcme, yaeee
ppxmvic (45) -> gynwzem, yhhzefl, eluqn, ncmsneu
pnhmvd (242)
dhpoh (5)
wrwqsw (62)
gxgnzi (186) -> lmrhhj, uaofl, yiipi
joryqqt (44)
uvvsl (67) -> xlgffbd, zdbjk
nqijykz (168) -> fmtzdw, jwrcrra
kfrar (88)
rovaby (25)
mkuow (31)
guaqgw (130) -> vjfkl, pwhdea
lehkqty (48)
twgajx (174) -> uokfe, oircg, wirzgw, tenzlo
krvnq (45)
gdpmx (77) -> gachpm, zcohp, pqrvq, pafhmi
hdtplwx (255) -> gahopg, dqmcksd
xszbzi (13) -> sxrjnm, ewowmhn, qjdzqm, tpnnkuw, rjfiqcz, nowyljq
mjryqy (118) -> qjamq, vgtvj
rvgcgd (332) -> kwfghyi, gvajepy, seztb, dntft
npwjod (7625) -> ryxauy, fzrfow, xhpbaer
kwwhiv (263) -> zgltwud, weiik
lrqskol (11)
eonodgj (62)
srqipi (1107) -> uxxwj, tgxpafk, sfdtqc, konmvry, tflcc, ngbnryz
bcyusmh (5)
kwikqd (274) -> jdnia, wdjpqq
saqta (60)
hivkyyy (68)
zeipd (13)
wfnrq (45) -> ccaeaw, qdoici, cvmgc, wpulnl
zzypx (96)
nopmvrz (340) -> rrfyhi, lrhclhk
ezbax (97)
oyrmwz (42) -> fvbygj, omzvw, ieejg, qahbhq, vjaffbt, poywgwi
eznjfl (59)
dmawkm (6)
hclhewh (82)
qcyua (53)
vzylhou (36)
nhylqxq (88)
ueikvt (87)
fkuvh (51)
rrfyhi (56)
btxgac (55)
buwffdx (76)
pwqisw (72)
uzzuddz (146) -> dgzcgr, anprhpf
ygzyhpx (84) -> gzsltcw, fhsxx
jeqlbxq (48)
esroty (22)
zdbjk (59)
lpxcjq (46)
akbciz (9)
njogs (11)
rpcczq (134) -> jnpwbjj, fawadg
ycnfy (194)
ciaql (8)
obctcme (43)
jpyaa (6)
ydnsh (172) -> xndzibu, pcciu
anobpj (152) -> ykdkp, pyfyx
dnqxg (348) -> qlbapm, xhljlrn
fhfcmv (131) -> ctpac, iaaws
ygfyhh (31)
aywwpfz (352) -> bsqbqtw, ygfyhh
wrtarf (78)
oaiipqw (46)
hvyscq (282) -> zagrlc, rrohj
nuwaia (1310) -> zzypx, shdbfhl
kljhkzg (50)
dvnzgo (89)
izkcvii (85) -> jyjkwnc, yxxrv
pgllqy (21)
derfr (58)
qgudtwk (67)
obkparr (81)
gjbeou (11)
zefywno (78)
oxcth (74) -> wuezook, snpwaur
pulfop (8)
tnrej (20)
vkufxby (184) -> ewlryhh, dhpoh
budohp (82)
rotbjjy (80)
ooixr (55) -> pmhmqfc, eevia
avemla (84)
hbjrjw (26) -> dkuds, qtyeiv, hobchva, ltfxsl
nfpawkr (53)
aqcscz (21)
zagrlc (49)
eulpqss (62)
vnxrh (63) -> mqhwy, xecjyv
mopevhs (220) -> cnnajhy, ywsjwhi
tsdpmhr (11)
ggmjft (197)
khaxl (251) -> yzfbc, jjrcs
uchxwm (69)
rtjzffy (41)
rguht (60)
rfmbo (92)
eqrmhiv (55) -> eiceooz, xuxuy, horeib, waznyo, ufryj, zbscr
srsuyk (75) -> ensngra, kskmxw, jyxztzm
iwbzsq (416) -> kemdk, iefjz, vhvmcal, qygtd
ximfci (62)
ycmhi (66)
zddnqld (76)
qemad (52)
teuku (88)
aktlne (86)
uphktul (63)
vxbas (304) -> cyzobwd, juaekax, axbupt, mwikxz
tzhto (297) -> jaxczdp, bphpr
vjmhzau (43)
mvhybn (66)
jbbpzze (53)
davnh (33)
ofshfcn (274)
hhqks (93)
ckatuf (280) -> uokdqce, zhiacn, csyuq
mnjhqkb (57)
vdexe (66)
kgiswh (15)
skxcsro (71)
whbqia (23682) -> uiges, jtwfdu, spqvn, gworrlc, mgene, aycnip
icmfzt (288) -> jldxskr, zbbyckg, teuhjl
xstglba (297) -> qqkvxv, fgcjopa
uamdcy (62)
fesyd (43)
jsdzajv (89)
ohlhl (82)
gwkdpj (62)
hrdvx (191) -> dcdxxv, uhjzmx
wwxctxk (128) -> vczbfs, ciaql, jmnhj, vvyhqon
ctebhmp (119)
zhhhbv (20)
ocspnqg (84) -> kpdms, ucjde, pgozvs, joryqqt
qrzofif (66)
dgfjw (58)
lnxxnux (60)
eveoo (40)
khjbclb (581) -> tespp, kmjbuf, phcizz
eawuhm (98)
lrkttbz (35)
qtyeiv (97)
owxcxd (243) -> eppcdr, lkfhfri, ytjuerk
hjibopk (76)
lgpgt (114) -> saqta, qjjjm
kuqmkzc (9)
fyvrp (78)
qoyhxl (72) -> vgqxpp, phqtbfn
getbk (40)
qmryr (87)
hdaii (21)
jzlquwl (65) -> ymnissn, kkeqyo, fozofs
wopvp (231) -> mfngva, dmawkm
xpuly (14)
cyrupz (55) -> whbqia, sopjux, cfcdlh, wuluv, uwmocg, xtcpynj, qjvtm
tkntd (45) -> fscqnmn, fodfvds
sqsyxpe (119) -> djvozoc, otdcfi
waangt (46) -> ohgok, ydndqy
tvhkggh (294) -> uhvtx, utsyk
cipzed (14)
ykjng (1685) -> kukkmrx, xriycgo, djdiyl
leccfrx (8) -> jzeeng, vdlhr, kazqqf, cnvce
pmxdc (20)
jafvw (395) -> gznxb, suwehi, brrvjn
zyuuri (70) -> jnenzeu, brsgom, xuknyi, ggttj, vbukg, lhcix
csvhlkb (88)
xsbjwz (35)
bssnhty (53)
qtmszjy (86)
tsdgx (20)
trabc (52)
ynsjo (190) -> zxymd, wxruhqa, ckatuf, hablmj
svxpof (51)
wpzcj (20)
pcdpxv (36)
vjfkl (96)
chzno (264) -> lrqskol, mcponby
ounsg (40)
ucozsnh (26)
ikdpgy (33)
zyxswnx (86)
ensngra (56)
emuysfg (50)
vqmckky (80)
eecdh (57)
yafxw (130) -> lpxcjq, qxkcxj
jnpwbjj (73)
prfyjj (283) -> kljhkzg, mgmmqpf
vgbxwsh (85)
tespp (113)
jyxztzm (56)
hkjbbs (23)
rxmvx (234) -> ljpyq, xbnbi, uwisk
apbzweg (21)
pnxmraq (6)
fnkxzn (1197) -> mwmguqn, igjhbnk
ytgqzq (62) -> ddiqs, jgdqth
oircg (12)
pmytxie (48)
pprsdm (15)
synlyuo (165) -> adzhm, ciizgft
vxbieux (86)
zoagmd (58)
nuomen (121) -> hdaii, zfufrfx, noksmmp, fcpmz
djvozoc (24)
mbora (95)
uokdqce (16)
brxmlaa (47)
bcxne (66)
twhnzo (53)
nhefqs (139) -> cpnwxr, erlwita, raaqp
usmhoj (159) -> vommsj, fhpik
wlkzwch (87) -> axouzs, bkjpa, opath, wpxaup, ejehv, qdnmni, uggjwfl
jcalbf (90)
mhzpj (17)
fhsxx (33)
mzfmw (206) -> wwwep, yitoepf, idgrh, nqjsdi
hqqcmi (86)
fscnkbm (100) -> kyspusd, iipyba
asgmujz (270) -> sqypk, pzoji
tznvgv (87)
ihquo (45)
dwuakz (35)
ychlsfn (61)
ksledpc (146) -> tqefb, oglut
lpweyw (54) -> syzptux, rruyk
nxfjeuy (87)
dgebx (37)
sqypk (47)
grgkrf (1515) -> rxbohqw, ijhvxea, tzhto
ywuqf (57)
ihtcuq (70) -> uafzt, ounuwsx, nakiug
akwdkl (32)
";

      //Node rootNode = RecreateTreeStructure(test_input);
      //Node rootNode = RecreateTreeStructure(input);
      Node rootNode = RecreateTreeStructure(irek_input);
      Console.WriteLine("Root node: " + (rootNode?.Name ?? "null"));

      // not a complete solution but enough to calculate result by hand
      Node u = FindUnbalanced(rootNode);
      Console.WriteLine(u);
    }

    static Node FindUnbalanced(Node rootNode)
    {
      Node n = rootNode;

      Node GetUnbalanced(List<Node> childs)
      {
        Dictionary<int, List<Node>> weights = new Dictionary<int, List<Node>>();
        foreach (Node c in n.Childs)
        {
          if (false == weights.ContainsKey(c.TotalWeight.Value))
            weights.Add(c.TotalWeight.Value, new List<Node>());
          weights[c.TotalWeight.Value].Add(c);
        }

        if (weights.Count > 1)
          foreach (int key in weights.Keys)
            if (weights[key].Count == 1)
              return weights[key][0];

        return null;
      }

      while (true)
      {
        Node u = GetUnbalanced(n.Childs.ToList());
        if (u == null)
          break;

        Console.WriteLine(u);
        n = u;
      }

      return n;
    }

    static Node RecreateTreeStructure(string input)
    {
      Dictionary<string, Node> nodes = new Dictionary<string, Node>();

      string[] lines = input.Split("\n")
        .Select(l => l.Trim())
        .Where(l => false == string.IsNullOrWhiteSpace(l))
        .ToArray();

      foreach (string line in lines)
      {
        Node n = new Node(line);
        if (false == nodes.ContainsKey(n.Name))
          nodes.Add(n.Name, n);

        nodes[n.Name].Weight = n.Weight;
        n = nodes[n.Name];

        string[] childsNames = Node.GetChildsNames(line);
        foreach (string childName in childsNames)
        {
          Node child = null;
          if (false == nodes.ContainsKey(childName))
          {
            child = new Node() { Name = childName };
            nodes.Add(childName, child);
          }
          else
          {
            child = nodes[childName];
          }

          child.Parent = n;
          n.Childs.Add(child);
        }
      }

      foreach (Node n in nodes.Values)
      {
        foreach (Node c in n.Childs)
          c.Parent = n;
        n.CalculateTotalWeight();
      }

      //Console.WriteLine("---------------------");
      //Console.WriteLine("All:");
      //foreach (Node n in nodes.Values)
      //{
      //  n.CalculateTotalWeight();
      //  Console.WriteLine(n);
      //}
      //Console.WriteLine("---------------------");
      //Console.WriteLine("Null parents:");
      //foreach (Node n in nodes.Values.Where(n => n.Parent == null))
      //  Console.WriteLine(n);
      //Console.WriteLine("---------------------");

      return nodes.Values.FirstOrDefault(n => n.Parent == null);
    }
  }
}
