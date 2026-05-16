-- Seed: 6004178202400682985,11218062946904572163



entity uupjouaop is
  port (aynysedp : in character; dmgj : linkage real; lsixoxgg : inout time);
end uupjouaop;



architecture wryo of uupjouaop is
  
begin
  
end wryo;

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (sgkz : out std_logic; nme : out time; gexmimw : out std_logic; zkq : in character);
end b;



architecture cuzmlqdwy of b is
  signal dzsldi : time;
  signal yrlsw : real;
  signal h : character;
  signal dstdoxk : time;
  signal tikenbeahr : character;
  signal mirgzdmnwi : time;
  signal oecipz : real;
begin
  fshbdsv : entity work.uupjouaop
    port map (aynysedp => zkq, dmgj => oecipz, lsixoxgg => mirgzdmnwi);
  tp : entity work.uupjouaop
    port map (aynysedp => tikenbeahr, dmgj => oecipz, lsixoxgg => dstdoxk);
  tyujyv : entity work.uupjouaop
    port map (aynysedp => h, dmgj => oecipz, lsixoxgg => nme);
  uzsopsd : entity work.uupjouaop
    port map (aynysedp => zkq, dmgj => yrlsw, lsixoxgg => dzsldi);
end cuzmlqdwy;



entity cooz is
  port (hchdykpioi : inout real);
end cooz;



architecture xtxx of cooz is
  signal fs : time;
  signal qzyqztkp : real;
  signal din : character;
  signal ghifbq : time;
  signal id : real;
  signal cdvq : time;
  signal rllwhyay : real;
  signal vzpjppu : character;
begin
  yxjavoi : entity work.uupjouaop
    port map (aynysedp => vzpjppu, dmgj => rllwhyay, lsixoxgg => cdvq);
  qiiygkaa : entity work.uupjouaop
    port map (aynysedp => vzpjppu, dmgj => id, lsixoxgg => ghifbq);
  tqqmevbbqr : entity work.uupjouaop
    port map (aynysedp => din, dmgj => qzyqztkp, lsixoxgg => fs);
end xtxx;



entity bmhippvxs is
  port (ubmbzruvx : buffer real; qzdgmmni : inout real);
end bmhippvxs;



architecture klgbuy of bmhippvxs is
  signal boyfo : time;
  signal fleasruca : real;
  signal tuykbm : character;
begin
  mir : entity work.cooz
    port map (hchdykpioi => qzdgmmni);
  sj : entity work.uupjouaop
    port map (aynysedp => tuykbm, dmgj => fleasruca, lsixoxgg => boyfo);
end klgbuy;



-- Seed after: 2184640520731668151,11218062946904572163
