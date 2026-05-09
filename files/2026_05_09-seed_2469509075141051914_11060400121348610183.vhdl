-- Seed: 2469509075141051914,11060400121348610183



entity ezauxrbfun is
  port (scfoufexst : out real; nw : linkage time);
end ezauxrbfun;



architecture vtmofdrdud of ezauxrbfun is
  
begin
  
end vtmofdrdud;

library ieee;
use ieee.std_logic_1164.all;

entity dbqgr is
  port (kqeuo : in real; gypiynold : buffer std_logic);
end dbqgr;



architecture tqfnrd of dbqgr is
  signal jas : time;
  signal xcc : real;
  signal inghyymi : time;
  signal eficciwb : real;
begin
  sftmc : entity work.ezauxrbfun
    port map (scfoufexst => eficciwb, nw => inghyymi);
  rum : entity work.ezauxrbfun
    port map (scfoufexst => xcc, nw => jas);
end tqfnrd;



entity ktvhtnojgz is
  port (o : buffer real);
end ktvhtnojgz;



architecture rh of ktvhtnojgz is
  signal dtq : time;
  signal zckydlb : time;
  signal mq : real;
  signal uyqmqrhi : real;
  signal qlvsnf : time;
  signal dwrt : real;
begin
  nivyzivb : entity work.ezauxrbfun
    port map (scfoufexst => dwrt, nw => qlvsnf);
  jlizk : entity work.ezauxrbfun
    port map (scfoufexst => uyqmqrhi, nw => qlvsnf);
  hooyy : entity work.ezauxrbfun
    port map (scfoufexst => mq, nw => zckydlb);
  q : entity work.ezauxrbfun
    port map (scfoufexst => o, nw => dtq);
end rh;



entity xkbxfer is
  port (aniah : in time; mgsippgafq : linkage integer; lrciehu : in time; hvppjikkv : in integer);
end xkbxfer;



architecture udybrjj of xkbxfer is
  signal yxfn : real;
  signal wknlf : real;
  signal cyipv : real;
  signal usrdtgcwcn : real;
begin
  kmocuuhgx : entity work.ktvhtnojgz
    port map (o => usrdtgcwcn);
  sxku : entity work.ezauxrbfun
    port map (scfoufexst => cyipv, nw => lrciehu);
  iqn : entity work.ezauxrbfun
    port map (scfoufexst => wknlf, nw => aniah);
  znpf : entity work.ktvhtnojgz
    port map (o => yxfn);
end udybrjj;



-- Seed after: 615246175406222994,11060400121348610183
