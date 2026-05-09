-- Seed: 1880199073511752081,17649104089099184527



entity emqdoacv is
  port (eaujuzgsj : buffer real; baofaklsck : in time);
end emqdoacv;



architecture prewkfkcv of emqdoacv is
  
begin
  
end prewkfkcv;



entity kgufxno is
  port (wkmekl : in integer; ezhjutm : out time);
end kgufxno;



architecture zbj of kgufxno is
  signal nsrntti : time;
  signal scf : real;
  signal hh : real;
begin
  idz : entity work.emqdoacv
    port map (eaujuzgsj => hh, baofaklsck => ezhjutm);
  zhslmjk : entity work.emqdoacv
    port map (eaujuzgsj => scf, baofaklsck => nsrntti);
end zbj;



entity fuexfzoh is
  port (tcmzqwz : linkage bit);
end fuexfzoh;



architecture zyqhvxbnf of fuexfzoh is
  signal bcoeqcbkpo : time;
  signal tt : integer;
  signal wiaiacs : time;
  signal gsfds : integer;
  signal l : time;
  signal rflcm : real;
begin
  psspbco : entity work.emqdoacv
    port map (eaujuzgsj => rflcm, baofaklsck => l);
  dczmrfifaz : entity work.kgufxno
    port map (wkmekl => gsfds, ezhjutm => wiaiacs);
  qeys : entity work.kgufxno
    port map (wkmekl => tt, ezhjutm => bcoeqcbkpo);
end zyqhvxbnf;

library ieee;
use ieee.std_logic_1164.all;

entity xbi is
  port (mgjrddc : linkage integer; sd : linkage std_logic; cckankwyi : out std_logic);
end xbi;



architecture qaua of xbi is
  signal a : bit;
  signal lrj : time;
  signal v : real;
begin
  la : entity work.emqdoacv
    port map (eaujuzgsj => v, baofaklsck => lrj);
  tcrgwc : entity work.fuexfzoh
    port map (tcmzqwz => a);
end qaua;



-- Seed after: 5833741572426704455,17649104089099184527
