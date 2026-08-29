-- Seed: 15224171186196707228,10463297573877745897

entity fafgvhybs is
  port (bxrek : out time);
end fafgvhybs;

architecture vsta of fafgvhybs is
  
begin
  -- Single-driven assignments
  bxrek <= bxrek;
end vsta;

library ieee;
use ieee.std_logic_1164.all;

entity wfo is
  port (phindwdosz : out std_logic);
end wfo;

architecture bjl of wfo is
  signal yzbi : time;
  signal od : time;
  signal asil : time;
begin
  qkgjocyfxb : entity work.fafgvhybs
    port map (bxrek => asil);
  pyvtt : entity work.fafgvhybs
    port map (bxrek => od);
  vunoz : entity work.fafgvhybs
    port map (bxrek => yzbi);
  
  -- Multi-driven assignments
  phindwdosz <= 'Z';
end bjl;

library ieee;
use ieee.std_logic_1164.all;

entity wh is
  port (tgcbpu : inout time; eirzngzq : in std_logic_vector(0 to 2); o : linkage integer);
end wh;

architecture muvgpzbywk of wh is
  signal tdssn : time;
begin
  aovbehyo : entity work.fafgvhybs
    port map (bxrek => tdssn);
end muvgpzbywk;

entity agwhjwm is
  port (felzxli : inout real; ecp : buffer real);
end agwhjwm;

library ieee;
use ieee.std_logic_1164.all;

architecture e of agwhjwm is
  signal hcwgg : time;
  signal xshsrgch : std_logic;
  signal yhab : std_logic;
  signal ovhkzbvqs : std_logic;
begin
  jztupthsqo : entity work.wfo
    port map (phindwdosz => ovhkzbvqs);
  fu : entity work.wfo
    port map (phindwdosz => yhab);
  yskv : entity work.wfo
    port map (phindwdosz => xshsrgch);
  edek : entity work.fafgvhybs
    port map (bxrek => hcwgg);
  
  -- Single-driven assignments
  felzxli <= 0_0.2;
  ecp <= felzxli;
end e;



-- Seed after: 4312443271516426065,10463297573877745897
