-- Seed: 17877879172928940421,1112937151005418631

library ieee;
use ieee.std_logic_1164.all;

entity mr is
  port (nrkxfpzp : in time_vector(1 to 3); ttyweukgv : linkage integer; nqbyyytdhz : in std_logic_vector(0 to 0));
end mr;

architecture bmb of mr is
  
begin
  
end bmb;

library ieee;
use ieee.std_logic_1164.all;

entity t is
  port (fhclym : out real; iymalthq : out std_logic_vector(2 to 3));
end t;

library ieee;
use ieee.std_logic_1164.all;

architecture eaoo of t is
  signal fdfkusxhr : std_logic_vector(0 to 0);
  signal ltsad : integer;
  signal pdyycytmqr : time_vector(1 to 3);
  signal qoaflciy : std_logic_vector(0 to 0);
  signal g : integer;
  signal yj : time_vector(1 to 3);
begin
  qiaxo : entity work.mr
    port map (nrkxfpzp => yj, ttyweukgv => g, nqbyyytdhz => qoaflciy);
  fkwonvy : entity work.mr
    port map (nrkxfpzp => pdyycytmqr, ttyweukgv => ltsad, nqbyyytdhz => fdfkusxhr);
  
  -- Single-driven assignments
  fhclym <= 14033.1_3;
  yj <= yj;
  
  -- Multi-driven assignments
  fdfkusxhr <= qoaflciy;
  iymalthq <= iymalthq;
end eaoo;

library ieee;
use ieee.std_logic_1164.all;

entity xsx is
  port (mvqslbaaf : buffer std_logic; ezsxtn : buffer std_logic; aupjx : out std_logic; be : linkage severity_level);
end xsx;

library ieee;
use ieee.std_logic_1164.all;

architecture wbngiji of xsx is
  signal ij : std_logic_vector(0 to 0);
  signal oon : integer;
  signal slkcoqm : time_vector(1 to 3);
  signal rfrlgjf : std_logic_vector(2 to 3);
  signal wzopkwzr : real;
begin
  xmvjpayqk : entity work.t
    port map (fhclym => wzopkwzr, iymalthq => rfrlgjf);
  h : entity work.mr
    port map (nrkxfpzp => slkcoqm, ttyweukgv => oon, nqbyyytdhz => ij);
  
  -- Single-driven assignments
  slkcoqm <= (1 hr, 3 ms, 16#1A25F.D_F_A_D_F# ms);
  
  -- Multi-driven assignments
  ij <= ij;
  rfrlgjf <= "W1";
  ezsxtn <= 'X';
end wbngiji;

entity girhcar is
  port (w : out integer; ekzeshorj : buffer character);
end girhcar;

library ieee;
use ieee.std_logic_1164.all;

architecture z of girhcar is
  signal h : severity_level;
  signal yfhsa : std_logic;
  signal znpf : std_logic;
  signal frcxt : integer;
  signal izn : std_logic_vector(0 to 0);
  signal dhhibo : std_logic_vector(0 to 0);
  signal jdxlvlrm : integer;
  signal tr : time_vector(1 to 3);
begin
  ksq : entity work.mr
    port map (nrkxfpzp => tr, ttyweukgv => jdxlvlrm, nqbyyytdhz => dhhibo);
  dynchrlj : entity work.mr
    port map (nrkxfpzp => tr, ttyweukgv => w, nqbyyytdhz => izn);
  g : entity work.mr
    port map (nrkxfpzp => tr, ttyweukgv => frcxt, nqbyyytdhz => dhhibo);
  iqnsbqroy : entity work.xsx
    port map (mvqslbaaf => znpf, ezsxtn => znpf, aupjx => yfhsa, be => h);
  
  -- Single-driven assignments
  tr <= (021 ms, 1 hr, 301 ns);
  ekzeshorj <= 'f';
  
  -- Multi-driven assignments
  yfhsa <= '1';
  izn <= "1";
end z;



-- Seed after: 2164222419755206525,1112937151005418631
