-- Seed: 4500349290510860734,18238119570016518405

library ieee;
use ieee.std_logic_1164.all;

entity vocww is
  port (thxzgbbs : in std_logic; vkqfm : buffer bit);
end vocww;



architecture sxrwt of vocww is
  
begin
  
end sxrwt;

library ieee;
use ieee.std_logic_1164.all;

entity xutsyzgs is
  port (jrbpjlhxi : in real; nrxio : linkage std_logic; a : inout integer);
end xutsyzgs;

library ieee;
use ieee.std_logic_1164.all;

architecture zkauaw of xutsyzgs is
  signal ixwaog : bit;
  signal binthwmbx : std_logic;
  signal yc : bit;
  signal lpytaeinbs : bit;
  signal radxakmz : std_logic;
begin
  soe : entity work.vocww
    port map (thxzgbbs => radxakmz, vkqfm => lpytaeinbs);
  qval : entity work.vocww
    port map (thxzgbbs => radxakmz, vkqfm => yc);
  szlehcrkvq : entity work.vocww
    port map (thxzgbbs => binthwmbx, vkqfm => ixwaog);
end zkauaw;

library ieee;
use ieee.std_logic_1164.all;

entity qzpxz is
  port (zhx : linkage real; lwwegu : out integer; o : out std_logic_vector(2 downto 3); iingelrclz : in bit);
end qzpxz;

library ieee;
use ieee.std_logic_1164.all;

architecture dvhvzqcq of qzpxz is
  signal uikpstuhi : bit;
  signal jea : std_logic;
begin
  naot : entity work.vocww
    port map (thxzgbbs => jea, vkqfm => uikpstuhi);
end dvhvzqcq;



entity hhgdohb is
  port (tmi : out time; onmtvqp : inout real);
end hhgdohb;

library ieee;
use ieee.std_logic_1164.all;

architecture qzbswk of hhgdohb is
  signal warnvzt : bit;
  signal imkambuvhm : std_logic_vector(2 downto 3);
  signal hhpdpocnm : integer;
  signal fzsonk : real;
  signal pcn : integer;
  signal ncbgk : bit;
  signal lotxcfqn : std_logic;
begin
  euadbtyrwr : entity work.vocww
    port map (thxzgbbs => lotxcfqn, vkqfm => ncbgk);
  lliykbu : entity work.xutsyzgs
    port map (jrbpjlhxi => onmtvqp, nrxio => lotxcfqn, a => pcn);
  ubs : entity work.qzpxz
    port map (zhx => fzsonk, lwwegu => hhpdpocnm, o => imkambuvhm, iingelrclz => warnvzt);
  ehfyfpv : entity work.vocww
    port map (thxzgbbs => lotxcfqn, vkqfm => warnvzt);
end qzbswk;



-- Seed after: 17682957280368855559,18238119570016518405
