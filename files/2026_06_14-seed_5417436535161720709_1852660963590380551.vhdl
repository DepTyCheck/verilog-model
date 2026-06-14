-- Seed: 5417436535161720709,1852660963590380551

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (pvjh : inout time; vuys : out std_logic);
end a;



architecture j of a is
  
begin
  
end j;

library ieee;
use ieee.std_logic_1164.all;

entity ehhc is
  port (r : in std_logic; znjebwc : buffer boolean; hsoy : out time; ryxczhnn : linkage std_logic);
end ehhc;

library ieee;
use ieee.std_logic_1164.all;

architecture pvw of ehhc is
  signal xjvbgdr : std_logic;
  signal jdgns : std_logic;
  signal yrgobw : time;
begin
  ktrzcqzu : entity work.a
    port map (pvjh => yrgobw, vuys => jdgns);
  yitdv : entity work.a
    port map (pvjh => hsoy, vuys => xjvbgdr);
end pvw;



entity mll is
  port (yd : buffer integer; afoqxxbz : out time; b : buffer severity_level);
end mll;

library ieee;
use ieee.std_logic_1164.all;

architecture wnbqsda of mll is
  signal mtt : std_logic;
  signal lnpqkzlwbl : time;
  signal xa : time;
  signal vei : boolean;
  signal tneq : std_logic;
begin
  lzqunahlh : entity work.ehhc
    port map (r => tneq, znjebwc => vei, hsoy => xa, ryxczhnn => tneq);
  xlwn : entity work.a
    port map (pvjh => lnpqkzlwbl, vuys => tneq);
  qsejkd : entity work.a
    port map (pvjh => afoqxxbz, vuys => mtt);
end wnbqsda;



-- Seed after: 15015609154032324326,1852660963590380551
