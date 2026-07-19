-- Seed: 8790356370818468970,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity aof is
  port (s : in std_logic);
end aof;

architecture imeg of aof is
  
begin
  
end imeg;

library ieee;
use ieee.std_logic_1164.all;

entity wyppp is
  port (wbyewnn : inout real; c : inout real_vector(1 to 1); tuxe : inout real; v : in std_logic);
end wyppp;

library ieee;
use ieee.std_logic_1164.all;

architecture t of wyppp is
  signal yjgpkyjn : std_logic;
begin
  pn : entity work.aof
    port map (s => yjgpkyjn);
end t;

library ieee;
use ieee.std_logic_1164.all;

entity zityni is
  port (u : buffer bit; xs : buffer std_logic_vector(4 to 0); rdaut : buffer bit; taj : inout integer);
end zityni;

library ieee;
use ieee.std_logic_1164.all;

architecture cmf of zityni is
  signal tr : std_logic;
  signal jvidqj : std_logic;
  signal zjmrlgthe : real;
  signal jz : real_vector(1 to 1);
  signal e : real;
begin
  uwdqpxp : entity work.wyppp
    port map (wbyewnn => e, c => jz, tuxe => zjmrlgthe, v => jvidqj);
  vgh : entity work.aof
    port map (s => jvidqj);
  ycgybor : entity work.aof
    port map (s => tr);
  
  -- Single-driven assignments
  taj <= 2#0000#;
  
  -- Multi-driven assignments
  tr <= jvidqj;
  jvidqj <= jvidqj;
end cmf;



-- Seed after: 15593595639190158978,5511103086789671269
