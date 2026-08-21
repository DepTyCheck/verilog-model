-- Seed: 4087387607616101447,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity hw is
  port (yrpf : linkage std_logic; fbahs : inout real);
end hw;

architecture mts of hw is
  
begin
  -- Single-driven assignments
  fbahs <= 2#0011.0111#;
end mts;

library ieee;
use ieee.std_logic_1164.all;

entity auikrktl is
  port (obitg : in character; lgbqj : inout std_logic_vector(1 downto 4));
end auikrktl;

library ieee;
use ieee.std_logic_1164.all;

architecture wecspqgkt of auikrktl is
  signal dzy : real;
  signal geylwq : real;
  signal xfvdob : std_logic;
  signal lswgnn : real;
  signal lxx : std_logic;
begin
  ptzdirotb : entity work.hw
    port map (yrpf => lxx, fbahs => lswgnn);
  qf : entity work.hw
    port map (yrpf => xfvdob, fbahs => geylwq);
  bbxvaj : entity work.hw
    port map (yrpf => lxx, fbahs => dzy);
  
  -- Multi-driven assignments
  lgbqj <= (others => '0');
  lgbqj <= lgbqj;
  lgbqj <= lgbqj;
  lgbqj <= lgbqj;
end wecspqgkt;

library ieee;
use ieee.std_logic_1164.all;

entity qa is
  port (n : linkage std_logic_vector(2 downto 1); rm : inout integer_vector(2 downto 1));
end qa;

library ieee;
use ieee.std_logic_1164.all;

architecture e of qa is
  signal fhop : real;
  signal q : std_logic;
begin
  xxucxy : entity work.hw
    port map (yrpf => q, fbahs => fhop);
  
  -- Single-driven assignments
  rm <= (2#1_0_1_0#, 8#5_0#);
  
  -- Multi-driven assignments
  q <= 'W';
  q <= q;
  q <= q;
end e;



-- Seed after: 1094885181753962621,16188444798499499427
