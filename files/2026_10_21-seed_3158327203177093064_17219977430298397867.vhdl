-- Seed: 3158327203177093064,17219977430298397867

library ieee;
use ieee.std_logic_1164.all;

entity lr is
  port (uibzcfbp : in string(4 downto 3); x : inout std_logic; gqhr : out bit_vector(2 downto 4));
end lr;

architecture pshdkv of lr is
  
begin
  -- Single-driven assignments
  gqhr <= (others => '0');
  
  -- Multi-driven assignments
  x <= x;
  x <= x;
  x <= x;
  x <= x;
end pshdkv;

library ieee;
use ieee.std_logic_1164.all;

entity hxi is
  port (ku : inout integer; wly : inout time; yalqyp : out std_logic; s : inout std_logic_vector(2 downto 0));
end hxi;

library ieee;
use ieee.std_logic_1164.all;

architecture z of hxi is
  signal rg : bit_vector(2 downto 4);
  signal dtqyjonzap : std_logic;
  signal bpmriolac : bit_vector(2 downto 4);
  signal umg : std_logic;
  signal nien : string(4 downto 3);
  signal rcctespf : bit_vector(2 downto 4);
  signal nxfyjhsmf : std_logic;
  signal rpkmabj : string(4 downto 3);
begin
  aqybidbi : entity work.lr
    port map (uibzcfbp => rpkmabj, x => nxfyjhsmf, gqhr => rcctespf);
  kcdxp : entity work.lr
    port map (uibzcfbp => nien, x => umg, gqhr => bpmriolac);
  tjalcljl : entity work.lr
    port map (uibzcfbp => rpkmabj, x => dtqyjonzap, gqhr => rg);
  
  -- Single-driven assignments
  wly <= wly;
  nien <= "jm";
  rpkmabj <= rpkmabj;
  ku <= ku;
end z;



-- Seed after: 13368064993412963784,17219977430298397867
