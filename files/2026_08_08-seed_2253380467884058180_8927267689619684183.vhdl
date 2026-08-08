-- Seed: 2253380467884058180,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity yhkutulwnr is
  port (t : out std_logic_vector(1 downto 3); twartxiy : out std_logic);
end yhkutulwnr;

architecture oobjg of yhkutulwnr is
  
begin
  
end oobjg;

entity rjd is
  port (nkmi : buffer bit);
end rjd;

library ieee;
use ieee.std_logic_1164.all;

architecture qdgsgivnd of rjd is
  signal bpmlb : std_logic_vector(1 downto 3);
  signal qxgsfxbii : std_logic;
  signal ff : std_logic_vector(1 downto 3);
begin
  uqd : entity work.yhkutulwnr
    port map (t => ff, twartxiy => qxgsfxbii);
  rdlf : entity work.yhkutulwnr
    port map (t => bpmlb, twartxiy => qxgsfxbii);
  
  -- Single-driven assignments
  nkmi <= '0';
end qdgsgivnd;

entity plj is
  port (aikfm : buffer boolean_vector(0 to 1));
end plj;

library ieee;
use ieee.std_logic_1164.all;

architecture ijyyjo of plj is
  signal n : bit;
  signal pu : bit;
  signal q : std_logic;
  signal o : std_logic_vector(1 downto 3);
begin
  w : entity work.yhkutulwnr
    port map (t => o, twartxiy => q);
  byhe : entity work.rjd
    port map (nkmi => pu);
  oqzv : entity work.rjd
    port map (nkmi => n);
  
  -- Single-driven assignments
  aikfm <= aikfm;
  
  -- Multi-driven assignments
  q <= q;
end ijyyjo;



-- Seed after: 7601279418850135764,8927267689619684183
