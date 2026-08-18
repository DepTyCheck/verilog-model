-- Seed: 1138722626702770227,5983430343285687595

library ieee;
use ieee.std_logic_1164.all;

entity qhalozp is
  port (cw : in std_logic_vector(0 downto 2); hxo : buffer integer);
end qhalozp;

architecture biyhvjjkwa of qhalozp is
  
begin
  
end biyhvjjkwa;

entity obpiyyln is
  port (bt : out integer_vector(0 to 2));
end obpiyyln;

library ieee;
use ieee.std_logic_1164.all;

architecture zdovi of obpiyyln is
  signal j : integer;
  signal cpnlnjl : integer;
  signal qdik : std_logic_vector(0 downto 2);
  signal zgybk : integer;
  signal dgoijaisc : std_logic_vector(0 downto 2);
begin
  aliv : entity work.qhalozp
    port map (cw => dgoijaisc, hxo => zgybk);
  fxuy : entity work.qhalozp
    port map (cw => qdik, hxo => cpnlnjl);
  gficzmgwm : entity work.qhalozp
    port map (cw => qdik, hxo => j);
  
  -- Single-driven assignments
  bt <= bt;
  
  -- Multi-driven assignments
  dgoijaisc <= "";
  dgoijaisc <= qdik;
  dgoijaisc <= dgoijaisc;
  dgoijaisc <= "";
end zdovi;



-- Seed after: 3766668669132632144,5983430343285687595
