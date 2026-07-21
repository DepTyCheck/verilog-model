-- Seed: 14398052800517758449,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity ziv is
  port (wygqby : buffer std_logic);
end ziv;

architecture bqdckfg of ziv is
  
begin
  
end bqdckfg;

library ieee;
use ieee.std_logic_1164.all;

entity tjam is
  port (vmrmthwg : linkage std_logic);
end tjam;

library ieee;
use ieee.std_logic_1164.all;

architecture zsggabqhr of tjam is
  signal kpioni : std_logic;
begin
  q : entity work.ziv
    port map (wygqby => kpioni);
  dxixxesbqk : entity work.ziv
    port map (wygqby => kpioni);
  
  -- Multi-driven assignments
  kpioni <= 'X';
  kpioni <= 'H';
end zsggabqhr;

entity niwnzcii is
  port (reoxvizw : buffer time; urztxvqjmc : in string(3 to 4));
end niwnzcii;

library ieee;
use ieee.std_logic_1164.all;

architecture xnkhvsqy of niwnzcii is
  signal wkfkutdbsb : std_logic;
  signal ibebdawepm : std_logic;
begin
  rfnofehxw : entity work.tjam
    port map (vmrmthwg => ibebdawepm);
  lecetw : entity work.ziv
    port map (wygqby => ibebdawepm);
  jrsqdsr : entity work.ziv
    port map (wygqby => wkfkutdbsb);
  
  -- Single-driven assignments
  reoxvizw <= reoxvizw;
  
  -- Multi-driven assignments
  ibebdawepm <= 'W';
  ibebdawepm <= '0';
  ibebdawepm <= 'X';
end xnkhvsqy;



-- Seed after: 11705442473878776747,11481034001933599325
