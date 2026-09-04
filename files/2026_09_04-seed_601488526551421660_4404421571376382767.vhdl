-- Seed: 601488526551421660,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity uiwh is
  port (ewlxwmfc : out std_logic_vector(0 to 1); i : linkage time_vector(3 downto 1));
end uiwh;

architecture u of uiwh is
  
begin
  -- Multi-driven assignments
  ewlxwmfc <= "ZH";
  ewlxwmfc <= "UU";
  ewlxwmfc <= ('W', '1');
  ewlxwmfc <= ewlxwmfc;
end u;

library ieee;
use ieee.std_logic_1164.all;

entity qndo is
  port (lfjesae : linkage integer_vector(4 to 2); plst : in std_logic; hzmeyohpdz : buffer time_vector(0 downto 0); v : buffer real);
end qndo;

architecture rshyfoy of qndo is
  
begin
  -- Single-driven assignments
  v <= v;
  hzmeyohpdz <= hzmeyohpdz;
end rshyfoy;

entity vjvfakm is
  port (ouljybo : buffer real_vector(3 to 0); ycl : inout real; aq : inout integer);
end vjvfakm;

library ieee;
use ieee.std_logic_1164.all;

architecture y of vjvfakm is
  signal ugrjj : time_vector(3 downto 1);
  signal lfqube : time_vector(3 downto 1);
  signal zcs : std_logic_vector(0 to 1);
  signal kkyeujhko : time_vector(0 downto 0);
  signal eue : std_logic;
  signal uoitmlp : integer_vector(4 to 2);
begin
  lqdapng : entity work.qndo
    port map (lfjesae => uoitmlp, plst => eue, hzmeyohpdz => kkyeujhko, v => ycl);
  v : entity work.uiwh
    port map (ewlxwmfc => zcs, i => lfqube);
  ewjw : entity work.uiwh
    port map (ewlxwmfc => zcs, i => ugrjj);
  
  -- Single-driven assignments
  aq <= 344;
  ouljybo <= (others => 0.0);
  
  -- Multi-driven assignments
  eue <= eue;
  zcs <= zcs;
  eue <= eue;
end y;



-- Seed after: 4965914226317911648,4404421571376382767
