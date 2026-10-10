-- Seed: 2360747742949187501,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity xforyom is
  port (lo : out std_logic_vector(4 to 2); roccfwmvqn : out time; bqssmbmzgr : buffer std_logic; qpxqrio : inout bit);
end xforyom;

architecture t of xforyom is
  
begin
  -- Single-driven assignments
  qpxqrio <= '0';
  roccfwmvqn <= 1_0_4_0.2_3 ns;
end t;

library ieee;
use ieee.std_logic_1164.all;

entity nuzqbfe is
  port (rcfxlexgbe : linkage std_logic_vector(2 to 2); tpfqpjbx : out integer_vector(1 to 1));
end nuzqbfe;

library ieee;
use ieee.std_logic_1164.all;

architecture lb of nuzqbfe is
  signal iytxmsdumu : bit;
  signal z : time;
  signal biejkcsjtl : bit;
  signal ewek : std_logic;
  signal dvpqbtck : time;
  signal cuylb : std_logic_vector(4 to 2);
begin
  wxdckebcr : entity work.xforyom
    port map (lo => cuylb, roccfwmvqn => dvpqbtck, bqssmbmzgr => ewek, qpxqrio => biejkcsjtl);
  gr : entity work.xforyom
    port map (lo => cuylb, roccfwmvqn => z, bqssmbmzgr => ewek, qpxqrio => iytxmsdumu);
  
  -- Single-driven assignments
  tpfqpjbx <= (others => 43220);
  
  -- Multi-driven assignments
  ewek <= 'U';
  cuylb <= cuylb;
end lb;



-- Seed after: 1206066742376427428,511364357853360275
