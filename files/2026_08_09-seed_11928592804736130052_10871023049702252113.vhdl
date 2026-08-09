-- Seed: 11928592804736130052,10871023049702252113

library ieee;
use ieee.std_logic_1164.all;

entity md is
  port (ctxhzmg : buffer integer; u : in std_logic_vector(4 downto 2));
end md;

architecture pmeo of md is
  
begin
  -- Single-driven assignments
  ctxhzmg <= ctxhzmg;
end pmeo;

entity ee is
  port (v : out time_vector(0 downto 2); wogdh : inout bit);
end ee;

library ieee;
use ieee.std_logic_1164.all;

architecture pjpgjqg of ee is
  signal obytfmxgpi : integer;
  signal nbbmaux : integer;
  signal mcoikuynou : std_logic_vector(4 downto 2);
  signal lxa : integer;
  signal ym : std_logic_vector(4 downto 2);
  signal hwnu : integer;
begin
  znwhw : entity work.md
    port map (ctxhzmg => hwnu, u => ym);
  raeeae : entity work.md
    port map (ctxhzmg => lxa, u => mcoikuynou);
  akjvqmfbp : entity work.md
    port map (ctxhzmg => nbbmaux, u => ym);
  d : entity work.md
    port map (ctxhzmg => obytfmxgpi, u => ym);
  
  -- Single-driven assignments
  v <= (others => 0 ns);
  wogdh <= wogdh;
  
  -- Multi-driven assignments
  mcoikuynou <= ym;
  ym <= "0HX";
end pjpgjqg;



-- Seed after: 873714508788514851,10871023049702252113
