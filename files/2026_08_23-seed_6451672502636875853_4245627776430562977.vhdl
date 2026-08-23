-- Seed: 6451672502636875853,4245627776430562977

entity gl is
  port (dgg : inout time);
end gl;

architecture gcud of gl is
  
begin
  -- Single-driven assignments
  dgg <= dgg;
end gcud;

library ieee;
use ieee.std_logic_1164.all;

entity cqjyz is
  port (efumpxkumg : out std_logic_vector(4 to 3); ky : inout time; b : linkage std_logic; aaifggc : linkage boolean);
end cqjyz;

architecture c of cqjyz is
  signal nzqfupv : time;
  signal xkcip : time;
  signal aknxo : time;
begin
  faqaq : entity work.gl
    port map (dgg => aknxo);
  hkbw : entity work.gl
    port map (dgg => xkcip);
  lmspfren : entity work.gl
    port map (dgg => ky);
  ry : entity work.gl
    port map (dgg => nzqfupv);
  
  -- Multi-driven assignments
  efumpxkumg <= (others => '0');
  efumpxkumg <= efumpxkumg;
  efumpxkumg <= "";
  efumpxkumg <= efumpxkumg;
end c;

entity zj is
  port (hjezsbjo : out time);
end zj;

library ieee;
use ieee.std_logic_1164.all;

architecture mijhxxql of zj is
  signal eex : boolean;
  signal vzgg : time;
  signal xuxa : time;
  signal qhc : boolean;
  signal uoxnpgedk : std_logic;
  signal qcphnoa : time;
  signal pmprit : std_logic_vector(4 to 3);
begin
  w : entity work.cqjyz
    port map (efumpxkumg => pmprit, ky => qcphnoa, b => uoxnpgedk, aaifggc => qhc);
  ywhlvkacmf : entity work.gl
    port map (dgg => xuxa);
  pe : entity work.gl
    port map (dgg => hjezsbjo);
  p : entity work.cqjyz
    port map (efumpxkumg => pmprit, ky => vzgg, b => uoxnpgedk, aaifggc => eex);
end mijhxxql;



-- Seed after: 15949131144303000792,4245627776430562977
