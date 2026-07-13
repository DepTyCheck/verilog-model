-- Seed: 14342807750818132408,3566912872917928779

library ieee;
use ieee.std_logic_1164.all;

entity dfwlgwfdt is
  port (adbas : linkage character; c : inout real; rdrsvzkwpe : linkage std_logic_vector(0 downto 4); qz : linkage std_logic);
end dfwlgwfdt;

architecture foaxevzj of dfwlgwfdt is
  
begin
  -- Single-driven assignments
  c <= c;
end foaxevzj;

use std.reflection.all;

entity gnwaa is
  port (uwmupf : inout floating_value_mirror);
end gnwaa;

library ieee;
use ieee.std_logic_1164.all;

architecture hakjvcl of gnwaa is
  signal uzroekedyw : std_logic;
  signal bheerbat : std_logic_vector(0 downto 4);
  signal tmc : real;
  signal u : character;
  signal tdcg : std_logic;
  signal rliarz : std_logic_vector(0 downto 4);
  signal gfrfxzmo : real;
  signal vckwr : character;
  signal x : std_logic;
  signal be : std_logic_vector(0 downto 4);
  signal nzistxgdlw : real;
  signal q : character;
begin
  pkwacdiyyq : entity work.dfwlgwfdt
    port map (adbas => q, c => nzistxgdlw, rdrsvzkwpe => be, qz => x);
  mtdmhhx : entity work.dfwlgwfdt
    port map (adbas => vckwr, c => gfrfxzmo, rdrsvzkwpe => rliarz, qz => tdcg);
  xd : entity work.dfwlgwfdt
    port map (adbas => u, c => tmc, rdrsvzkwpe => bheerbat, qz => uzroekedyw);
  
  -- Multi-driven assignments
  bheerbat <= be;
  be <= (others => '0');
  uzroekedyw <= 'X';
end hakjvcl;



-- Seed after: 16839931972610854413,3566912872917928779
