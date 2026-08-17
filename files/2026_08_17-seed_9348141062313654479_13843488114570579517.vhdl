-- Seed: 9348141062313654479,13843488114570579517

library ieee;
use ieee.std_logic_1164.all;

entity lrcjxrm is
  port (oujjqwp : linkage severity_level; ynfhubehzm : linkage std_logic; ih : in boolean_vector(4 to 0));
end lrcjxrm;

architecture x of lrcjxrm is
  
begin
  
end x;

entity fteihpc is
  port (mlxfo : in real; bacydmttnp : linkage time; xrzhkrfb : linkage time; mhc : buffer time);
end fteihpc;

library ieee;
use ieee.std_logic_1164.all;

architecture ofsoioaca of fteihpc is
  signal xxso : boolean_vector(4 to 0);
  signal glozuqwut : std_logic;
  signal qnl : severity_level;
  signal wak : std_logic;
  signal fg : severity_level;
  signal sszpoe : boolean_vector(4 to 0);
  signal kplpiw : std_logic;
  signal jhc : severity_level;
begin
  bpvlixjpg : entity work.lrcjxrm
    port map (oujjqwp => jhc, ynfhubehzm => kplpiw, ih => sszpoe);
  ntvju : entity work.lrcjxrm
    port map (oujjqwp => fg, ynfhubehzm => wak, ih => sszpoe);
  ohjtxghafc : entity work.lrcjxrm
    port map (oujjqwp => qnl, ynfhubehzm => glozuqwut, ih => xxso);
  
  -- Single-driven assignments
  xxso <= sszpoe;
  sszpoe <= sszpoe;
  mhc <= mhc;
  
  -- Multi-driven assignments
  kplpiw <= '1';
end ofsoioaca;



-- Seed after: 10571388728135338336,13843488114570579517
