-- Seed: 5188893034042407974,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity lpgip is
  port (huy : buffer std_logic_vector(0 to 4); idsa : inout std_logic);
end lpgip;

architecture eaeljlv of lpgip is
  
begin
  -- Multi-driven assignments
  idsa <= 'Z';
  idsa <= 'L';
  huy <= ('U', 'U', 'U', 'L', '1');
end eaeljlv;

entity qdxudyey is
  port (aj : in real);
end qdxudyey;

library ieee;
use ieee.std_logic_1164.all;

architecture j of qdxudyey is
  signal krzlry : std_logic;
  signal idb : std_logic_vector(0 to 4);
  signal utrnmvye : std_logic;
  signal fxbgre : std_logic;
  signal xgyz : std_logic_vector(0 to 4);
begin
  tievif : entity work.lpgip
    port map (huy => xgyz, idsa => fxbgre);
  xy : entity work.lpgip
    port map (huy => xgyz, idsa => utrnmvye);
  tjnwnxf : entity work.lpgip
    port map (huy => xgyz, idsa => utrnmvye);
  coe : entity work.lpgip
    port map (huy => idb, idsa => krzlry);
  
  -- Multi-driven assignments
  xgyz <= "-1-X-";
  xgyz <= ('0', '-', '-', 'W', '1');
end j;

entity fvuia is
  port (rcm : out real; owihpi : in boolean_vector(1 to 0));
end fvuia;

architecture jtbklyv of fvuia is
  
begin
  -- Single-driven assignments
  rcm <= 2.3_3;
end jtbklyv;

entity xruvcdk is
  port (fzc : linkage severity_level; pnio : in boolean; p : inout time; sjhd : inout integer_vector(0 downto 1));
end xruvcdk;

architecture piptw of xruvcdk is
  
begin
  -- Single-driven assignments
  p <= 16#F45# ns;
  sjhd <= (others => 0);
end piptw;



-- Seed after: 16312165787407219155,6697892553037813751
