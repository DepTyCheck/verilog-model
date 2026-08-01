-- Seed: 8222103774359432086,4292249356257567981

library ieee;
use ieee.std_logic_1164.all;

entity hmsusq is
  port (egertie : in std_logic_vector(1 to 3); ksbv : out boolean; hdo : in real);
end hmsusq;

architecture ap of hmsusq is
  
begin
  -- Single-driven assignments
  ksbv <= TRUE;
end ap;

library ieee;
use ieee.std_logic_1164.all;

entity gtrzlaf is
  port (ycrxxhs : out time; hnqd : inout std_logic_vector(0 to 2); svkoyrqto : inout time; ruyccexvn : in character);
end gtrzlaf;

library ieee;
use ieee.std_logic_1164.all;

architecture igbhq of gtrzlaf is
  signal y : boolean;
  signal c : std_logic_vector(1 to 3);
  signal uepeav : boolean;
  signal ehntmga : std_logic_vector(1 to 3);
  signal ak : boolean;
  signal qf : real;
  signal zllphty : boolean;
begin
  eowikyrpg : entity work.hmsusq
    port map (egertie => hnqd, ksbv => zllphty, hdo => qf);
  neapdk : entity work.hmsusq
    port map (egertie => hnqd, ksbv => ak, hdo => qf);
  qy : entity work.hmsusq
    port map (egertie => ehntmga, ksbv => uepeav, hdo => qf);
  kdduncq : entity work.hmsusq
    port map (egertie => c, ksbv => y, hdo => qf);
  
  -- Single-driven assignments
  svkoyrqto <= svkoyrqto;
  ycrxxhs <= svkoyrqto;
  
  -- Multi-driven assignments
  hnqd <= hnqd;
end igbhq;



-- Seed after: 1874798785139025669,4292249356257567981
