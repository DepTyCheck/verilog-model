-- Seed: 11479387442520758082,3687118713772291287

entity uavaplpm is
  port (zrxxowl : inout real; jq : buffer time);
end uavaplpm;

architecture bvnron of uavaplpm is
  
begin
  -- Single-driven assignments
  jq <= 22 us;
end bvnron;

entity j is
  port (dlgabpalay : inout real; vhuqn : inout time);
end j;

architecture yvk of j is
  signal r : time;
begin
  reqimmtc : entity work.uavaplpm
    port map (zrxxowl => dlgabpalay, jq => r);
  
  -- Single-driven assignments
  vhuqn <= 1 hr;
end yvk;

library ieee;
use ieee.std_logic_1164.all;

entity noobn is
  port (pbar : buffer std_logic_vector(2 downto 2); ywwf : out std_logic; eukawloli : out time);
end noobn;

architecture jeqgatsn of noobn is
  signal hi : real;
  signal twwcpzwr : time;
  signal aqtb : real;
  signal fnhtjlcdv : time;
  signal usvyj : real;
begin
  flx : entity work.uavaplpm
    port map (zrxxowl => usvyj, jq => fnhtjlcdv);
  dxhmk : entity work.uavaplpm
    port map (zrxxowl => aqtb, jq => twwcpzwr);
  jhmvs : entity work.uavaplpm
    port map (zrxxowl => hi, jq => eukawloli);
  
  -- Multi-driven assignments
  pbar <= (others => 'U');
end jeqgatsn;



-- Seed after: 3692636667603108120,3687118713772291287
