-- Seed: 8941630631800985784,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity xboolghysc is
  port (iscnlouqjd : inout boolean; irt : buffer std_logic);
end xboolghysc;

architecture z of xboolghysc is
  
begin
  -- Multi-driven assignments
  irt <= irt;
  irt <= irt;
  irt <= 'Z';
  irt <= 'X';
end z;

library ieee;
use ieee.std_logic_1164.all;

entity hshm is
  port (nvykafr : in integer; itukg : inout time; mkn : buffer std_logic_vector(2 to 3));
end hshm;

library ieee;
use ieee.std_logic_1164.all;

architecture fy of hshm is
  signal mlnshesns : std_logic;
  signal xky : boolean;
  signal qsixinqhdx : boolean;
  signal gvmz : std_logic;
  signal elen : boolean;
begin
  ymtel : entity work.xboolghysc
    port map (iscnlouqjd => elen, irt => gvmz);
  u : entity work.xboolghysc
    port map (iscnlouqjd => qsixinqhdx, irt => gvmz);
  vmg : entity work.xboolghysc
    port map (iscnlouqjd => xky, irt => mlnshesns);
  
  -- Single-driven assignments
  itukg <= itukg;
  
  -- Multi-driven assignments
  mkn <= mkn;
  mkn <= ('H', 'Z');
  mkn <= mkn;
end fy;



-- Seed after: 8054848649932661860,7304262412290825129
