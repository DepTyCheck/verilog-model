-- Seed: 6767090993591681014,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity zgb is
  port (iot : buffer std_logic_vector(4 downto 2); vqfkpxz : inout string(1 to 4); e : in integer);
end zgb;

architecture ypdevmojfp of zgb is
  
begin
  -- Single-driven assignments
  vqfkpxz <= "iten";
  
  -- Multi-driven assignments
  iot <= "L1-";
  iot <= iot;
end ypdevmojfp;

entity jar is
  port (gtuwiucegu : inout real; etrgumra : linkage character; tbeejrph : out real_vector(1 downto 1); ymqpmyyu : buffer integer);
end jar;

library ieee;
use ieee.std_logic_1164.all;

architecture fusa of jar is
  signal bbkljiain : integer;
  signal nzuzwtiz : string(1 to 4);
  signal ulub : integer;
  signal v : string(1 to 4);
  signal fceobj : std_logic_vector(4 downto 2);
  signal huvfuxaka : string(1 to 4);
  signal xzagfs : std_logic_vector(4 downto 2);
begin
  pazlvfd : entity work.zgb
    port map (iot => xzagfs, vqfkpxz => huvfuxaka, e => ymqpmyyu);
  yznty : entity work.zgb
    port map (iot => fceobj, vqfkpxz => v, e => ulub);
  itmz : entity work.zgb
    port map (iot => xzagfs, vqfkpxz => nzuzwtiz, e => bbkljiain);
  
  -- Single-driven assignments
  tbeejrph <= tbeejrph;
  bbkljiain <= ymqpmyyu;
  
  -- Multi-driven assignments
  xzagfs <= ('U', '0', '-');
  xzagfs <= xzagfs;
end fusa;



-- Seed after: 11292956670144191061,3426943171337919827
