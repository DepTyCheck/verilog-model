-- Seed: 14013577313792330387,10594830431004325987

library ieee;
use ieee.std_logic_1164.all;

entity zizmjx is
  port (xvnavrrblk : inout boolean_vector(0 to 0); bwu : out std_logic_vector(3 downto 1); uvid : inout std_logic);
end zizmjx;

architecture zat of zizmjx is
  
begin
  -- Single-driven assignments
  xvnavrrblk <= (others => FALSE);
end zat;

library ieee;
use ieee.std_logic_1164.all;

entity mppiq is
  port (c : buffer std_logic);
end mppiq;

library ieee;
use ieee.std_logic_1164.all;

architecture ksduyr of mppiq is
  signal ue : std_logic_vector(3 downto 1);
  signal ahngrpg : boolean_vector(0 to 0);
  signal ow : boolean_vector(0 to 0);
  signal zftp : std_logic;
  signal ubrqwsrd : std_logic_vector(3 downto 1);
  signal rxe : boolean_vector(0 to 0);
begin
  s : entity work.zizmjx
    port map (xvnavrrblk => rxe, bwu => ubrqwsrd, uvid => zftp);
  qrevpxfi : entity work.zizmjx
    port map (xvnavrrblk => ow, bwu => ubrqwsrd, uvid => c);
  jks : entity work.zizmjx
    port map (xvnavrrblk => ahngrpg, bwu => ue, uvid => c);
  
  -- Multi-driven assignments
  zftp <= 'H';
end ksduyr;



-- Seed after: 8605237141261009318,10594830431004325987
