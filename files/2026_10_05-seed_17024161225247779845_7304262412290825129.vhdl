-- Seed: 17024161225247779845,7304262412290825129

library ieee;
use ieee.std_logic_1164.all;

entity xro is
  port (oag : linkage std_logic; qahqd : inout time_vector(1 downto 2); jb : in time_vector(4 downto 3));
end xro;

architecture djbn of xro is
  
begin
  
end djbn;

library ieee;
use ieee.std_logic_1164.all;

entity vgiwvcp is
  port (hfks : in std_logic_vector(2 to 1); lvyxi : linkage integer; rkxjt : inout integer_vector(1 to 1));
end vgiwvcp;

library ieee;
use ieee.std_logic_1164.all;

architecture dwabkxdk of vgiwvcp is
  signal b : time_vector(1 downto 2);
  signal zytgk : time_vector(1 downto 2);
  signal hfsdqp : time_vector(4 downto 3);
  signal nryt : time_vector(1 downto 2);
  signal tzcispft : std_logic;
begin
  cdnvm : entity work.xro
    port map (oag => tzcispft, qahqd => nryt, jb => hfsdqp);
  vyewp : entity work.xro
    port map (oag => tzcispft, qahqd => zytgk, jb => hfsdqp);
  zk : entity work.xro
    port map (oag => tzcispft, qahqd => b, jb => hfsdqp);
  
  -- Single-driven assignments
  rkxjt <= (others => 16#2C#);
  
  -- Multi-driven assignments
  tzcispft <= '1';
  tzcispft <= tzcispft;
  tzcispft <= tzcispft;
end dwabkxdk;



-- Seed after: 7916889227318868392,7304262412290825129
