-- Seed: 5742259578986692989,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity lwkbpu is
  port (yvywmniv : in std_logic_vector(1 downto 2); j : out std_logic);
end lwkbpu;

architecture cjtoisqrq of lwkbpu is
  
begin
  -- Multi-driven assignments
  j <= 'U';
  j <= 'X';
end cjtoisqrq;

entity qw is
  port (wlcmj : out bit);
end qw;

library ieee;
use ieee.std_logic_1164.all;

architecture hu of qw is
  signal ji : std_logic;
  signal di : std_logic;
  signal xcldr : std_logic_vector(1 downto 2);
begin
  ysqsdtpfu : entity work.lwkbpu
    port map (yvywmniv => xcldr, j => di);
  dnfoalowzo : entity work.lwkbpu
    port map (yvywmniv => xcldr, j => di);
  pyptextedq : entity work.lwkbpu
    port map (yvywmniv => xcldr, j => ji);
  
  -- Single-driven assignments
  wlcmj <= '1';
  
  -- Multi-driven assignments
  xcldr <= "";
  xcldr <= "";
  ji <= '1';
  di <= 'L';
end hu;



-- Seed after: 5459317875310259393,17047277710231705797
