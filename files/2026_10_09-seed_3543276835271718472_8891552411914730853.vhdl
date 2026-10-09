-- Seed: 3543276835271718472,8891552411914730853

entity soxaxug is
  port (bwl : in integer; xcbz : buffer bit_vector(4 to 1));
end soxaxug;

architecture bgbie of soxaxug is
  
begin
  -- Single-driven assignments
  xcbz <= (others => '0');
end bgbie;

library ieee;
use ieee.std_logic_1164.all;

entity bl is
  port (vxjonmxyar : inout std_logic_vector(3 to 1); b : out bit; edblx : in time);
end bl;

architecture ojf of bl is
  signal gmvofo : bit_vector(4 to 1);
  signal hdbumlgs : integer;
  signal xcilz : bit_vector(4 to 1);
  signal si : bit_vector(4 to 1);
  signal jfyxzfhce : integer;
begin
  vmhtx : entity work.soxaxug
    port map (bwl => jfyxzfhce, xcbz => si);
  ygitoz : entity work.soxaxug
    port map (bwl => jfyxzfhce, xcbz => xcilz);
  gukxgq : entity work.soxaxug
    port map (bwl => hdbumlgs, xcbz => gmvofo);
  
  -- Single-driven assignments
  b <= '1';
  hdbumlgs <= 204;
  jfyxzfhce <= jfyxzfhce;
  
  -- Multi-driven assignments
  vxjonmxyar <= vxjonmxyar;
  vxjonmxyar <= (others => '0');
  vxjonmxyar <= "";
end ojf;

library ieee;
use ieee.std_logic_1164.all;

entity msofyy is
  port (heq : out std_logic_vector(1 downto 2); zkztsgsuky : inout std_logic);
end msofyy;

architecture hmjyls of msofyy is
  
begin
  -- Multi-driven assignments
  zkztsgsuky <= zkztsgsuky;
  zkztsgsuky <= 'L';
  heq <= "";
  zkztsgsuky <= zkztsgsuky;
end hmjyls;



-- Seed after: 5610338594222089370,8891552411914730853
