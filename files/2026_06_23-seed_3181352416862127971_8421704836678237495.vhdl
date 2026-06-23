-- Seed: 3181352416862127971,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (r : out std_logic; tbtwoz : out integer; o : inout std_logic);
end g;

architecture jrnvyg of g is
  
begin
  -- Single-driven assignments
  tbtwoz <= 8#2_2_2#;
  
  -- Multi-driven assignments
  o <= 'L';
end jrnvyg;

entity yuhii is
  port (n : buffer integer);
end yuhii;

library ieee;
use ieee.std_logic_1164.all;

architecture sfpu of yuhii is
  signal lrpkf : integer;
  signal dlt : std_logic;
  signal ylleeqvr : std_logic;
begin
  p : entity work.g
    port map (r => ylleeqvr, tbtwoz => n, o => dlt);
  hskyzyspzr : entity work.g
    port map (r => ylleeqvr, tbtwoz => lrpkf, o => dlt);
  
  -- Multi-driven assignments
  ylleeqvr <= 'H';
  ylleeqvr <= '-';
  ylleeqvr <= '0';
end sfpu;

library ieee;
use ieee.std_logic_1164.all;

entity xapzuw is
  port (hwfhkxqzdw : buffer std_logic_vector(2 downto 3); ibrmgiteu : out integer; tz : in integer; kklj : out real);
end xapzuw;

library ieee;
use ieee.std_logic_1164.all;

architecture cnoy of xapzuw is
  signal vkaiadxzcy : std_logic;
begin
  ruaabvreun : entity work.g
    port map (r => vkaiadxzcy, tbtwoz => ibrmgiteu, o => vkaiadxzcy);
  
  -- Single-driven assignments
  kklj <= 0031.0;
  
  -- Multi-driven assignments
  vkaiadxzcy <= 'U';
  vkaiadxzcy <= 'Z';
  hwfhkxqzdw <= (others => '0');
end cnoy;

entity s is
  port (ubd : in boolean);
end s;

architecture buptsbsbn of s is
  
begin
  
end buptsbsbn;



-- Seed after: 13381626527411819521,8421704836678237495
