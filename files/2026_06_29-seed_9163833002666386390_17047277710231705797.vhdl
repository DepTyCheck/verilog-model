-- Seed: 9163833002666386390,17047277710231705797

library ieee;
use ieee.std_logic_1164.all;

entity lxmt is
  port (bwcs : in time; ksxjeqqrp : buffer std_logic; yiyczqyq : linkage time; fmiqn : in string(3 to 5));
end lxmt;

architecture wdkyp of lxmt is
  
begin
  -- Multi-driven assignments
  ksxjeqqrp <= 'U';
end wdkyp;

library ieee;
use ieee.std_logic_1164.all;

entity mcfmibqkun is
  port (gkcrhylnt : buffer real_vector(2 downto 4); vriybywjjt : linkage std_logic);
end mcfmibqkun;

architecture edxgbdrwt of mcfmibqkun is
  
begin
  -- Single-driven assignments
  gkcrhylnt <= (others => 0.0);
end edxgbdrwt;

entity ydcwdqdf is
  port (qgbpre : in integer);
end ydcwdqdf;

library ieee;
use ieee.std_logic_1164.all;

architecture odmnt of ydcwdqdf is
  signal hgkkeblwa : time;
  signal w : std_logic;
  signal yw : real_vector(2 downto 4);
  signal e : string(3 to 5);
  signal v : time;
  signal uwaunqaebd : time;
  signal jo : string(3 to 5);
  signal symoulcxpl : time;
  signal l : std_logic;
  signal nx : time;
begin
  d : entity work.lxmt
    port map (bwcs => nx, ksxjeqqrp => l, yiyczqyq => symoulcxpl, fmiqn => jo);
  yzjegk : entity work.lxmt
    port map (bwcs => uwaunqaebd, ksxjeqqrp => l, yiyczqyq => v, fmiqn => e);
  lump : entity work.mcfmibqkun
    port map (gkcrhylnt => yw, vriybywjjt => w);
  wpxtwuqli : entity work.lxmt
    port map (bwcs => hgkkeblwa, ksxjeqqrp => l, yiyczqyq => nx, fmiqn => jo);
  
  -- Single-driven assignments
  e <= ('a', 'n', 'x');
  jo <= ('s', 'v', 'a');
  uwaunqaebd <= 1_4_3 ms;
  
  -- Multi-driven assignments
  l <= 'L';
  l <= 'X';
  l <= 'Z';
  l <= '0';
end odmnt;



-- Seed after: 6911050309407813494,17047277710231705797
