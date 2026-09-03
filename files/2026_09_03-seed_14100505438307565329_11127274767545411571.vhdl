-- Seed: 14100505438307565329,11127274767545411571

library ieee;
use ieee.std_logic_1164.all;

entity klmi is
  port (lzunkx : buffer std_logic; uncx : out real);
end klmi;

architecture fvvh of klmi is
  
begin
  
end fvvh;

entity ltam is
  port (fwdpsww : in bit_vector(1 downto 0));
end ltam;

library ieee;
use ieee.std_logic_1164.all;

architecture vgprdsz of ltam is
  signal ganbnkoani : real;
  signal uqbevaof : std_logic;
  signal sznpjdnzzp : real;
  signal eqjjj : std_logic;
  signal grrztzsyrr : real;
  signal bcjpxc : real;
  signal d : std_logic;
begin
  k : entity work.klmi
    port map (lzunkx => d, uncx => bcjpxc);
  utvvxk : entity work.klmi
    port map (lzunkx => d, uncx => grrztzsyrr);
  s : entity work.klmi
    port map (lzunkx => eqjjj, uncx => sznpjdnzzp);
  ud : entity work.klmi
    port map (lzunkx => uqbevaof, uncx => ganbnkoani);
end vgprdsz;

entity csstxbqkoj is
  port (cwmnwd : linkage real; qtuaxd : in bit_vector(2 to 0); unifbqzdw : out real; wzqdmmenoi : linkage boolean);
end csstxbqkoj;

library ieee;
use ieee.std_logic_1164.all;

architecture ohpr of csstxbqkoj is
  signal fritw : real;
  signal wxzihio : real;
  signal izvqmmmjz : std_logic;
  signal h : bit_vector(1 downto 0);
begin
  nfugb : entity work.ltam
    port map (fwdpsww => h);
  ckhzqxoha : entity work.klmi
    port map (lzunkx => izvqmmmjz, uncx => wxzihio);
  rkoteykld : entity work.klmi
    port map (lzunkx => izvqmmmjz, uncx => fritw);
  
  -- Single-driven assignments
  unifbqzdw <= 2#00.0_1_1_1_1#;
  h <= ('1', '1');
  
  -- Multi-driven assignments
  izvqmmmjz <= izvqmmmjz;
  izvqmmmjz <= izvqmmmjz;
  izvqmmmjz <= izvqmmmjz;
end ohpr;



-- Seed after: 4509618825360653143,11127274767545411571
