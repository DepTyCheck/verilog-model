-- Seed: 16188161992726277074,14094562573555574003

entity frnekle is
  port (kgjoywb : linkage time_vector(4 downto 4); jlxqevjuky : out severity_level; my : in time_vector(1 to 0); kdqahnj : in bit);
end frnekle;

architecture fgptkucp of frnekle is
  
begin
  -- Single-driven assignments
  jlxqevjuky <= jlxqevjuky;
end fgptkucp;

library ieee;
use ieee.std_logic_1164.all;

entity zvlfgexrgy is
  port (bmhpmpbafw : in std_logic_vector(0 to 3); ibhtxjjjxk : out time; uyt : buffer std_logic);
end zvlfgexrgy;

architecture nwkxurpuk of zvlfgexrgy is
  signal agby : bit;
  signal eot : severity_level;
  signal raf : time_vector(4 downto 4);
  signal bzdz : bit;
  signal zdfieuub : time_vector(1 to 0);
  signal grhvk : severity_level;
  signal zwcoymbmvk : time_vector(4 downto 4);
  signal s : bit;
  signal fhtcs : time_vector(1 to 0);
  signal ekeezvxw : severity_level;
  signal psylbalh : time_vector(4 downto 4);
begin
  hnbed : entity work.frnekle
    port map (kgjoywb => psylbalh, jlxqevjuky => ekeezvxw, my => fhtcs, kdqahnj => s);
  otutskwbq : entity work.frnekle
    port map (kgjoywb => zwcoymbmvk, jlxqevjuky => grhvk, my => zdfieuub, kdqahnj => bzdz);
  coay : entity work.frnekle
    port map (kgjoywb => raf, jlxqevjuky => eot, my => zdfieuub, kdqahnj => agby);
  
  -- Single-driven assignments
  fhtcs <= fhtcs;
  zdfieuub <= fhtcs;
  ibhtxjjjxk <= ibhtxjjjxk;
  s <= s;
  
  -- Multi-driven assignments
  uyt <= 'H';
end nwkxurpuk;

entity eq is
  port (tohgybn : out real_vector(0 downto 1));
end eq;

library ieee;
use ieee.std_logic_1164.all;

architecture uw of eq is
  signal wv : std_logic;
  signal nybwg : time;
  signal z : std_logic_vector(0 to 3);
begin
  dp : entity work.zvlfgexrgy
    port map (bmhpmpbafw => z, ibhtxjjjxk => nybwg, uyt => wv);
  
  -- Single-driven assignments
  tohgybn <= tohgybn;
  
  -- Multi-driven assignments
  wv <= wv;
end uw;

library ieee;
use ieee.std_logic_1164.all;

entity qfg is
  port (txomqjn : inout std_logic_vector(0 downto 4));
end qfg;

architecture vny of qfg is
  signal gxulcgafn : real_vector(0 downto 1);
  signal xrhmveduer : severity_level;
  signal rl : time_vector(4 downto 4);
  signal b : bit;
  signal fedfwf : time_vector(1 to 0);
  signal bkqecm : severity_level;
  signal egcdvrx : time_vector(4 downto 4);
  signal uacjvek : real_vector(0 downto 1);
begin
  sbrxa : entity work.eq
    port map (tohgybn => uacjvek);
  lmtc : entity work.frnekle
    port map (kgjoywb => egcdvrx, jlxqevjuky => bkqecm, my => fedfwf, kdqahnj => b);
  wszqdosay : entity work.frnekle
    port map (kgjoywb => rl, jlxqevjuky => xrhmveduer, my => fedfwf, kdqahnj => b);
  zn : entity work.eq
    port map (tohgybn => gxulcgafn);
  
  -- Multi-driven assignments
  txomqjn <= (others => '0');
end vny;



-- Seed after: 12278669217194064746,14094562573555574003
