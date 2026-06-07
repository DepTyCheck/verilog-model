-- Seed: 5733774986809370044,7332793847894666635



entity wwbp is
  port (m : out severity_level; p : buffer real_vector(4 to 2));
end wwbp;



architecture j of wwbp is
  
begin
  
end j;



entity vsms is
  port (tf : buffer real; yhxssvmzrq : linkage time);
end vsms;



architecture enrpautkjy of vsms is
  signal gjt : real_vector(4 to 2);
  signal d : severity_level;
  signal v : real_vector(4 to 2);
  signal mwzjp : severity_level;
  signal rlqznmfd : real_vector(4 to 2);
  signal vpme : severity_level;
begin
  rjfqbkrk : entity work.wwbp
    port map (m => vpme, p => rlqznmfd);
  eoahyxr : entity work.wwbp
    port map (m => mwzjp, p => v);
  pjit : entity work.wwbp
    port map (m => d, p => gjt);
end enrpautkjy;

library ieee;
use ieee.std_logic_1164.all;

entity wy is
  port (tjcgxge : linkage time; zcbp : linkage integer; mbwfuduyv : linkage bit_vector(0 to 0); dzjogp : linkage std_logic_vector(1 downto 1));
end wy;



architecture gruvrv of wy is
  signal ekqvdvl : real;
  signal pp : real_vector(4 to 2);
  signal rlsy : severity_level;
  signal uvjwqcho : real_vector(4 to 2);
  signal vnydwpu : severity_level;
  signal ckozarkdo : real_vector(4 to 2);
  signal trmu : severity_level;
begin
  dsd : entity work.wwbp
    port map (m => trmu, p => ckozarkdo);
  sawyfvtv : entity work.wwbp
    port map (m => vnydwpu, p => uvjwqcho);
  akfnxzcb : entity work.wwbp
    port map (m => rlsy, p => pp);
  klh : entity work.vsms
    port map (tf => ekqvdvl, yhxssvmzrq => tjcgxge);
end gruvrv;

library ieee;
use ieee.std_logic_1164.all;

entity dqizxel is
  port (fur : inout real; oqhxkxo : linkage time; vlwrjxjqn : linkage std_logic_vector(0 to 4));
end dqizxel;

library ieee;
use ieee.std_logic_1164.all;

architecture mspooycyv of dqizxel is
  signal iwtrmx : std_logic_vector(1 downto 1);
  signal iklbbkepuj : bit_vector(0 to 0);
  signal uws : integer;
  signal luzutiq : time;
begin
  liz : entity work.wy
    port map (tjcgxge => luzutiq, zcbp => uws, mbwfuduyv => iklbbkepuj, dzjogp => iwtrmx);
end mspooycyv;



-- Seed after: 14300348573401462496,7332793847894666635
