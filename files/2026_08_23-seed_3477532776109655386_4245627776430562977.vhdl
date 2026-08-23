-- Seed: 3477532776109655386,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity yxz is
  port (bx : in std_logic; lbi : linkage boolean; zvuj : out std_logic_vector(3 to 0); m : buffer std_logic_vector(4 to 3));
end yxz;

architecture gjjbgobidg of yxz is
  
begin
  -- Multi-driven assignments
  zvuj <= m;
  m <= (others => '0');
  m <= (others => '0');
  m <= m;
end gjjbgobidg;

library ieee;
use ieee.std_logic_1164.all;

entity ahxhrbjvn is
  port (dbkeeqy : in std_logic; xivxqj : linkage real_vector(1 to 3); jtrlbmal : in real; ukozeyau : buffer integer);
end ahxhrbjvn;

library ieee;
use ieee.std_logic_1164.all;

architecture fgglyvyow of ahxhrbjvn is
  signal rgcdeetpo : std_logic_vector(4 to 3);
  signal foresarnd : boolean;
  signal lcrzl : std_logic_vector(3 to 0);
  signal whhhffgu : boolean;
  signal eppofw : std_logic;
begin
  ebugkdcd : entity work.yxz
    port map (bx => eppofw, lbi => whhhffgu, zvuj => lcrzl, m => lcrzl);
  ighkwxpv : entity work.yxz
    port map (bx => dbkeeqy, lbi => foresarnd, zvuj => lcrzl, m => rgcdeetpo);
  
  -- Single-driven assignments
  ukozeyau <= 2#11110#;
  
  -- Multi-driven assignments
  eppofw <= dbkeeqy;
  rgcdeetpo <= "";
  eppofw <= dbkeeqy;
end fgglyvyow;

entity frdiegfynx is
  port (rta : inout time; n : buffer real);
end frdiegfynx;

library ieee;
use ieee.std_logic_1164.all;

architecture cmpfhibkco of frdiegfynx is
  signal sarvw : std_logic_vector(4 to 3);
  signal vgiarjnx : std_logic_vector(3 to 0);
  signal snsvp : boolean;
  signal rwdfvjfgqh : std_logic_vector(4 to 3);
  signal z : std_logic_vector(3 to 0);
  signal l : boolean;
  signal pxiqfh : std_logic;
  signal oasxug : integer;
  signal cpncekoa : real;
  signal gxmcwmr : real_vector(1 to 3);
  signal vax : integer;
  signal cidpo : real;
  signal cmiknvhjeb : real_vector(1 to 3);
  signal qioanmt : std_logic;
begin
  qvvwzirkkl : entity work.ahxhrbjvn
    port map (dbkeeqy => qioanmt, xivxqj => cmiknvhjeb, jtrlbmal => cidpo, ukozeyau => vax);
  pvz : entity work.ahxhrbjvn
    port map (dbkeeqy => qioanmt, xivxqj => gxmcwmr, jtrlbmal => cpncekoa, ukozeyau => oasxug);
  f : entity work.yxz
    port map (bx => pxiqfh, lbi => l, zvuj => z, m => rwdfvjfgqh);
  skujab : entity work.yxz
    port map (bx => qioanmt, lbi => snsvp, zvuj => vgiarjnx, m => sarvw);
  
  -- Single-driven assignments
  cidpo <= 16#D_D.1#;
  cpncekoa <= cpncekoa;
  
  -- Multi-driven assignments
  qioanmt <= 'H';
  z <= z;
  pxiqfh <= '-';
end cmpfhibkco;



-- Seed after: 7940251604111958138,4245627776430562977
