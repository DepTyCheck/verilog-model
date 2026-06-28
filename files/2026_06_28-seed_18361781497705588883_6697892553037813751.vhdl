-- Seed: 18361781497705588883,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity cmwgoijcax is
  port (ifpgxuusdh : inout time; ea : in real; neikr : linkage std_logic_vector(4 to 0); myvlzuam : buffer integer);
end cmwgoijcax;

architecture u of cmwgoijcax is
  
begin
  -- Single-driven assignments
  myvlzuam <= 4;
  ifpgxuusdh <= 1_4.3 fs;
end u;

library ieee;
use ieee.std_logic_1164.all;

entity ce is
  port (hbmiljzozc : out time; jfvkfac : linkage std_logic_vector(4 to 3); pyq : out real; upikst : inout real_vector(3 downto 2));
end ce;

architecture jo of ce is
  
begin
  -- Single-driven assignments
  hbmiljzozc <= 32 ps;
  upikst <= (0.03, 3.2_1_2_4);
  pyq <= 16#0_3.A_E_8#;
end jo;

library ieee;
use ieee.std_logic_1164.all;

entity eiodwq is
  port (vdzol : out real; xb : in std_logic_vector(1 to 2));
end eiodwq;

library ieee;
use ieee.std_logic_1164.all;

architecture xzms of eiodwq is
  signal dshg : integer;
  signal ici : time;
  signal uquixifhzq : integer;
  signal ncme : std_logic_vector(4 to 0);
  signal reqnl : time;
begin
  mpx : entity work.cmwgoijcax
    port map (ifpgxuusdh => reqnl, ea => vdzol, neikr => ncme, myvlzuam => uquixifhzq);
  oe : entity work.cmwgoijcax
    port map (ifpgxuusdh => ici, ea => vdzol, neikr => ncme, myvlzuam => dshg);
  
  -- Single-driven assignments
  vdzol <= 8#0_1_7.1#;
end xzms;

entity febbx is
  port (uyhslxtmo : out string(4 downto 1); hipeqo : linkage time; xcnr : linkage string(2 downto 3));
end febbx;

library ieee;
use ieee.std_logic_1164.all;

architecture oxwvuxgw of febbx is
  signal muedozdo : integer;
  signal vhnu : time;
  signal pbcwaopu : real_vector(3 downto 2);
  signal ikzd : real;
  signal lzwkvu : time;
  signal qi : real_vector(3 downto 2);
  signal kdggdzx : real;
  signal azkjisfhs : std_logic_vector(4 to 3);
  signal qlmnjsjtq : time;
  signal udll : real_vector(3 downto 2);
  signal cwqauygm : real;
  signal mcwdoenuw : std_logic_vector(4 to 0);
  signal i : time;
begin
  c : entity work.ce
    port map (hbmiljzozc => i, jfvkfac => mcwdoenuw, pyq => cwqauygm, upikst => udll);
  bxedeuuk : entity work.ce
    port map (hbmiljzozc => qlmnjsjtq, jfvkfac => azkjisfhs, pyq => kdggdzx, upikst => qi);
  arwzmvjog : entity work.ce
    port map (hbmiljzozc => lzwkvu, jfvkfac => mcwdoenuw, pyq => ikzd, upikst => pbcwaopu);
  dbq : entity work.cmwgoijcax
    port map (ifpgxuusdh => vhnu, ea => kdggdzx, neikr => mcwdoenuw, myvlzuam => muedozdo);
  
  -- Single-driven assignments
  uyhslxtmo <= ('m', 'z', 'm', 'w');
  
  -- Multi-driven assignments
  mcwdoenuw <= "";
end oxwvuxgw;



-- Seed after: 11899184017842623483,6697892553037813751
