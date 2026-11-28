-- Seed: 17235178497885252422,15306146742274979323

library ieee;
use ieee.std_logic_1164.all;

entity lv is
  port (wjrxcizs : in std_logic; hjgzyvxgx : buffer std_logic);
end lv;

architecture ccaj of lv is
  
begin
  -- Multi-driven assignments
  hjgzyvxgx <= hjgzyvxgx;
  hjgzyvxgx <= 'W';
  hjgzyvxgx <= '-';
  hjgzyvxgx <= '-';
end ccaj;

entity zugbru is
  port (wngnzj : inout integer);
end zugbru;

library ieee;
use ieee.std_logic_1164.all;

architecture ocjr of zugbru is
  signal pxgqp : std_logic;
  signal ujekr : std_logic;
  signal ieeiaztewa : std_logic;
  signal bbbuq : std_logic;
  signal gaakcppak : std_logic;
begin
  c : entity work.lv
    port map (wjrxcizs => gaakcppak, hjgzyvxgx => bbbuq);
  fnidjeojr : entity work.lv
    port map (wjrxcizs => bbbuq, hjgzyvxgx => gaakcppak);
  qrerirub : entity work.lv
    port map (wjrxcizs => ieeiaztewa, hjgzyvxgx => ujekr);
  oxfhkj : entity work.lv
    port map (wjrxcizs => pxgqp, hjgzyvxgx => gaakcppak);
  
  -- Single-driven assignments
  wngnzj <= wngnzj;
end ocjr;

library ieee;
use ieee.std_logic_1164.all;

entity hihryn is
  port (qpypzpwkpj : inout time; hghs : buffer std_logic_vector(2 to 0); nbaqmh : buffer real_vector(4 to 1); cjiax : in real_vector(4 downto 3));
end hihryn;

library ieee;
use ieee.std_logic_1164.all;

architecture bokclpon of hihryn is
  signal ijtvn : std_logic;
  signal okssewzrdb : std_logic;
  signal wnq : std_logic;
begin
  jlvyclgyd : entity work.lv
    port map (wjrxcizs => wnq, hjgzyvxgx => wnq);
  jts : entity work.lv
    port map (wjrxcizs => okssewzrdb, hjgzyvxgx => ijtvn);
  
  -- Single-driven assignments
  qpypzpwkpj <= qpypzpwkpj;
  nbaqmh <= nbaqmh;
  
  -- Multi-driven assignments
  okssewzrdb <= wnq;
  okssewzrdb <= 'W';
  hghs <= "";
end bokclpon;

entity prgayuahn is
  port (xphr : linkage integer);
end prgayuahn;

library ieee;
use ieee.std_logic_1164.all;

architecture j of prgayuahn is
  signal ymloi : std_logic;
  signal nvfg : std_logic;
  signal caeaivmr : integer;
  signal z : std_logic;
  signal iim : std_logic;
begin
  baqonfwjo : entity work.lv
    port map (wjrxcizs => iim, hjgzyvxgx => iim);
  bema : entity work.lv
    port map (wjrxcizs => iim, hjgzyvxgx => z);
  bewfy : entity work.zugbru
    port map (wngnzj => caeaivmr);
  yzubtqre : entity work.lv
    port map (wjrxcizs => nvfg, hjgzyvxgx => ymloi);
end j;



-- Seed after: 10538394787601610299,15306146742274979323
