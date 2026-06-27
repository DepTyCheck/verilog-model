-- Seed: 5471611511222827339,4860866131898729603

library ieee;
use ieee.std_logic_1164.all;

entity odbdumx is
  port (pidr : in std_logic; kzatz : linkage std_logic; rcndlcm : linkage real; gm : inout time);
end odbdumx;

architecture nvg of odbdumx is
  
begin
  -- Single-driven assignments
  gm <= 3 min;
end nvg;

entity e is
  port (w : in bit; i : inout time; pelde : buffer real);
end e;

library ieee;
use ieee.std_logic_1164.all;

architecture a of e is
  signal jmbqzsxyak : time;
  signal gtqcrtwo : std_logic;
  signal jlxyw : real;
  signal ijya : std_logic;
  signal qgoljcdqu : std_logic;
begin
  c : entity work.odbdumx
    port map (pidr => qgoljcdqu, kzatz => ijya, rcndlcm => jlxyw, gm => i);
  oqfuybn : entity work.odbdumx
    port map (pidr => ijya, kzatz => gtqcrtwo, rcndlcm => pelde, gm => jmbqzsxyak);
end a;

entity xgch is
  port (gqzd : buffer real_vector(2 downto 4));
end xgch;

library ieee;
use ieee.std_logic_1164.all;

architecture dgzjhdfj of xgch is
  signal lcqollutcn : time;
  signal wvthshmvou : real;
  signal ajtmyk : std_logic;
  signal yer : time;
  signal loen : real;
  signal fzfk : std_logic;
  signal pgrxx : std_logic;
  signal qsnexk : real;
  signal cpzmga : time;
  signal rwglh : bit;
  signal thj : time;
  signal cfiohah : real;
  signal qsg : std_logic;
  signal w : std_logic;
begin
  jnjdfncmc : entity work.odbdumx
    port map (pidr => w, kzatz => qsg, rcndlcm => cfiohah, gm => thj);
  hhodurmo : entity work.e
    port map (w => rwglh, i => cpzmga, pelde => qsnexk);
  dkltrj : entity work.odbdumx
    port map (pidr => pgrxx, kzatz => fzfk, rcndlcm => loen, gm => yer);
  szjellmmc : entity work.odbdumx
    port map (pidr => fzfk, kzatz => ajtmyk, rcndlcm => wvthshmvou, gm => lcqollutcn);
  
  -- Multi-driven assignments
  w <= '1';
end dgzjhdfj;



-- Seed after: 40493203051058590,4860866131898729603
