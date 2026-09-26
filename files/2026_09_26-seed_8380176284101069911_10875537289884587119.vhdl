-- Seed: 8380176284101069911,10875537289884587119

entity tczkpwv is
  port (jhphjzzx : in time; b : in boolean_vector(4 downto 3); bs : inout boolean);
end tczkpwv;

architecture upxkwm of tczkpwv is
  
begin
  -- Single-driven assignments
  bs <= TRUE;
end upxkwm;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (gfk : out std_logic_vector(0 to 0); hekiroe : out integer; bffktbjh : out time);
end h;

architecture yoynfy of h is
  signal zqv : boolean;
  signal lko : boolean_vector(4 downto 3);
  signal akgseiha : boolean;
  signal tpqidgcwl : boolean_vector(4 downto 3);
  signal qvoaqt : time;
begin
  kdavjgwe : entity work.tczkpwv
    port map (jhphjzzx => qvoaqt, b => tpqidgcwl, bs => akgseiha);
  nkz : entity work.tczkpwv
    port map (jhphjzzx => qvoaqt, b => lko, bs => zqv);
  
  -- Single-driven assignments
  qvoaqt <= bffktbjh;
  lko <= lko;
  tpqidgcwl <= (TRUE, TRUE);
  hekiroe <= hekiroe;
  bffktbjh <= bffktbjh;
  
  -- Multi-driven assignments
  gfk <= gfk;
  gfk <= gfk;
  gfk <= gfk;
end yoynfy;



-- Seed after: 8724623588778566433,10875537289884587119
