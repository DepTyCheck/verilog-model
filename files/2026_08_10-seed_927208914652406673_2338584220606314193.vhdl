-- Seed: 927208914652406673,2338584220606314193

entity ylzkrjqvo is
  port (slolg : in bit_vector(3 to 0); hmltful : in time_vector(0 downto 1); konmpbrc : buffer real; kk : buffer time);
end ylzkrjqvo;

architecture smnwifqi of ylzkrjqvo is
  
begin
  -- Single-driven assignments
  konmpbrc <= konmpbrc;
  kk <= 2#0# ms;
end smnwifqi;

library ieee;
use ieee.std_logic_1164.all;

entity tsyotv is
  port (clwgjm : out time; zlyqx : in std_logic);
end tsyotv;

architecture ptqynk of tsyotv is
  signal gvuylblnc : real;
  signal ikatzzdqyo : time_vector(0 downto 1);
  signal ko : bit_vector(3 to 0);
  signal logxrs : time;
  signal s : real;
  signal owptga : time_vector(0 downto 1);
  signal nfawykchs : bit_vector(3 to 0);
  signal jarqxu : time;
  signal qhewhuqkx : real;
  signal wrdzyrovp : time_vector(0 downto 1);
  signal ghcn : bit_vector(3 to 0);
  signal zhhtdsf : time;
  signal t : real;
  signal rwy : time_vector(0 downto 1);
  signal prldujes : bit_vector(3 to 0);
begin
  enmocv : entity work.ylzkrjqvo
    port map (slolg => prldujes, hmltful => rwy, konmpbrc => t, kk => zhhtdsf);
  yhvjuyz : entity work.ylzkrjqvo
    port map (slolg => ghcn, hmltful => wrdzyrovp, konmpbrc => qhewhuqkx, kk => jarqxu);
  fkbrlzm : entity work.ylzkrjqvo
    port map (slolg => nfawykchs, hmltful => owptga, konmpbrc => s, kk => logxrs);
  wwfcbkbdm : entity work.ylzkrjqvo
    port map (slolg => ko, hmltful => ikatzzdqyo, konmpbrc => gvuylblnc, kk => clwgjm);
  
  -- Single-driven assignments
  prldujes <= ghcn;
  rwy <= wrdzyrovp;
  ikatzzdqyo <= (others => 0 ns);
  owptga <= wrdzyrovp;
end ptqynk;



-- Seed after: 11919795984228887470,2338584220606314193
