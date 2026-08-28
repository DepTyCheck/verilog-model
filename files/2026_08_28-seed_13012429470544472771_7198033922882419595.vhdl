-- Seed: 13012429470544472771,7198033922882419595

entity uyzh is
  port (d : out real; san : inout boolean_vector(0 to 3));
end uyzh;

architecture zpvvdue of uyzh is
  
begin
  -- Single-driven assignments
  d <= d;
  san <= san;
end zpvvdue;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (zlluhzjc : in std_logic; picbfp : linkage real_vector(1 to 2));
end m;

architecture llgiqwl of m is
  signal acsnyozsv : boolean_vector(0 to 3);
  signal isothjy : real;
  signal xtbdnue : boolean_vector(0 to 3);
  signal putzlsyalu : real;
  signal eusoemy : boolean_vector(0 to 3);
  signal wjzdeoei : real;
begin
  nlzehp : entity work.uyzh
    port map (d => wjzdeoei, san => eusoemy);
  wgfhzrqj : entity work.uyzh
    port map (d => putzlsyalu, san => xtbdnue);
  bvbvfz : entity work.uyzh
    port map (d => isothjy, san => acsnyozsv);
end llgiqwl;

entity spepmivcbn is
  port (lbhzm : buffer integer);
end spepmivcbn;

architecture qttpusn of spepmivcbn is
  signal chyyrtk : boolean_vector(0 to 3);
  signal otmmqrglx : real;
  signal oux : boolean_vector(0 to 3);
  signal guqs : real;
  signal x : boolean_vector(0 to 3);
  signal ootyfj : real;
  signal ejirphhpb : boolean_vector(0 to 3);
  signal zkfiswu : real;
begin
  tgsmn : entity work.uyzh
    port map (d => zkfiswu, san => ejirphhpb);
  twzxlzqj : entity work.uyzh
    port map (d => ootyfj, san => x);
  ssx : entity work.uyzh
    port map (d => guqs, san => oux);
  v : entity work.uyzh
    port map (d => otmmqrglx, san => chyyrtk);
  
  -- Single-driven assignments
  lbhzm <= 16#B638#;
end qttpusn;

library ieee;
use ieee.std_logic_1164.all;

entity cx is
  port (mzenmzob : buffer std_logic_vector(3 downto 1); gw : out std_logic_vector(2 downto 0));
end cx;

library ieee;
use ieee.std_logic_1164.all;

architecture gmnrwmf of cx is
  signal dymm : real_vector(1 to 2);
  signal kjqo : std_logic;
begin
  kwzpfxjel : entity work.m
    port map (zlluhzjc => kjqo, picbfp => dymm);
  
  -- Multi-driven assignments
  kjqo <= kjqo;
  kjqo <= '-';
  kjqo <= kjqo;
  gw <= mzenmzob;
end gmnrwmf;



-- Seed after: 18017045498111726821,7198033922882419595
