-- Seed: 5809992395847948651,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity eo is
  port (nnnqeqvcc : in integer; sdbbq : linkage std_logic_vector(0 to 4));
end eo;

architecture iwrdsbjjm of eo is
  
begin
  
end iwrdsbjjm;

entity spslu is
  port (txdtnzqnd : in bit);
end spslu;

library ieee;
use ieee.std_logic_1164.all;

architecture dydxkp of spslu is
  signal yhscri : std_logic_vector(0 to 4);
  signal kwokndiz : integer;
  signal lsrhh : std_logic_vector(0 to 4);
  signal medtfi : integer;
begin
  hu : entity work.eo
    port map (nnnqeqvcc => medtfi, sdbbq => lsrhh);
  krg : entity work.eo
    port map (nnnqeqvcc => kwokndiz, sdbbq => lsrhh);
  pdhwblmw : entity work.eo
    port map (nnnqeqvcc => kwokndiz, sdbbq => yhscri);
  plcnkmjf : entity work.eo
    port map (nnnqeqvcc => kwokndiz, sdbbq => yhscri);
  
  -- Single-driven assignments
  medtfi <= 16#91#;
  kwokndiz <= 2#0_0_0#;
  
  -- Multi-driven assignments
  yhscri <= ('L', 'H', 'U', '0', '1');
  lsrhh <= lsrhh;
  yhscri <= "WWU10";
  lsrhh <= lsrhh;
end dydxkp;



-- Seed after: 18401476291356893079,5805648483995786113
