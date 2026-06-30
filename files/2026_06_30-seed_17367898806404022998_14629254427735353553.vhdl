-- Seed: 17367898806404022998,14629254427735353553

entity vkxywjqta is
  port (dtff : in integer_vector(3 to 1); woabgel : out boolean; bhxi : inout real; ax : in real_vector(4 to 2));
end vkxywjqta;

architecture rzlxyx of vkxywjqta is
  
begin
  -- Single-driven assignments
  woabgel <= TRUE;
  bhxi <= 16#2_0_F_8.6E49F#;
end rzlxyx;

library ieee;
use ieee.std_logic_1164.all;

entity dga is
  port (ldme : out std_logic_vector(3 to 0));
end dga;

architecture fc of dga is
  signal pq : real_vector(4 to 2);
  signal w : real;
  signal vmsyxqje : boolean;
  signal vwtdmcib : integer_vector(3 to 1);
begin
  meyugt : entity work.vkxywjqta
    port map (dtff => vwtdmcib, woabgel => vmsyxqje, bhxi => w, ax => pq);
  
  -- Single-driven assignments
  pq <= (others => 0.0);
  vwtdmcib <= (others => 0);
  
  -- Multi-driven assignments
  ldme <= (others => '0');
  ldme <= "";
  ldme <= "";
  ldme <= "";
end fc;

entity yhlrxxr is
  port (nyli : buffer time);
end yhlrxxr;

library ieee;
use ieee.std_logic_1164.all;

architecture vfqrx of yhlrxxr is
  signal eemj : std_logic_vector(3 to 0);
  signal tguue : real_vector(4 to 2);
  signal yloaudhil : real;
  signal pp : boolean;
  signal wpwehm : integer_vector(3 to 1);
begin
  wrofasxqj : entity work.vkxywjqta
    port map (dtff => wpwehm, woabgel => pp, bhxi => yloaudhil, ax => tguue);
  hjoifvrnek : entity work.dga
    port map (ldme => eemj);
  
  -- Single-driven assignments
  nyli <= 2 min;
  
  -- Multi-driven assignments
  eemj <= (others => '0');
  eemj <= "";
  eemj <= (others => '0');
end vfqrx;

entity jddajl is
  port (efxphrrxun : out real);
end jddajl;

architecture j of jddajl is
  signal odeht : real;
  signal snzwhyd : boolean;
  signal lzkkysd : boolean;
  signal kdpurgdn : real_vector(4 to 2);
  signal dnb : real;
  signal wzynutum : boolean;
  signal zrlxuew : integer_vector(3 to 1);
begin
  yowg : entity work.vkxywjqta
    port map (dtff => zrlxuew, woabgel => wzynutum, bhxi => dnb, ax => kdpurgdn);
  mzcllgs : entity work.vkxywjqta
    port map (dtff => zrlxuew, woabgel => lzkkysd, bhxi => efxphrrxun, ax => kdpurgdn);
  rllortkzo : entity work.vkxywjqta
    port map (dtff => zrlxuew, woabgel => snzwhyd, bhxi => odeht, ax => kdpurgdn);
  
  -- Single-driven assignments
  zrlxuew <= (others => 0);
end j;



-- Seed after: 7559818322266618701,14629254427735353553
